;;; azure-devops.el --- Azure-devops & org-mode working in symphony -*- coding: utf-8; lexical-binding: t; -*-
;; Author: Henrik Kjerringvåg <henrik@kjerringvag.no>
;; Version: 2022.07.15
;; URL: https://github.com/hkjels/azure-devops.el
;; Keywords: tools, azure, devops
;; Package-Requires: ((emacs "28.1") (azure "2022.07.15") (a "1.0.0") (dash "2.19.1") (s "1.12.0") (all-the-icons "5.0.0") (svg-lib "0.2.5"))

;; Copyright (C) 2024 Henrik Kjerringvåg
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Find tasks, discuss, record work etc. It's all working together with
;; org-mode, so clocking in etc works as you would expect.



;;; Code:

(require 'a)
(require 'all-the-icons)
(require 'azure)
(require 'dash)
(require 's)
(require 'svg-lib)
(require 'json)

(defgroup azure-devops nil
  "Azure-devops & org-mode working in symphony"
  :prefix "azure-devops-"
  :link '(url-link "https://github.com/hkjels/azure.el")
  :group 'azure
  :group 'tools)

;; Required information


(defcustom azure-devops-search-latest-atop t
  "Wether to order the search-results with the latest entry first or last."
  :group 'azure
  :type 'boolean)

(defcustom azure-devops-discussion-latest-atop nil
  "Wether to order the thread of discussion with the latest comment first or last."
  :group 'azure
  :type 'boolean)

(defcustom azure-devops-search-show-header t
  "Wether to show or hide the header in the search-results buffer."
  :group 'azure
  :type 'boolean)

(defcustom azure-devops-search-results-max 200
  "Maximum number of results returned when searching for work-items.
   Note that <b>200</b> is the maximum supported by Azure's API."
  :group 'azure
  :type 'natnum)

(defcustom azure-devops-search-buffer "*azure searching %P*"
  "Name of the buffer used to display search results.

   Note that you can add certain properties via formatting specifiers:
       %O - Organization
       %P - Project
       %T - Team"
  :group 'azure
  :type 'string)

(defcustom azure-devops-item-buffer "*azure - %t*"
  "Name of the buffer used to display a work-item.

   Note that you can add certain properties via formatting specifiers:
       %O - Organization
       %P - Project
       %T - Team
       %t - Item title
       %a - Item assignee"
  :group 'azure
  :type 'string)

(defvar azure-devops-mapping-states
  '(("New" . "")
    ("To Do" . "")
    ("Active" . "TODO")
    ("Doing" . "TODO")
    ("Done" . "DONE")
    ("Resolved" . "DONE")
    ("Closed" . "DONE")
    ("Removed" . "REMOVED"))
  "Align work-item states with TODO-states of org-mode.")

;; Menus and bindings


(defvar azure-devops-search-mode-hook nil
  "Hook that's run when `azure-devops-search-mode` is turned on.")

(defvar azure-devops-search-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "RET") #'azure-devops-work-item)
    (define-key map (kbd "<double-mouse-1>") #'azure-devops-work-item)
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map (kbd "SPC") 'azure-devops-search)
      (define-key prefix-map (kbd "k") 'azure-devops--keywords)
      (define-key prefix-map (kbd "t") (lambda () (interactive) (azure-devops--menu "type")))
      (define-key prefix-map (kbd "a") (lambda () (interactive) (azure-devops--menu "assignees")))
      (define-key prefix-map (kbd "s") (lambda () (interactive) (azure-devops--menu "state")))
      (define-key prefix-map (kbd "r") (lambda () (interactive) (azure-devops--menu "area")))
      (define-key prefix-map (kbd "i") (lambda () (interactive) (azure-devops--menu "iteration")))
      (define-key prefix-map (kbd "g") (lambda () (interactive) (azure-devops--menu "tags")))
      (define-key map (kbd azure-prefix-key) prefix-map))
    map)
  "Keymap used with the work-item search.")

(defvar azure-devops-work-item-menu
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used when visiting a work-item.")

;; Search

;; We have an interactive buffer where you can query for work-items
;; asynchronously.


(defvar azure-devops--skipped 0
  "When querying for work-items, this is the number of work-items that
will be skipped. Used internally for pagination.
Will be increments of `azure-devops-search-results-max`.")

;; Faces


(defface azure-devops-item-mine '((t :inherit font-lock-builtin-face))
  "Face used with work-items that are assigned to you."
  :group 'azure)

(defface azure-devops-item-active '((t :inherit bold))
  "Face used with active work-items."
  :group 'azure)

(defface azure-devops-item-new '((t :inherit bold))
  "Face used with new work-items."
  :group 'azure)

(defface azure-devops-item-closed '((t :inherit font-lock-comment-face))
  "Face used with closed work-items."
  :group 'azure)

(defface azure-devops-item-resolved '((t :inherit font-lock-comment-face))
  "Face used with resolved work-items."
  :group 'azure)

(defvar azure-devops-item-tags
  '(:background "#f3f8ff"
    :foreground "#2751e5"
    :stroke-color "#b9ceff"
    :font-size 8
    :font-weight 600
    :margin 1
    :stroke 1
    :radius 8)
  "Default style settings for tags.")

(defun azure-devops-face-by-state (state)
  (let ((state (downcase state)))
   (cond ((s-equals? state "new") 'azure-devops-item-new)
         ((s-equals? state "active") 'azure-devops-item-active)
         ((s-equals? state "closed") 'azure-devops-item-closed)
         ((s-equals? state "resolved") 'azure-devops-item-resolved))))

;; TODO Header [0/3]
;; - [ ] Improve alignment
;; - [ ] Add filtering functions
;; - [ ] Update upon changing filters

(defun azure-devops--build-filter-object ()
  "Builds a filter object based on the current filter settings."
  (let ((filter-object (make-hash-table :test 'equal)))
    (puthash "System.TeamProject" azure-project filter-object)
    (when azure--keywords
      (puthash "System.Keywords" azure--keywords filter-object))
    (when azure--types
      (puthash "System.WorkItemType" azure--types filter-object))
    (when azure--assignees
      (puthash "System.AssignedTo" (list azure--assignees) filter-object))
    (when azure--state
      (puthash "System.State" azure--state filter-object))
    (when azure--area
      (puthash "System.AreaPath" (list azure--area) filter-object))
    (when azure--iteration
      (puthash "System.IterationPath" (list azure--iteration) filter-object))
    (when azure--tags
      (puthash "System.Tags" (list azure--tags) filter-object))
    filter-object))

(defun azure-devops--clear-all-filters ()
  (interactive)
  (setq azure--types nil
	azure--keywords nil
        azure--assignees nil
        azure--state nil
        azure--area nil
        azure--iteration nil
        azure--tags nil)
  (azure-devops--search))

(defun azure-devops--test-output ()
  (interactive)
  (azure-log this-command "Test output"))

(defun azure-devops--define-mouse-key (command &optional args)
  "General mouse handler that takes a COMMAND and optionally ARGS."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
                (lambda (click)
                  (interactive "e")
                  (mouse-select-window click)
                  (apply command args)))
    map))

(defun azure-devops--keywords ()
  (interactive)
  (let ((keywords (read-string "Filter by keyword: ")))
    (setq azure--keywords keywords)
    (azure-devops--search)))

(defun azure-devops--parse-work-item-types (data)
  "Parse the work item types DATA from Azure DevOps API."
  (mapcar (lambda (item) (cdr (assoc 'name item))) data))

(defun azure-devops--fetch-work-item-types (callback)
  "Fetch available work item types.

See URL: https://learn.microsoft.com/en-us/rest/api/azure/devops/wit/work-item-types/list?view=azure-devops-rest-7.1
for more information."
  (azure-get "https://dev.azure.com/{organization}/{project}/_apis/wit/workitemtypes"
	     (cl-function
	      (lambda (&key data &allow-other-keys)
                (let ((types (azure-devops--parse-work-item-types (cdr (assoc 'value data))))
		      (this-command "azure-devops--fetch-work-item-types"))
		  (funcall callback types)
		  (azure-log this-command "%S" types))))
	     '(("api-version" . "7.1-preview.2"))))

(defun azure-devops--get-available-types ()
  "Get available work item types and allow the user to select multiple."
  (interactive)
  (unless azure--available-types
    (azure-devops--fetch-work-item-types
     (lambda (types)
       (setq azure--available-types types))))
  (setq azure--types (completing-read-multiple "Select work item types: " azure--available-types))
  (azure-devops--search))

(defun azure-devops--get-available-team-members ()
  "Get available team-members and allow the user to select multiple."
  (interactive)
  (unless azure--available-team-members
    (azure--team-members
     (lambda (members)
       (setq azure--available-team-members members))))
  (let* ((members (mapcar 'car  azure--available-team-members))
	 (selected-members (completing-read-multiple "Select assignees: " members)))
    (setq azure--assignees selected-members))
  (azure-devops--search))

(defun azure-devops--menu (type)
  "Open a dynamic menu based on the TYPE of the header."
  (interactive)
  (let ((items (cond
                ((string= type "type") (azure-devops--get-available-types))
                ((string= type "assignees") (azure-devops--get-available-team-members))
                ;; ((string= type "state") (azure-devops--get-available-states))
                ;; ((string= type "area") (azure-devops--get-available-areas))
                ;; ((string= type "iteration") (azure-devops--get-available-iterations))
                ;; ((string= type "tags") (azure-devops--get-available-tags))
                (t (error "Unknown type")))))))

(defun azure-devops--search-header-types ()
  "Tap the types-name in the header-line to change it."
  (let ((map (azure-devops--define-mouse-key 'azure-devops--menu '("type"))))
    `(:propertize ,(truncate-string-to-width (s-join ", " (or azure--types '("Types"))) 15 nil 32 "…")
                  mouse-face header-line-highlight
                  help-echo "Filter by type"
                  keymap ,map)))

(defun azure-devops--search-header-assignee ()
  "Tap the assignee-name in the header-line to change it."
  (let ((map (azure-devops--define-mouse-key 'azure-devops--menu '("assignees"))))
    `(:propertize ,(truncate-string-to-width (s-join ", " (or azure--assignees '("Assigned to"))) 15 nil 32 "…")
                  mouse-face header-line-highlight
                  help-echo "Filter by assignee"
                  keymap ,map)))

(defun azure-devops--search-header-state ()
  "Tap the state-name in the header-line to change it."
  (let ((map (azure-devops--define-mouse-key 'azure-devops--menu '("state"))))
    `(:propertize ,(truncate-string-to-width (s-join ", " (or azure--state '("State"))) 15 nil 32 "…")
                  mouse-face header-line-highlight
                  help-echo "Filter by state"
                  keymap ,map)))

(defun azure-devops--search-header-area ()
  "Tap the area-name in the header-line to change it."
  (let ((map (azure-devops--define-mouse-key 'azure-devops--menu '("area"))))
    `(:propertize ,(truncate-string-to-width (s-join ", " (or azure--area '("Area"))) 15 nil 32 "…")
                  mouse-face header-line-highlight
                  help-echo "Filter by area"
                  keymap ,map)))

(defun azure-devops--search-header-iteration ()
  "Tap the iteration-name in the header-line to change it."
  (let ((map (azure-devops--define-mouse-key 'azure-devops--menu '("iteration"))))
    `(:propertize ,(truncate-string-to-width (s-join ", " (or azure--iteration '("Iteration"))) 15 nil 32 "…")
                  mouse-face header-line-highlight
                  help-echo "Filter by iteration"
                  keymap ,map)))

(defun azure-devops--search-header-tags ()
  "Tap the tags-name in the header-line to change it."
  (let ((map (azure-devops--define-mouse-key 'azure-devops--menu '("tags"))))
    `(:propertize ,(truncate-string-to-width (s-join ", " (or azure--tags '("Tags"))) 15 nil 32 "…")
                  mouse-face header-line-highlight
                  help-echo "Filter by tags"
                  keymap ,map)))

(defun azure-devops--search-header-clear ()
  "Button to clear all filters."
  (let ((map (azure-devops--define-mouse-key 'azure-devops--clear-all-filters)))
    `(:propertize ,(all-the-icons-faicon "times-circle")
		  mouse-face header-line-highlight
		  help-echo "Clear all filters"
		  keymap ,map)))

(defun azure-devops--search-header-line ()
  "Header-line used with the search-buffer to enable various filtering."
  (let ((space "  "))
    (setq-local header-line-format
		(list
		 (azure-devops--search-header-types) space
		 (azure-devops--search-header-assignee) space
		 (azure-devops--search-header-state) space
		 (azure-devops--search-header-area) space
		 (azure-devops--search-header-iteration) space
		 (azure-devops--search-header-tags) space
		 (azure-devops--search-header-clear)))))

;; [[https://docs.microsoft.com/en-us/rest/api/azure/devops/search/work-item-search-results/fetch-work-item-search-results][Work Item Search Results]]


(defun azure-devops-search-selected-id ()
  (let ((buf (get-buffer (azure-devops--buffer-name azure-devops-search-buffer))))
    (with-current-buffer buf
      (let ((line (buffer-substring-no-properties
		   (line-beginning-position)
		   (line-end-position))))
	(->> (s-collapse-whitespace line)
             (s-match "^[^0-9]*\\([0-9]+\\)")
             (cl-first)
             (s-trim)
             (string-to-number))))))

(defun azure-devops--search (&optional text skip)
  "Query azure's API for work-items.

   See URL 'https://docs.microsoft.com/en-us/rest/api/azure/devops/search/work-item-search-results/fetch-work-item-search-results'
   for more information."
  (let ((url "https://almsearch.dev.azure.com/{organization}/{project}/_apis/search/workitemsearchresults")
        (top (math-min (math-max 0 azure-devops-search-results-max) 200))
        (skip (or skip 0))
	(filters (azure-devops--build-filter-object)))
    (azure-post url
                (cl-function
                 (lambda (&key data &allow-other-keys)
                   (let* ((work-items (mapcar
                                       (lambda (item)
                                         (mapcar 'cdr (cdr (assoc 'fields item))))
                                       (cdr (assoc 'results data))))
                          (work-items (sort work-items
                                            (lambda (a b)
                                              (not (s-less? (nth 7 a) (nth 7 b))))))
                          (work-items (if azure-devops-search-latest-atop work-items (reverse work-items))))
                     (when (not (eq azure-devops--work-items work-items))
                       (progn
                         (setq azure-devops--work-items work-items)
                         (azure-devops--update-search-buffer))))))
                `(("searchText" . ,(if (and text (not (string= "" text))) text "NOT null"))
                  ("$orderBy" . ((("field" . "system.id") 
                                  ("sortOrder" . "DESC"))))
                  ("$skip" . ,skip)
                  ("$top" . ,top)
		  ("filters" . ,(json-encode filters))
                  ("includeFacets" . "true"))
                '(("api-version" . "7.1-preview.1")))))

;; TODO Results buffer [3/8]

;; - [X] Make sure font-locking only spans one line at a time
;; - [ ] Color read items differently
;; - [ ] Color assigned items differently
;; - [ ] Use mode-menu in mode-line
;; - [X] Add item-type icon (bug, user-story, etc)
;; - [X] Replace tags using svg-lib
;; - [ ] Apply a fringe indicator if an item was updated after viewing it
;; - [ ] Use transient to enable more powerful search, filtering, creation, etc

;; When doing a search (~azure-devops-search~), we validate the configuration
;; first via ~azure-init~.  The rest is handled interactively from inside
;; the search results buffer.

(defun azure-devops--buffer-name (buffer-name)
  "Get the formatted/compiled BUFFER-NAME."
  (s-replace-all `(("%O" . ,azure-organization)
                   ("%P" . ,azure-project)
                   ("%T" . ,azure-team))
                 buffer-name))

(defvar azure-devops--work-items '()
  "Work-items currently being listed.")

(defun azure-devops-search-selected ()
  "Return the currently selected work-item from the search results list."
  (let* ((item-num (- (line-number-at-pos (point)) 1))
         (work-item (nth item-num azure-devops--work-items)))
    work-item))

(defun azure-devops--setup-search-buffer ()
  "Setup of the buffer that holds our search-results.
\\{azure-devops-search-mode-map}"
  (let ((buf (get-buffer-create (azure-devops--buffer-name azure-devops-search-buffer))))
    (switch-to-buffer buf)
    (kill-all-local-variables)
    (hack-dir-local-variables)
    (hack-local-variables-apply)
    (use-local-map azure-devops-search-mode-map)
    (read-only-mode t)
    (buffer-disable-undo)
    (setq-local truncate-lines t
                line-move-visual t
                show-trailing-whitespace nil)))

(defun azure-devops--update-search-buffer ()
  "Update the search-buffer with WORK-ITEMS."
  (let ((buf (get-buffer (azure-devops--buffer-name azure-devops-search-buffer)))
	(map (azure-devops--define-mouse-key
	      (lambda ()
		(let* ((work-item (azure-devops-search-selected))
		       (id (cdr (assoc 'id work-item))))
		  (azure-devops-work-item id)))))
	(this-command "azure-devops--update-search-buffer"))
    (azure-log this-command "Work items: %S" azure-devops--work-items)
    (with-current-buffer buf
      (hl-line-mode t)
      (when azure-devops-search-show-header
	(azure-devops--search-header-line))
      (save-excursion
	(setq inhibit-read-only t)
	(when (equal 0 azure-devops--skipped)
	  (delete-region (point-min) (point-max)))
	(goto-line azure-devops--skipped)
	(beginning-of-line (if (> azure-devops--skipped 0) 1 0))
	(mapcar
	 (lambda (item)
	   (pcase-let
	       ((`(,id ,type ,title ,assignee ,state ,tags ,_ ,created ,changed) item))
	     (let* ((width (max 50 (- (window-width) 60 (string-width "\t\t\t\t"))))
		    (fmt (concat "%." (format "%d" width) "s"))
		    (title (truncate-string-to-width (s-collapse-whitespace title) width nil 32 "…"))
		    (face (if (string= assignee azure--user) 'azure-devops-item-mine (azure-devops-face-by-state state)))
		    (item-type (cond ((s-equals? type "Bug") (all-the-icons-material "bug_report" :face `((t :inherit ,face :weight normal))))
				     ((s-equals? type "User Story") (all-the-icons-octicon "book" :face `((t :inherit ,face :weight normal))))
				     ((s-equals? type "Feature") (all-the-icons-octicon "rocket" :face `((t :inherit ,face :weight normal))))
				     ((s-equals? type "Task") (all-the-icons-octicon "checklist" :face `((t :inherit ,face :weight normal))))
				     (t ""))))
	       (insert (propertize (format "%-10s\t%-8s" id state) 'font-lock-face face))
	       (insert (propertize (format "\t%s " item-type) 'help-echo (format " %s " type)))
	       (insert (propertize (format "%s\t" title) 'font-lock-face face))
	       (when (s-present? tags)
		 (--map (insert-image (apply 'svg-lib-tag it
					     '(svg-lib-style-compute-default)
					     azure-devops-item-tags))
			(s-split ";" tags)))
	       (insert (propertize "\n" 'font-lock-face face)))))
	 azure-devops--work-items)
	(setq inhibit-read-only nil)))))

(defun azure-devops-search-skip ()
  (when (and (s-starts-with? (buffer-name (current-buffer)) "*azure search")
             (= (point) (point-max)))
    (let ((skip (+ azure-devops--skipped azure-devops-search-results-max)))
      (azure-log this-command "Reached the end of the search-buffer")
      (setq azure-devops--skipped skip)
      (azure-devops--search query skip))))

(add-hook 'post-command-hook 'azure-devops-search-skip)

;;;###autoload
(define-derived-mode azure-devops-search-mode special-mode "azure-devops-search"
  "Major-mode to search for work-items.

\\{azure-devops-search-mode-map}"
  :group 'azure
  :after-hook azure-devops-search-mode-hook
  :syntax-table nil
  :abbrev-table nil
  (add-hook 'window-configuration-change-hook 'azure-devops--update-search-buffer nil 'local))

;;;###autoload
(defun azure-devops-search (query)
  "Opens a dedicated search-buffer for work-items in azure devops."
  (interactive (list (read-string "Enter search query (leave empty to return all work items): ")))
  (unless (azure--valid-p)
    (user-error "You need to run `azure-init` first!"))
  (azure-devops--setup-search-buffer)
  (azure-devops-search-mode)
  (azure--set-user)
  (azure-devops--search query)
  (run-mode-hooks 'azure-devops-search-mode-hook))

;; Comments


(defun azure-devops--comments (id)
  ""
  (promise-new
   (lambda (resolve _reject)
     (let ((url (format "https://dev.azure.com/{organization}/{project}/_apis/wit/workItems/%d/comments" id)))
       (azure-get url
                  (cl-function
                   (lambda (&key data &allow-other-keys)
                     (let ((comments (cdr (assoc 'comments data)))
                           (this-command "azure-devops--comments"))
                       (funcall resolve comments)
                       (azure-log this-command "%S" comments))))
                  '(("api-version" . "7.1-preview.3")))))))

;; Work items


(defun azure-devops--item-buffer (title assignee)
  "Returns the compiled name of a work-item buffer."
  (s-replace-all `(("%O" . ,azure-organization)
                   ("%P" . ,azure-project)
                   ("%T" . ,azure-team)
                   ("%t" . ,title)
                   ("%a" . ,assignee))
                 azure-devops-item-buffer))

;; TODO Work Item Buffer [0/3]

;; - [ ] Enable editing
;; - [ ] Make sure links can be followed within azure-devops.el scope
;; - [ ] Use view-mode until changes are synchronized


(defun azure-devops-work-item-file (id)
  "Expanded file-path of the work-item prefixed with ID."
  (car
   (file-expand-wildcards
    (expand-file-name (format "%d-*.org" id) azure-cache-directory))))

(defun azure-devops--create-or-flush-work-item-buffer (id)
  "Open the file associated with the work-item with ID and update it's content.

   If a file does not exist, a new one will be created."
  (promise-new
   (lambda (resolve _reject)
     (let ((logbook-p nil)
           (check-point (point-min))
           (this-command "azure-devops--create-or-flush-work-item-buffer"))
       (when (eq (azure-devops-work-item-file id) nil)
         (let* ((new-name (format "%d-Not-yet-updated.org" id))
                (buf (generate-new-buffer new-name)))
           (azure-log this-command "Creating a new work-item file named: %S" new-name)
           (save-excursion
             (with-current-buffer buf
               (org-mode)
               (insert "\n\n* Personal Notes\n")
               (write-file (expand-file-name new-name azure-cache-directory))))))
       (azure-log this-command "Open file on disk, regardless if it’s new or old")
       (find-file (azure-devops-work-item-file id))
       (with-current-buffer (current-buffer)
         (goto-char check-point)
         (save-excursion
           (while (re-search-forward ":logbook:" nil 'noerror)
             (azure-log this-command "Logbook entry exists, delete everything before the entry")
             (delete-region (point) (match-beginning 0))
             (setq logbook-p t)))
         (when logbook-p
           (azure-log this-command "Move pointer to after the logbook entry")
           (while (re-search-forward ":logbook:.+:end:" nil)
             (setq-local check-point (match-end 0))
             (goto-char check-point)))
         (save-excursion
          (while (re-search-forward "* Personal Notes" nil 'noerror)
            (when (length> (buffer-substring-no-properties check-point (- (match-beginning 0) 1)) 1)
              (azure-log this-command "Delete everything from the pointer (line %d) to the personal notes section (line %d)"
                         (line-number-at-pos check-point)
                         (line-number-at-pos (- (match-beginning 0) 1)))
              (delete-region check-point (- (match-beginning 0) 1)))))
         (azure-log this-command "Return the work-item buffer: %S" (buffer-name (current-buffer)))
         (funcall resolve (buffer-name (current-buffer))))))))

(defun azure-devops--work-item-properties (work-item)
  "Creates a properties drawer for essential WORK-ITEM information."
  (let* ((fields (cdr (assoc 'fields work-item)))
         (id (cdr (assoc 'id work-item)))
         (rev (cdr (assoc 'rev work-item)))
         (state (cdr (assoc 'System.State fields)))
         (created (cdr (assoc 'System.CreatedDate fields)))
         (by (cdr (assoc 'displayName
                         (cdr (assoc 'System.CreatedBy fields))))))
    (azure-log this-command "Adding properties for: %d" id)
    (format ":properties:\n:id: %d\n:rev: %d\n:state: %s\n:created: %s\n:created-by: %s\n:end:\n" id rev state created by)))

(defun azure-devops--work-item-title (work-item)
  "Formats the WORK-ITEM title into an `org-mode` heading."
  (let* ((fields (cdr (assoc 'fields work-item)))
         (state (s-trim (cdr (assoc (cdr (assoc 'System.State fields)) azure-devops-mapping-states))))
         (title (cdr (assoc 'System.Title fields))))
    (azure-log this-command "Adding title: %s" title)
    (format "* %s%s\n" (if (s-blank? state) "" (concat state " ")) title)))

(defun azure-devops--work-item-type (work-item)
  "Return an icon that represents the type of the WORK-ITEM."
  (let* ((fields (cdr (assoc 'fields work-item)))
         (id (cdr (assoc 'id work-item)))
         (item-type (downcase (cdr (assoc 'System.WorkItemType fields)))))
    (azure-log this-command "Adding work-item type for: %d" id)
    (cond ((s-equals? item-type "bug") (propertize (all-the-icons-material "bug_report")
                                                   'help-echo `item-type))
          ((s-equals? item-type "user story") (propertize (all-the-icons-octicon "book")
                                                          'help-echo `item-type))
          (t ""))))

(defun azure-devops--work-item-content (work-item)
  "Return the body (description, repro) of a WORK-ITEM."
  (let* ((fields (cdr (assoc 'fields work-item)))
         (description (cdr (assoc 'System.Description fields)))
         (repro (cdr (assoc 'Microsoft.VSTS.TCM.ReproSteps fields))))
    (azure-log this-command "All fields: %S" fields)
    (azure-log this-command "Adding description: %s" description)
    (when description (azure--html-to-org description))
    (when repro (azure--html-to-org repro))))

(defun azure-devops--work-item-comments (comments)
  "Format COMMENTS into a discussions section."
  (let ((comments (if azure-devops-discussion-latest-atop comments (reverse comments)))
        (template (s-join "\n" [":properties:"
                                ":id: %d"
                                ":created: %s"
                                ":created-by: %s"
                                ":end:"
                                "%s"
                                ""])))
    (azure-log this-command "Discussion (%d): %S" (length comments) comments)
    (format "\n\n* Discussion (%d)\n\n%s" (length comments) 
            (s-join "\n" (mapcar
                          (lambda (comment)
                            (let ((id (cdr (assoc 'id comment)))
                                  (text (s-trim (azure--html-to-org (cdr (assoc 'text comment)))))
                                  (by (cdr (assoc 'displayName (cdr (assoc 'createdBy comment)))))
                                  (created (cdr (assoc 'createdDate comment))))
                              (format template id created by text)))
                          comments)))))

;; We retrieve all the information needed first and if that succeeds,
;; we replace everything in our local copy of the issue with what we
;; retrieved. Only clocking and personal notes are persisted from the
;; local copy.
(async-defun azure-devops--update-work-item-buffer (id)  
  "Update the work-item buffer for the work-item with ID."
  (let* ((work-item (await (azure-devops--work-item-get id)))
         (comments (await (azure-devops--comments id)))
         (buf (await (azure-devops--create-or-flush-work-item-buffer id)))
         (fields (cdr (assoc 'fields work-item)))
         (filename (format "%s.org" (s-dashed-words (cdr (assoc 'System.Title fields)))))
         (logbook-p nil)
         (this-command "azure-devops--update-work-item-buffer"))
    (with-current-buffer buf
      (goto-char (point-min))
      (insert (azure-devops--work-item-properties work-item))
      (insert (azure-devops--work-item-type work-item) "\n")
      (insert (azure-devops--work-item-title work-item))
      (save-excursion
        (while (re-search-forward ":logbook:" nil 'noerror)
          (azure-log this-command "Logbook entry was found!")
          (setq logbook-p t)))
      (when logbook-p
        (while (re-search-forward ":end:" nil)
          (azure-log this-command "Logbook entry was closed!")
          (goto-char (match-end 0))))
      (insert (azure-devops--work-item-content work-item))
      (insert (azure-devops--work-item-comments comments))
      (save-buffer)
      (azure-log this-command "Rename file: %s -> %s" (format "%d-Not-yet-updated" id) (format "%d-%s" id filename))
      (rename-visited-file (format "%d-%s" id filename)))))

(defun azure-devops-work-item (id)
  "Show the work-item with ID in a buffer of it's own.

  See URL 'https://docs.microsoft.com/en-us/rest/api/azure/devops/wit/work-items/get-work-item'
  for more information."
  (interactive (list (azure-devops-search-selected-id)))
  (azure-log this-command "Show work-item with id: %S" id)
  (funcall 'azure-devops--update-work-item-buffer id))

;; [[https://docs.microsoft.com/en-us/rest/api/azure/devops/wit/work-items/create][Create]]


(defun azure-devops-work-item-create (item-type title)
  "Create a new work-item by specifying ITEM-TYPE and TITLE.

   See URL 'https://docs.microsoft.com/en-us/rest/api/azure/devops/wit/work-items/create'
   for more information."
  (interactive (list (completing-read "Item type: " '("Epic" "Issue" "Task"))
                     (read-from-minibuffer "Item title: ")))
  (let ((url (concat "https://dev.azure.com/{organization}/{project}/_apis/wit/workitems/$" item-type))
        (title (format "%s" title)))
    (azure-post url
                (cl-function
                 (lambda (&key data &allow-other-keys)
                   (azure-devops-work-item (cdr (assoc 'id data)))))
                `((("op" . "add")
                   ("path" . "/fields/System.title")
                   ("from" . nil)
                   ("value" . ,title)))
                '(("api-version" . "7.1-preview.3"))
                '(("Content-Type" . "application/json-patch+json")))))

;; [[https://docs.microsoft.com/en-us/rest/api/azure/devops/wit/work-items/get-work-item][Get Work Item]]


(defun azure-devops--work-item-get (id)
  "Get all the relevant information about a work-item by it's ID.

  See URL 'https://docs.microsoft.com/en-us/rest/api/azure/devops/wit/work-items/get-work-item'
  for more information."
  (promise-new
   (lambda (resolve _reject)
     (azure-get (format "https://dev.azure.com/{organization}/{project}/_apis/wit/workitems/%d" id)
                (cl-function
                 (lambda (&key data &allow-other-keys)
                   (let ((this-command "azure-devops--work-item-get"))
                    (progn (azure-log this-command "Work item: %S" data)
                           (funcall resolve data)))))
                '(("$expand" . "All")
                  ("api-version" . "7.1-preview.3"))))))

(provide 'azure-devops)
;;; azure-devops.el ends here
