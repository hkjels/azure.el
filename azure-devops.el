;; -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Henrik Kjerringvåg <henrik@kjerringvag.no>
;; Version: 2022.07.15
;; URL: https://github.com/hkjels/azure-devops.el
;; Keywords: tools, azure, devops
;; Package-Requires: ((emacs "28.1") (azure "2022.07.15") (a "1.0.0") (dash "2.19.1") (s "1.12.0") (all-the-icons "5.0.0") (svg-lib "0.2.5"))

;; Copyright (C) 2023 Henrik Kjerringvåg 
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


;; Find tasks, discuss, record work etc. It's all working together with
;; org-mode, so clocking in etc works as you would expect.



(require 'a)
(require 'all-the-icons)
(require 'azure)
(require 'dash)
(require 's)
(require 'svg-lib)

(defgroup azure-devops nil
  "Azure-devops & org-mode working in symphony"
  :prefix "azure-devops-"
  :link '(url-link "https://github.com/hkjels/azure.el")
  :group 'azure
  :group 'tools)

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

(defvar azure-devops-search-mode-hook nil
  "Hook that's run when `azure-devops-search-mode` is turned on.")

(defvar azure-devops-search-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "RET") #'azure-devops-work-item)
    map)
  "Keymap used with the work-item search.")

(defvar azure-devops-work-item-menu
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used when visiting a work-item.")



;; We have an interactive buffer where you can query for work-items
;; asynchronously.


(defvar azure-devops--skipped 0
  "When querying for work-items, this is the number of work-items that
  will be skipped. Used internally for pagination.")

(defface azure-devops-item-active '((t :inherit bold))
  "Face used with active work-items."
  :group 'azure)

(defface azure-devops-item-new '((t :inherit bold))
  "Face used with new work-items."
  :group 'azure)

(defface azure-devops-item-closed '((t :inherit font-lock-type-face))
  "Face used with closed work-items."
  :group 'azure)

(defface azure-devops-item-resolved '((t :inherit font-lock-builtin-face))
  "Face used with resolved work-items."
  :group 'azure)

(defun azure-devops-face-by-state (state)
  (let ((state (downcase state)))
   (cond ((s-equals? state "new") 'azure-devops-item-new)
         ((s-equals? state "active") 'azure-devops-item-active)
         ((s-equals? state "closed") 'azure-devops-item-closed)
         ((s-equals? state "resolved") 'azure-devops-item-resolved))))

(defun azure-devops--test-output ()
  (azure-log this-command "Test output"))

(defun azure-devops--define-mouse-key (command)
  "Defines a mouse-action to be used with the head-line widgets."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-2]
                (lambda (click)
                  (interactive "e")
                  (mouse-select-window click)
                  (call-interactively command)))
    map))

(defun azure-devops--search-header-board ()
  "Tap the board-name in the header-line to change it."
  (let ((map (azure-devops--define-mouse-key 'azure-devops--test-output)))
    `(:propertize ,(truncate-string-to-width (format " Board: %s " azure--board) 100 nil 32 "…")
                  mouse-face header-line-highlight
                  help-echo "Change board"
                  keymap ,map)))

(defun azure-devops--search-header-assignee ()
  "Tap the assignee-name in the header-line to change it."
  (let ((map (azure-devops--define-mouse-key 'azure-devops--test-output)))
    `(:propertize ,(truncate-string-to-width (format " Assignee: %s " azure--assignee) 100 nil 32 "…")
                  mouse-face header-line-highlight
                  help-echo "Change assignee"
                  keymap ,map)))

(defun azure-devops--search-header-state ()
  "Tap the state-name in the header-line to change it."
  (let ((map (azure-devops--define-mouse-key 'azure-devops--test-output)))
    `(:propertize ,(truncate-string-to-width (format " State: %s " azure--state) 100 nil 32 "…")
                  mouse-face header-line-highlight
                  help-echo "Change state"
                  keymap ,map)))

(defun azure-devops--search-header-line ()
  "Header-line used with the search-buffer to enable various filtering."
  (let ((space "\t\t"))
    (setq-local
     header-line-format
     (list
      (azure-devops--search-header-board) space
      (azure-devops--search-header-assignee) space
      (azure-devops--search-header-state)))))

(defun azure-devops-search-selected-id ()
  (let ((line (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position))))
    (->> (s-collapse-whitespace line)
         (s-match "^[^0-9]+\\([0-9]+\\)")
         (cl-first)
         (s-trim)
         (string-to-number))))

(defun azure-devops--search (&optional text assignee status skip)
  "Query azure's API for work-items.

   See URL 'https://docs.microsoft.com/en-us/rest/api/azure/devops/search/work-item-search-results/fetch-work-item-search-results'
   for more information."
  (let ((url "https://almsearch.dev.azure.com/{organization}/{project}/_apis/search/workitemsearchresults")
        (top (math-min (math-max 0 azure-devops-search-results-max) 200))
        (skip (or skip 0)))
    (message "Project: %s" azure-project)
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
                     (message "Project: %s" azure-project)
                     (when (not (eq azure-devops--work-items work-items))
                       (progn
                         (setq azure-devops--work-items work-items)
                         (azure-devops--update-search-buffer))))))
                `(("searchText" . ,(or text "NOT null"))
                  ("$orderBy" . ((("field" . "system.id") 
                                  ("sortOrder" . "DESC"))))
                  ("$skip" . ,skip)
                  ("$top" . ,top)
                  ("includeFacets" . "true"))
                '(("api-version" . "7.1-preview.1")))))



;; - [ ] Make sure font-locking only spans one line at a time
;; - [ ] Color read items differently
;; - [ ] Color assigned items differently
;; - [ ] Use mode-menu in mode-line
;; - [ ] Add item-type icon (bug, user-story, etc)
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
    (hl-line-mode t)
    (buffer-disable-undo)
    (setq-local truncate-lines t
                line-move-visual t
                show-trailing-whitespace nil)
    (when azure-devops-search-show-header
      (azure-devops--search-header-line))))

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
      (save-excursion
        (setq inhibit-read-only t)
        (goto-line azure-devops--skipped)
        (beginning-of-line (if (> azure-devops--skipped 0) 1 0))
        (mapcar
         (lambda (item)
           (pcase-let
               ((`(,id ,type ,title ,assignee ,state ,tags ,_ ,created ,changed) item))
             (let* ((width (- (window-width) 30 (string-width "\t\t\t\t")))
                    (fmt (concat "%." (format "%d" width) "s"))
                    (title (truncate-string-to-width (s-collapse-whitespace title) width nil 32 "…"))
                    (face (azure-devops-face-by-state state))
                    (item-type (cond ((s-equals? type "Bug") (all-the-icons-octicon "bug" :face '(:foreground "indian red")))
                                     ((s-equals? type "User Story") (all-the-icons-octicon "book" :face face))
                                     ((s-equals? type "Feature") (all-the-icons-octicon "rocket" :face face))
                                     ((s-equals? type "Task") (all-the-icons-octicon "checklist" :face face))
                                     (t ""))))
               (azure-log this-command "Assignee: %S" assignee)
               (insert (propertize (format "%s\t%-8s" id state) 'font-lock-face face))
               (insert (propertize (format "\t%s " item-type) 'help-echo (format " %s " type)))
               (insert (propertize (format "%s\t" title) 'font-lock-face face))
               (when (s-present? tags)
                 (--map (insert-image (svg-lib-tag (upcase it)
                                                   (svg-lib-style-compute-default)
                                                   :margin 1 :padding 1 :height 0.6))
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
      (azure-devops--search query assignee state skip))))

(add-hook 'post-command-hook 'azure-devops-search-skip)

;;;###autoload
(define-derived-mode azure-devops-search-mode special-mode "azure-devops-search"
  "Major-mode to search for work-items.

\\{azure-devops-search-mode-map}"
  :group 'azure
  :after-hook azure-devops-search-mode-hook
  :syntax-table nil
  :abbrev-table nil
  ;; (add-hook 'window-configuration-change-hook 'azure-devops--update-search-buffer nil 'local)
  )

;;;###autoload
(defun azure-devops-search ()
  "Opens a dedicated search-buffer for work-items in azure devops."
  (interactive)
  (unless (azure--valid-p)
    (user-error "You need to run `azure-init` first!"))
  (azure-devops--setup-search-buffer)
  (azure-devops-search-mode)
  (azure--set-user)
  (azure-devops--search))

;; (defun azure-devops-search (&optional query board assignee state)
;;   "Major-mode to search for work-items.
;; \\{azure-devops-search-mode-map}"
;;   (interactive)
;;   (if (azure--valid-p)
;;       (progn
;;         (azure--set-user)
;;         (azure-devops--setup-search-buffer)
;;         (azure-devops--search query assignee state)
;;         (add-hook 'window-configuration-change-hook 'azure-devops--update-search-buffer nil 'local)
;;         (run-mode-hooks 'azure-devops-search-mode-hook))
;;     (user-error "You need to run `azure-init` first!")))

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

(defun azure-devops--item-buffer (title assignee)
  "Returns the compiled name of a work-item buffer."
  (s-replace-all `(("%O" . ,azure-organization)
                   ("%P" . ,azure-project)
                   ("%T" . ,azure-team)
                   ("%t" . ,title)
                   ("%a" . ,assignee))
                 azure-devops-item-buffer))



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
  ""
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
  ""
  (let* ((fields (cdr (assoc 'fields work-item)))
         (state (s-trim (cdr (assoc (cdr (assoc 'System.State fields)) azure-devops-mapping-states))))
         (title (cdr (assoc 'System.Title fields))))
    (azure-log this-command "Adding title: %s" title)
    (format "* %s%s\n" (if (s-blank? state) "" (concat state " ")) title)))

(defun azure-devops--work-item-type (work-item)
  (let* ((fields (cd (assoc 'fields work-item)))
         (item-type (downcase (cdr (assoc 'System.WorkItemType fields)))))
    (cond ((s-equals? item-type "bug") (propertize (all-the-icons-octicon "bug")
                                                   'face `(:family ,(all-the-icons-octicon-family) :foreground "red")
                                                   'display '(raise 0.1)
                                                   'help-echo `item-type))
          ((s-equals? item-type "user story") (propertize (all-the-icons-octicon "book")
                                                          'face `(:family ,(all-the-icons-octicon-family) :foreground "light-blue")
                                                          'display '(raise 0.1)
                                                          'help-echo `item-type))
          (t ""))))

(defun azure-devops--work-item-content (work-item)
  ""
  (let* ((fields (cdr (assoc 'fields work-item)))
         (description (cdr (assoc 'System.Description fields)))
         (repro (cdr (assoc 'Microsoft.VSTS.TCM.ReproSteps fields))))
    (azure-log this-command "All fields: %S" fields)
    (azure-log this-command "Adding description: %s" description)
    (when description (azure--html-to-org description))
    (when repro (azure--html-to-org repro))))

(defun azure-devops--work-item-comments (comments)
  ""
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

(async-defun azure-devops--update-work-item-buffer (id)  
  "Update the work-item buffer.

   We retrieve all the information needed first and if that succeeds,
   we replace everything in our local copy of the issue with what we
   retrieved. Only clocking and personal notes are persisted from the
   local copy."
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
      (insert (s-concat (azure-devops--work-item-type) "\n"))
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
      (rename-visited-file (format "%d-%s" id filename))
      (message "Work item was updated!"))))

(defun azure-devops-work-item (id)
  "Show the work-item with ID in a buffer of it's own.

  See URL 'https://docs.microsoft.com/en-us/rest/api/azure/devops/wit/work-items/get-work-item'
  for more information."
  (interactive (list (azure-devops-search-selected-id)))
  (azure-log this-command "Show work-item with id: %S" id)
  (funcall 'azure-devops--update-work-item-buffer id))

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
