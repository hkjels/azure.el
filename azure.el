;; -*- coding: utf-8; lexical-binding: t; -*-
;; azure.el --- Interact with Azure from the comfort of your favorite editor

;; Author: Henrik Kjerringvåg <henrik@kjerringvag.no>
;; Version: 2022.07.15
;; URL: https://github.com/hkjels/azure.el
;; Keywords: tools, azure
;; Package-Requires: ((emacs "26.2") (async-await "1.1") (request "0.3.3") (s "1.12.0"))

;; Copyright (C) 2022  Henrik Kjerringvåg
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
;;; The purpose of this package from the onset, is to cover my day to day
;; needs when dealing with Azure. Basically; finding tasks, changing
;; their status, comment, create new tasks and record work. 



;; Code:

(require 'async-await)
(require 'easymenu)
(require 'plstore)
(require 'request)
(require 'json)
(require 's)

(defgroup azure nil
  "Interact with Azure from the comfort of your favorite editor."
  :prefix "azure-"
  :link '(url-link "https://github.com/hkjels/azure.el")
  :group 'azure
  :group 'tools)

;; Token

;; To access the Azure API, you'll need to provide a personal
;; access-token. You'll be prompted for the token upon using ~azure.el~ the
;; first time. The token is stored in a ~plstore~, so you'll need to have
;; GPG set up properly for encryption. Note that even though the
;; token-file is encrypted, I would highly recommend not saving this file
;; in a place accessible to others. Hence, if you share your
;; emacs-config, you'll likely want to customize ~azure-access-token-file~.

(defcustom azure-access-token-file (concat user-emacs-directory "azure.plstore")
  "File for storing and retrieving access-token."
  :group 'azure
  :type 'file)



;; This is where we store and access the said token. 

(defun azure-access-token ()
  "Returns an access-token already stored on disk or asks for token if not.
   In either case, the access-token will be returned base64-encoded."
  (base64-encode-string
   (concat ":"
           (let ((azure-access-token-file (expand-file-name azure-access-token-file)))
             (if (file-exists-p azure-access-token-file)
                 (let* ((store (plstore-open azure-access-token-file))
                        (plist (plstore-get store "basic")))
                   (plstore-close store)
                   (plist-get (cdr plist) :access-token))
               (let* ((token (read-from-minibuffer "Personal access-token: "))
                      (store (plstore-open azure-access-token-file)))
                 (plstore-put store "basic" nil `(:access-token ,token))
                 (plstore-save store)
                 (plstore-close store)
                 token))))))

;; Caching

;; Some parts of ones Azure-setup rarely change, so we do some
;; rudimentary caching to save a few requests. We also keep
;; search-filters for the life of a session.

(defcustom azure-cache-directory (concat user-emacs-directory "azure")
  "Directory used for caching. We need files on disk for clocking and
  the org-agenda to work as intended.
  Note that if you change this directory, you'll need to run
  azure-init in order for the agenda to update."
  :group 'azure
  :type 'directory)

(defvar azure--projects '()
  "List of projects.")

(defvar azure--teams '()
  "List of teams.")

(defvar azure--boards '()
  "List of boards.")

(defvar azure--query nil
  "Query currently used for filtering.")

(defvar azure--board nil
  "Board currently used for filtering.")

(defvar azure--assignee nil
  "Assignee currently used for filtering.")

(defvar azure--state nil
  "Work-item state currently used for filtering.")

;; Required information


(defcustom azure-organization nil
  "The name of the Azure DevOps organization."
  :group 'azure
  :type 'string)

(defcustom azure-project nil
  "Project ID or project name."
  :group 'azure
  :type 'string)

(defcustom azure-team nil
  "Team ID or team name."
  :group 'azure
  :type 'string)

(defcustom azure-search-latest-atop t
  "Wether to order the search-results with the latest entry first or last."
  :group 'azure
  :type 'boolean)

(defcustom azure-discussion-latest-atop nil
  "Wether to order the thread of discussion with the latest comment first or last."
  :group 'azure
  :type 'boolean)

(defcustom azure-search-show-header t
  "Wether to show or hide the header in the search-results buffer."
  :group 'azure
  :type 'boolean)

(defcustom azure-search-results-max 100
  "Maximum number of results returned when searching for work-items.
   <b>200</b> is the maximum supported by Azure's API."
  :group 'azure
  :type 'natnum)

(defcustom azure-search-buffer "*azure searching %P*"
  "Name of the buffer used to display search results.

   Note that you can add certain properties via formatting specifiers:
       %O - Organization
       %P - Project
       %T - Team"
  :group 'azure
  :type 'string)

(defcustom azure-item-buffer "*azure - %t*"
  "Name of the buffer used to display a work-item.

   Note that you can add certain properties via formatting specifiers:
       %O - Organization
       %P - Project
       %T - Team
       %t - Item title
       %a - Item assignee"
  :group 'azure
  :type 'string)

(defcustom azure-debug nil
  "Wether to output debug-information. Only relevant to contributors.")

(defvar azure-mapping-states '(("To Do" . "")
                               ("Doing" . "TODO")
                               ("Done" . "DONE"))
  "Align work-item states with TODO-states of org-mode.")

(defconst azure-api-version "6.0"
  "Fallback version of the Azure-API to use if api-version is not
  set per request.")

(defconst azure-url
  "https://dev.azure.com/{organization}/{project}/{team}/_apis/{api}"
  "Base-URL of the Azure API. Note that the API spans multiple hosts,
  but this is the most common one.")

;; Menus and bindings


(defcustom azure-use-menu t
  "Show a dedicated menu for Azure in the menu-bar."
  :group 'azure
  :type 'boolean)

(defvar azure-select-project-hook nil
  "Hook run when a project is selected.")

(defvar azure-select-team-hook nil
  "Hook run when a team is selected.")

(defvar azure-minor-mode-hook nil
  "Hook that's run when `azure-minor-mode` is turned on.")

(defvar azure-search-mode-hook nil
  "Hook that's run when `azure-search-mode` is turned on.")

(defvar azure-minor-mode-menu
  (let ((map (make-sparse-keymap)))
    map)
  "Menu-map used when `azure-minor-mode` is turned on.")

(defvar azure-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used when `azure-minor-mode` is turned on.")

(defvar azure-search-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "RET") 'azure-work-item)
    map)
  "Keymap used with the work-item search.")

(defvar azure-work-item-menu
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used when visiting a work-item.")

(easy-menu-define azure-minor-mode-menu
  azure-minor-mode-map
  "Menu available when azure-minor-mode is enabled."
  '("Azure" :visible azure-use-menu
    ["----"
     :visible (not (azure--valid-p))]
    ["Initialize" azure-init
     :visible (not (azure--valid-p))
     :help "Setup azure.el for first-time use."]
    "----"
    ["Search for work-item" azure-search-mode
     :help "List and search for work-items."]
    ["Show work-item" azure-work-item
     :help "Quickly find and show a specific work-item."]
    ["Create work-item" azure-work-item-create
     :help "Create a new work-item."]))

;; Logging


(defun azure-log (&rest args)
  "Like `message`, but will only output when `azure-debug` is not `nil`."
  (when azure-debug
    (cl-fresh-line)
    (apply 'message args)))

;; Request handling


;; (defvar progress (make-progress-reporter "Synchronizing with Azure..." 0 500))

;; (defvar timer nil)

(defun azure-req (method api success &optional params data headers)
  "Make a request to the Azure API and return it to the passed in SUCCESS-handler.
  Note that instead of using this function directly, you should use
  the helper-functions. `azure-get` etc.

  METHOD should be one of (GET, PUT, POST, PATCH)

  API is the path to the resource in Azure's API or a full URL

  SUCCESS is the handler that gets the results of the request.

  Optionally, you can pass additional PARAMS, DATA & HEADERS.
  <i>Note that DATA is treated as json.<i>
  "
  (let ((url (s-replace-all `(("{organization}" . ,azure-organization)
                              ("{project}" . ,azure-project)
                              ("{team}" . ,azure-team)
                              ("{api}" . ,api))
                            (if (s-starts-with? "https" api) api azure-url)))
        (params (a-merge `(("api-version" . ,azure-api-version)) (or params '())))
        (data (or data '()))
        (headers (a-merge `(("Authorization" . ,(concat "Basic " (azure-access-token)))
                            ("Accepts" . "application/json")
                            ("Content-Type" . "application/json")
                            ("User-Agent" . "azure.el"))
                          (or headers '()))))
    (azure-log "Azure requested: %s" url)
    ;; (when (eq timer nil)
    ;;   (setq timer (run-with-timer 0.2 t (lambda () (progress-reporter-update progress)))))
    (request (url-encode-url url)
      :type (upcase method)
      :data (json-encode data)
      :params params
      :parser 'json-read
      :headers headers
      :success success
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (progn
                  (azure-log "Arguments when error occurred:\n%s" args)
                  (error "%s" error-thrown))))
      ;; :complete (cl-function
      ;;            (lambda (&key response &allow-other-keys)
      ;;              (cancel-timer timer)
      ;;              (progress-reporter-done progress)))
      )))

(defun azure-get (api success &optional params)
  "GET a resource and return it to the success-handler."
  (azure-req "GET" api success params))

(defun azure-put (api success &optional params)
  "PUT to a resource and return the result to the success-handler."
  (azure-req "PUT" api success params))

(defun azure-patch (api params success)
  "PATCH a resource and return the result to the success-handler."
  (azure-req "PATCH" api success params))

(defun azure-post (api success &optional data params headers)
  "POST a resource and return the result to the success-handler."
  (azure-req "POST" api success params data headers))

;; Helper functions


(defun azure--html-to-org (html)
  "Convert an HTML string into org-mode string."
  (shell-command-to-string
   (concat "echo \"" html "\" | pandoc -f html -t org")))

(defun azure--org-to-html (org)
  "Convert ORG-mode string into html string."
  (shell-command-to-string
   (concat "echo \"" org "\" | pandoc -f org -t html")))

;; [[https://docs.microsoft.com/en-us/rest/api/azure/devops/core/projects/list][Projects]]


(defun azure-select-project ()
  "Select a project from a list of all the projects in the
   organization that the authenticated user has access to.

   See URL 'https://docs.microsoft.com/en-us/rest/api/azure/devops/core/projects/list'
   for more information."
  (promise-new
   (lambda (resolve _reject)
     (let ((url "https://dev.azure.com/{organization}/_apis/projects"))
       (azure-get url
                  (cl-function
                   (lambda (&key data &allow-other-keys)
                     (let* ((projects (mapcar (lambda (project)
                                                (cdr (assoc 'name project)))
                                              (cdr (assoc 'value data))))
                            (project (completing-read "Select project: " projects)))
                       (message "Switched to azure-project %s" project)
                       (setq azure-project project)
                       (run-hooks 'azure-select-project-hook)
                       (funcall resolve project)))))))))

;; [[https://docs.microsoft.com/en-us/rest/api/azure/devops/core/teams/get-all-teams][Teams]]


(defun azure--team-members (callback)
  "Get a list of members for a specific team and return it to the CALLBACK."
  (azure-get "members/"
             (cl-function
              (lambda (&key data &allow-other-keys)
                (callback (cdr (assoc 'value data)))))))

(defun azure-select-team ()
  "Select a team from a list of all the teams in the
   organization that the authenticated user has access to.

   See URL 'https://docs.microsoft.com/en-us/rest/api/azure/devops/core/teams/get-all-teams'
   for more information."
  (promise-new
   (let ((url "https://dev.azure.com/{organization}/_apis/teams"))
     (lambda (resolve _reject)
       (azure-get url
                  (cl-function
                   (lambda (&key data &allow-other-keys)
                     (let* ((teams (mapcar (lambda (team)
                                             (cdr (assoc 'name team)))
                                           (cdr (assoc 'value data))))
                            (team (completing-read "Select team: " teams)))
                       (azure-log "Switched to %s team" team)
                       (setq azure-team team)
                       (run-hooks 'azure-select-team-hook)
                       (funcall resolve team))))
                  '(("api-version" . "7.1-preview.3")))))))

;; Faces


(defface azure-item-id '((t :inherit shadow))
  "Face used with a work-items id.")

(defface azure-item-title '((t :inherit default))
  "Face used with a work-items title.")

(defface azure-item-state '((t :inherit bold))
  "Face used with a work-items state.")

(defface azure-item-tags '((t :inherit italic))
  "Face used with a work-items tags.")

;; Header

(defun azure--test-output ()
  (message "Test output"))

(defun azure--define-mouse-key (command)
  "Defines a mouse-action to be used with the head-line widgets."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-2]
                (lambda (click)
                  (interactive "e")
                  (azure-log "Clicked!")
                  (mouse-select-window click)
                  (call-interactively command)))
    map))

(defun azure--search-header-string-query ()
  "Tap search-query in the header-line to interactively change it."
  (let ((map (azure--define-mouse-key 'azure--test-output)))
    `(:propertize ,(truncate-string-to-width (format " Query: %s " azure--query) 100 nil 32 "…")
                  mouse-face header-line-highlight
                  help-echo "Change search query"
                  keymap ,map)))

(defun azure--search-header-board ()
  "Tap the board-name in the header-line to change it."
  (let ((map (azure--define-mouse-key 'azure--test-output)))
    `(:propertize ,(truncate-string-to-width (format " Board: %s " azure--board) 100 nil 32 "…")
                  mouse-face header-line-highlight
                  help-echo "Change board"
                  keymap ,map)))

(defun azure--search-header-assignee ()
  "Tap the assignee-name in the header-line to change it."
  (let ((map (azure--define-mouse-key 'azure--test-output)))
    `(:propertize ,(truncate-string-to-width (format " Assignee: %s " azure--assignee) 100 nil 32 "…")
                  mouse-face header-line-highlight
                  help-echo "Change assignee"
                  keymap ,map)))

(defun azure--search-header-state ()
  "Tap the state-name in the header-line to change it."
  (let ((map (azure--define-mouse-key 'azure--test-output)))
    `(:propertize ,(truncate-string-to-width (format " State: %s " azure--state) 100 nil 32 "…")
                  mouse-face header-line-highlight
                  help-echo "Change state"
                  keymap ,map)))

(defun azure--search-header-line ()
  "Header-line used with the search-buffer to enable various filtering."
  (let ((space "\t\t"))
    (setq-local
     header-line-format
     (list
      (azure--search-header-string-query) space
      (azure--search-header-board) space
      (azure--search-header-assignee) space
      (azure--search-header-state)))))

;; [[https://docs.microsoft.com/en-us/rest/api/azure/devops/search/work-item-search-results/fetch-work-item-search-results][Work Item Search Results]]


(defun azure--search (&optional text assignee status)
  "Query azure's API for work-items.

   See URL 'https://docs.microsoft.com/en-us/rest/api/azure/devops/search/work-item-search-results/fetch-work-item-search-results'
   for more information."
  (let ((url "https://almsearch.dev.azure.com/{organization}/{project}/_apis/search/workitemsearchresults")
        (top (math-min (math-max 0 azure-search-results-max) 200)))
    (azure-post url
                (cl-function
                 (lambda (&key data &allow-other-keys)
                   (let* ((work-items (mapcar
                                       (lambda (item)
                                         (mapcar 'cdr (cdr (assoc 'fields item))))
                                       (cdr (assoc 'results data))))
                          (work-items (sort work-items
                                            (lambda (a b)
                                              (not (s-less? (nth 7 a) (nth 7 b)))))))
                     (setq azure--work-items (if azure-search-latest-atop work-items (reverse work-items)))
                     (azure--update-search-buffer))))
                `(("searchText" . ,(or text "NOT null"))
                  ("$skip" . "0")
                  ("$top" . ,top)
                  ("includeFacets" . "true")
                  ("filters" . ("System.AssignedTo" . (,(or assignee "")))))
                '(("api-version" . "7.1-preview.1")))))

;; Results buffer

;; When doing a search (~azure-search~), we validate the configuration
;; first via ~azure-init~.  The rest is handled interactively from inside
;; the search results buffer.

(defun azure--buffer-name (buffer-name)
  "Get the formatted/compiled BUFFER-NAME."
  (s-replace-all `(("%O" . ,azure-organization)
                   ("%P" . ,azure-project)
                   ("%T" . ,azure-team))
                 buffer-name))

(defvar azure--work-items '()
  "Work-items currently being listed.")

(defun azure-search-selected ()
  "Return the currently selected work-item from the search results list."
  (let* ((item-num (- (line-number-at-pos (point)) 1))
         (work-item (nth item-num azure--work-items)))
    work-item))

(defun azure--setup-search-buffer ()
  "Setup of the buffer that holds our search-results."
  (let ((buf (get-buffer-create (azure--buffer-name azure-search-buffer))))
    (switch-to-buffer buf)
    (read-only-mode)
    (hl-line-mode)
    (buffer-disable-undo)
    (when azure-search-show-header
      (azure--search-header-line))))

(defun azure--update-search-buffer ()
  "Update the search-buffer with WORK-ITEMS."
  (let ((buf (get-buffer (azure--buffer-name azure-search-buffer)))
        (map (azure--define-mouse-key
              (lambda ()
                (let* ((work-item (azure-search-selected))
                       (id (cdr (assoc 'id work-item))))
                  (azure-work-item id))))))
    (azure-log "Work items: %S" azure--work-items)
    (with-current-buffer buf
      (setq inhibit-read-only t)
      (erase-buffer)
      (goto-char (point-min))
      (mapcar
       (lambda (item)
         (pcase-let
             ((`(,id ,type ,title ,assignee ,state ,tags ,_ ,created ,changed) item))
           (let* ((width (- (window-width) 30 (string-width "\t\t\t\t")))
                  (fmt (concat "%." (format "%d" width) "s"))
                  (title (truncate-string-to-width (s-collapse-whitespace title) width nil 32 "…")))
             (insert (format "\t%s\t%s\t%s\t%s\n" 
                             (propertize id 'face 'azure-item-id)
                             (propertize state 'face 'azure-item-state)
                             (propertize title 'face 'azure-item-title 'keymap map)
                             (propertize (if (s-blank? tags) "" (format "(%s)" tags)) 'face 'azure-item-tags))))))
       azure--work-items)
      (setq inhibit-read-only nil))))

(defun azure-search-mode (&optional project team query board assignee state)
  "Major-mode to search for work-items using a dedicated buffer.
\\{azure-search-mode-map}"
  (interactive)
  (if (azure--valid-p)
      (progn
        (azure--setup-search-buffer)
        (setq major-mode 'azure-search-mode
              mode-name "azure-search"
              font-lock-defaults '(nil))
        (azure--search query assignee state)
        (use-local-map azure-search-mode-map)
        (add-hook 'quit-window-hook 'azure-search-quit nil 'local)
        (add-hook 'window-configuration-change-hook 'azure--update-search-buffer nil 'local)
        (run-mode-hooks 'azure-search-mode-hook))
    (user-error "You need to run `azure-init` first!")))

(defun azure-search-quit ()
  "Quits azure-search and kills the search buffer."
  (remove-hook 'window-configuration-change-hook 'azure--update-search-buffer nil 'local)
  (remove-hook 'quit-window-hook 'azure-search-quit nil 'local)
  (kill-buffer (azure--buffer-name azure-search-buffer)))

;; Comments


(defun azure--comments (id)
  ""
  (promise-new
   (lambda (resolve _reject)
     (let ((url (format "https://dev.azure.com/{organization}/{project}/_apis/wit/workItems/%d/comments" id)))
       (azure-get url
                  (cl-function
                   (lambda (&key data &allow-other-keys)
                     (let ((comments (cdr (assoc 'comments data))))
                       (funcall resolve comments)
                       (azure-log "Comments: %S" comments))))
                  '(("api-version" . "7.1-preview.3")))))))

;; Work items


(defun azure--item-buffer (title assignee)
  "Returns the compiled name of a work-item buffer."
  (s-replace-all `(("%O" . ,azure-organization)
                   ("%P" . ,azure-project)
                   ("%T" . ,azure-team)
                   ("%t" . ,title)
                   ("%a" . ,assignee))
                 azure-item-buffer))

;; Work Item Buffer


(defun azure-work-item-file (id)
  "Expanded file-path of the work-item prefixed with ID."
  (car
   (file-expand-wildcards
    (expand-file-name (format "%d-*.org" id) azure-cache-directory))))

(defun azure--create-or-flush-work-item-buffer (id)
  "Open the file associated with the work-item with ID and update it's content.

   If a file does not exist, a new one will be created."
  (promise-new
   (lambda (resolve _reject)
     (let ((logbook-p nil)
           (check-point nil))
       (when (eq (azure-work-item-file id) nil)
         (let* ((new-name (format "%d-Not-yet-updated.org" id))
                (buf (generate-new-buffer new-name)))
           (azure-log "Creating a new work-item file - %S" new-name)
           (save-excursion
             (with-current-buffer buf
               (org-mode)
               (insert "\n\n* Personal Notes\n")
               (write-file (expand-file-name new-name azure-cache-directory))))))
       (find-file (azure-work-item-file id))
       (with-current-buffer (current-buffer)
         (goto-char (point-min))
         ;; Delete everything before a logbook entry, when an entry exists
         (azure-log "Check for logbook entry")
         (save-excursion
           (while (re-search-forward ":logbook:" nil 'noerror)
             (azure-log "Delete everything before logbook entry")
             (delete-region (point) (match-beginning 0))
             (setq logbook-p t)))
         (goto-char (point-min))
         (setq check-point (point))
         (azure-log "Start from the beginning again")
         (when logbook-p
           (while (re-search-forward ":logbook:.+:end:" nil)
             (goto-char (match-end 0))
             (setq check-point (match-end 0))))
         ;; Delete everything after the logbook entry and before the personal notes section
         (save-excursion
           (while (re-search-forward "* Personal Notes" nil 'noerror)
             (azure-log "Delete everything after logbook entry except personal notes")
             (delete-region check-point (- (match-beginning 0) 1))))
         (azure-log "Return the work-item buffer - %S" (buffer-name (current-buffer)))
         (funcall resolve (buffer-name (current-buffer))))))))

(defun azure--work-item-properties (work-item)
  ""
  (let* ((fields (cdr (assoc 'fields work-item)))
         (id (cdr (assoc 'id work-item)))
         (rev (cdr (assoc 'rev work-item)))
         (state (cdr (assoc 'System.State fields)))
         (created (cdr (assoc 'System.CreatedDate fields)))
         (by (cdr (assoc 'displayName
                         (cdr (assoc 'System.CreatedBy fields))))))
    (format ":properties:\n:id: %d\n:rev: %d\n:state: %s\n:created: %s\n:created-by: %s\n:end:\n" id rev state created by)))

(defun azure--work-item-title (work-item)
  ""
  (let* ((fields (cdr (assoc 'fields work-item)))
         (state (s-trim (cdr (assoc (cdr (assoc 'System.State fields)) azure-mapping-states))))
         (title (cdr (assoc 'System.Title fields))))
    (format "* %s%s\n" (if (s-blank? state) "" (concat state " ")) title)))

(defun azure--work-item-content (work-item)
  ""
  (let* ((fields (cdr (assoc 'fields work-item)))
         (description (cdr (assoc 'System.Description fields))))
    (azure--html-to-org description)))

(defun azure--work-item-comments (comments)
  ""
  (let ((comments (if azure-discussion-latest-atop comments (reverse comments)))
        (template (s-join "\n" [":properties:"
                                ":id: %d"
                                ":created: %s"
                                ":created-by: %s"
                                ":end:"
                                "%s"
                                ""])))
    (azure-log "Discussion (%d): %S" (length comments) comments)
    (format "\n\n* Discussion (%d)\n\n%s" (length comments) 
            (s-join "\n" (mapcar
                          (lambda (comment)
                            (let ((id (cdr (assoc 'id comment)))
                                  (text (s-trim (azure--html-to-org (cdr (assoc 'text comment)))))
                                  (by (cdr (assoc 'displayName (cdr (assoc 'createdBy comment)))))
                                  (created (cdr (assoc 'createdDate comment))))
                              (format template id created by text)))
                          comments)))))

(async-defun azure--update-work-item-buffer (id)  
  "Update the work-item buffer.

   We retrieve all the information needed first and if that succeeds,
   we replace everything in our local copy of the issue with what we
   retrieved. Only clocking and personal notes are persisted from the
   local copy."
  (let ((work-item (await (azure--work-item-get id)))
        (comments (await (azure--comments id)))
        (buf (await (azure--create-or-flush-work-item-buffer id)))
        (logbook-p nil))
    (with-current-buffer buf
      (azure-log "Patch in changes to buffer")
      (goto-char (point-min))
      (insert (azure--work-item-properties work-item))
      (insert (azure--work-item-title work-item))
      (save-excursion
        (while (re-search-forward ":logbook:" nil 'noerror)
          (azure-log "Logbook entry was found!")
          (setq logbook-p t)))
      (when logbook-p
        (while (re-search-forward ":end:" nil)
          (azure-log "Logbook entry was closed!")
          (goto-char (match-end 0))))
      (insert (azure--work-item-content work-item))
      (insert (azure--work-item-comments comments))
      (message "Work item updated!"))))

(defun azure-work-item (id)
  "Show the work-item with ID in a buffer of it's own.

  See URL 'https://docs.microsoft.com/en-us/rest/api/azure/devops/wit/work-items/get-work-item'
  for more information."
  (interactive (list (azure-search-selected-id)))
  (funcall 'azure--update-work-item-buffer id))

;; [[https://docs.microsoft.com/en-us/rest/api/azure/devops/wit/work-items/create][Create]]


(defun azure-work-item-create (item-type title)
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
                   (azure-work-item (cdr (assoc 'id data)))))
                `((("op" . "add")
                   ("path" . "/fields/System.title")
                   ("from" . nil)
                   ("value" . ,title)))
                '(("api-version" . "7.1-preview.3"))
                '(("Content-Type" . "application/json-patch+json")))))

;; [[https://docs.microsoft.com/en-us/rest/api/azure/devops/wit/work-items/get-work-item][Get Work Item]]


(defun azure--work-item-get (id)
  "Get all the relevant information about a work-item by it's ID.

  See URL 'https://docs.microsoft.com/en-us/rest/api/azure/devops/wit/work-items/get-work-item'
  for more information."
  (promise-new
   (lambda (resolve _reject)
     (azure-get (format "https://dev.azure.com/{organization}/{project}/_apis/wit/workitems/%d" id)
                (cl-function
                 (lambda (&key data &allow-other-keys)
                   (progn (azure-log "Work item: %S" data)
                          (funcall resolve data))))
                '(("$expand" . "All")
                  ("api-version" . "7.1-preview.3"))))))

;; Initialization

;; In order to use Azure's API, we need to set the required fields to
;; valid values. This can all be done interactively via ~azure-init~. If
;; you are located in the project in question, you can also save the
;; fields to a ~.dir-locals.el~ file so that you don't need to repeat the
;; initialization over and over.

(defun azure--save-dir-locals ()
  "Creates or modifies .dir-locals.el with preferences required by azure.el."
  (when (read-answer
         (concat
          (propertize "Would you like to save these settings to " 'face '(default))
          (propertize ".dir-locals.el`" 'face '(bold default))
          (propertize "?" 'face '(default)))
         '(("yes" ?y "Save to disk")
           ("no" ?n "Skip")))
    (save-excursion
      (add-dir-local-variable nil 'azure-organization azure-organization)
      (add-dir-local-variable nil 'azure-project azure-project)
      (add-dir-local-variable nil 'azure-team azure-team)
      (save-buffer))))

(async-defun azure-init ()
  "Set required fields and add our cache-directory to the org-agenda.

  You'll be prompted if these settings should be persisted to disk."
  (interactive)
  (when (eq azure-organization nil)
    (setq azure-organization
          (url-encode-url
           (read-from-minibuffer "Organization name: "))))
  (when (eq azure-project nil)
    (await (azure-select-project)))
  (when (eq azure-team nil)
    (await (azure-select-team)))
  (azure--save-dir-locals)
  (make-directory azure-cache-directory 'make-parents)
  (add-to-list 'org-agenda-files azure-cache-directory))

(defun azure--valid-p ()
  "Predicate of wether all required configurations are set."
  (and (not (eq azure-organization nil))
       (not (eq azure-project nil))
       (not (eq azure-team nil))))

;; Minor mode

;; This package is written as a minor-mode in order to cleanly provide
;; menus & bindings.

;;;###autoload
(define-minor-mode azure-minor-mode
  "Toggle Azure mode.

   When Azure mode is enabled, you can access azure-commands from the
   mode-line and/or menu-bar."
  :global t
  :group 'azure
  :lighter " azure"
  :keymap azure-minor-mode-map
  (when azure-minor-mode
    (run-mode-hooks 'azure-minor-mode-hook)))

(provide 'azure)
;;; azure.el ends here
