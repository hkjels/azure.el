;; -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Henrik Kjerringvåg <henrik@kjerringvag.no>
;; Version: 2022.07.15
;; URL: https://github.com/hkjels/azure.el
;; Keywords: tools, azure
;; Package-Requires: ((emacs "28.1") (async-await "1.1") (request "0.3.3") (a "1.0.0") (dash "2.19.1") (s "1.12.0"))

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


;; Interface for working with Azure's API's. There are no plans on my
;; behalf of supporting all the features of Azure, but rather serve my
;; day to day needs. Feel free to create PR's and issues for missing
;; functionality.



(require 'async-await)
(require 'easymenu)
(require 'plstore)
(require 'request)
(require 'json)
(require 'dash)
(require 'a)
(require 's)

(defgroup azure nil
  "Interact with Azure from the comfort of your favorite editor."
  :prefix "azure-"
  :link '(url-link "https://github.com/hkjels/azure.el")
  :group 'azure
  :group 'tools)



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
  "Returns access-token stored on disk or asks for token if not found.
   In either case, the access-token will be returned base64-encoded."
  (base64-encode-string
   (concat ":"
           (let ((azure-access-token-file (expand-file-name azure-access-token-file))
                 (this-command "azure-access-token"))
             (if (file-exists-p azure-access-token-file)
                 (let* ((store (plstore-open azure-access-token-file))
                        (_ (azure-log this-command "plstore: \"%s\" was opened" azure-access-token-file))
                        (plist (plstore-get store "basic")))
                   (plstore-close store)
                   (plist-get (cdr plist) :access-token))
               (let* ((token (read-from-minibuffer "Personal access-token: "))
                      (store (plstore-open azure-access-token-file)))
                 (plstore-put store "basic" nil `(:access-token ,token))
                 (plstore-save store)
                 (plstore-close store)
                 token))))))



;; Some parts of ones Azure-setup rarely change, so we do some
;; rudimentary caching to save a few requests. We also keep
;; search-filters for the life of a session.

(defcustom azure-cache-directory (concat user-emacs-directory "azure")
  "Directory used for caching. We need files on disk for clocking and
  the org-agenda to work as intended.
  Note that if you change this directory, you'll need to re-run
  azure-init in order for the agenda to update."
  :group 'azure
  :type 'directory)

(defvar azure--user nil
  "The user that is currently logged in.")

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
  "Work-item states currently used for filtering.")

(defcustom azure-pandoc-executable "pandoc"
  "The CLI used to convert to and from org-mode."
  :group 'azure
  :type 'string)

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

(defcustom azure-debug nil
  "Wether to output debug-information. Only relevant to contributors.")

(defconst azure-api-version "6.0"
  "Fallback version of the Azure-API to use if api-version is not
  set per request.")

(defconst azure-url
  "https://dev.azure.com/{organization}/{project}/{team}/_apis/{api}"
  "Base-URL of the Azure API. Note that the API spans multiple hosts,
  but this is the most common one.")

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

(defvar azure-minor-mode-menu
  (let ((map (make-sparse-keymap)))
    map)
  "Menu-map used when `azure-minor-mode` is turned on.")

(defvar azure-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used when `azure-minor-mode` is turned on.")

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
    ["Search for work-item" azure-devops-search
     :help "List and search for work-items."]
    ["Show work-item" azure-devops-work-item
     :help "Quickly find and show a specific work-item."]
    ["Create work-item" azure-devops-work-item-create
     :help "Create a new work-item."]))

(defcustom azure-log-buffer "*azure-log*"
  "Name of the buffer to output debugging information."
  :group 'azure
  :type 'string)

(defcustom azure-log-time-format "%H:%M:%S"
  "Format of the prepended timestamp of each logged line."
  :group 'azure
  :type 'string)

(defun azure-log (context &rest messages)
  "Log to a dedicated buffer set by `azure-log-buffer` when `azure-debug` is not `nil`.
   CONTEXT should be a string that lets you know where the message occurred.
   MESSAGES is what you want to log."
  (let ((trace (backtrace-get-frames 'azure-log)))
    (message trace)
    (when azure-debug
      (with-current-buffer (get-buffer-create azure-log-buffer)
        (read-only-mode -1)
        (goto-char (point-min))
        (insert (format "%s %s: %s\n"
                        (format-time-string azure-log-time-format (current-time))
                        context
                        (apply 'format messages)))
        (read-only-mode 1)))))

(defun azure-req (method api success &optional params data headers)
  "Make a request to the Azure API and return it to the passed in SUCCESS-handler.
  <i>Note that instead of using this function directly, you should use
  the helper-functions. `azure-get` etc.</i>

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
        (params (a-merge `(("api-version" . ,azure-api-version)) params))
        (headers (a-merge `(("Authorization" . ,(concat "Basic " (azure-access-token)))
                            ("Accepts" . "application/json")
                            ("Content-Type" . "application/json")
                            ("User-Agent" . "azure.el"))
                          headers))
        (this-command "azure-req"))
    (azure-log this-command "Requested URL: %s" url)
    (request (url-encode-url url)
      :type (upcase method)
      :data (json-encode data)
      :params params
      :parser 'json-read
      :headers headers
      :success success
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (let ((this-command "azure-req-err"))
                  (azure-log this-command "Arguments when error occurred:\n%s" args)
                  (error "%s" error-thrown)))))))

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



;; These helper-functions are why we rely on =pandoc=. We convert to and
;; from HTML and org-mode, so that we can work in regular text-documents.

(defun azure--html-to-org (html)
  "Convert an HTML string into org-mode string."
  (unless (executable-find azure-pandoc-executable)
    (error "The pandoc executable was not found on your PATH. It is a pre-requisite to azure.el."))
  (->> (shell-command-to-string (concat "echo \"" html "\" | pandoc -f html -t org"))
       (s-chop-left 2)
       (s-chop-right 2)))

(defun azure--org-to-html (org)
  "Convert org-mode string into HTML string."
  (unless (executable-find azure-pandoc-executable)
    (error "The pandoc executable was not found on your PATH. It is a pre-requisite to azure.el."))
  (format "%s"
          (shell-command-to-string
           (concat "echo \"" org "\" | pandoc -f org -t html"))))

(defun azure-get-current-user ()
  "Get information about the currently logged in user."
  (promise-new
   (lambda (resolve _reject)
     (let ((url "https://dev.azure.com/{organization}/_apis/connectiondata"))
       (azure-get url
                  (cl-function
                   (lambda (&key data &allow-other-keys)
                     (let ((this-command "azure-get-user"))
                       (progn (azure-log this-command "Logged in user: %S" data)
                              (funcall resolve (assoc :data data))))))
                  '(("api-version" . "7.0-preview")))))))

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
                       (azure-log this-command "Switched to azure-project: %s" project)
                       (message "Switched to azure-project %s" project)
                       (setq azure-project project)
                       (run-hooks 'azure-select-project-hook)
                       (funcall resolve project)))))))))

(defun azure--team-members (callback)
  "Get a list of members for a specific team and return it through a CALLBACK."
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
                       (azure-log this-command "Switched to team: %s" team)
                       (message "Switched to %s team" team)
                       (setq azure-team team)
                       (run-hooks 'azure-select-team-hook)
                       (funcall resolve team))))
                  '(("api-version" . "7.1-preview.3")))))))



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

(async-defun azure--set-user ()
  "Cache the currently logged in user."
  (when (eq azure--user nil)
    (let ((user (await (azure-get-current-user))))
      (azure-log this-command "Logged in as: %S" user)
      (setq-default azure--user user))))

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
  (azure--set-user)
  (make-directory azure-cache-directory 'make-parents)
  (add-to-list 'org-agenda-files azure-cache-directory))

(defun azure--valid-p ()
  "Predicate of wether all required configurations are set."
  (and (not (eq azure-organization nil))
       (not (eq azure-project nil))
       (not (eq azure-team nil))))



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
