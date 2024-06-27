;;; azure-evil.el --- Evil bindings for azure.el & friends -*- coding: utf-8; lexical-binding: t; -*-
;; Author: Henrik Kjerringvåg <henrik@kjerringvag.no>
;; Version: 2022.07.15
;; URL: https://github.com/hkjels/azure.el
;; Keywords: tools, azure, evil
;; Package-Requires: ((emacs "28.1") (evil "1.15.0") (azure "2022.07.15"))

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
;;; This package provides Evil bindings for azure.el.



;; Code:

(require 'evil)
(require 'azure)
(require 'azure-devops)

;; Bindings for devops

(evil-set-initial-state 'azure-devops-search-mode 'motion)

(evil-define-key 'motion azure-devops-search-mode-map
  (kbd "RET") #'azure-devops-work-item)

(provide 'azure-evil)
;;; azure-evil.el ends here
