;;; auto-project.el --- Loads modes based on git project

;; Copyright (C) 2014 Pierre Larochelle

;; Author: Pierre Larochelle <pierre@larochelle.io>
;; Version: 2.9.0
;; Keywords: lists

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A way to load modes based on git project
;;
;; See documentation on https://github.com/pierrel/auto-project.el

;;; Code:

(require 'project-root)
(require 'dash)

(defgroup auto-project ()
  "Customize group for auto-project.el"
  :group 'lisp
  :prefix "auto-project")

(defvar *auto-project-configs* '())

(defun auto-project-config-file ()
  (with-project-root
      (concat default-directory ".emacs-config.el")))

(defun auto-project-load-file (file)
  (if (not (-contains? *auto-project-configs* file))
      (progn
        (add-to-list '*auto-project-configs* file)
        (load-file file))))

(defun auto-project-load-config ()
  (ignore-errors
    (let ((config-file (auto-project-config-file)))
      (if (file-regular-p config-file)
        (auto-project-load-file config-file)))))

(add-to-list 'find-file-hook 'auto-project-load-config)
