;;; egg-base.el --- Emacs Got Git - Emacs interface to Git

;; Copyright (C) 2008  Linh Dang
;; Copyright (C) 2008  Marius Vollmer
;; Copyright (C) 2009  Tim Moore
;; Copyright (C) 2010  Alexander Prusov
;; Copyright (C) 2011-12 byplayer
;;
;; Author: Bogolisk <bogolisk@gmail.com>
;; Created: 19 Aug 2008
;; Version: 1.0.2
;; Keywords: git, version control, release management
;;
;; Special Thanks to
;;   Antoine Levitt, Bogolisk,
;;   Christian Köstlin
;;   Max Mikhanosha
;;   Aleksandar Simic
;;
;; Egg is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Egg is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary
;;;    This is my fork of Marius's excellent magit. his work is at:
;;;    http://zagadka.vm.bytemark.co.uk/magit
;;;
;;;    This is my fork of bogolisk egg . his work is at
;;     http://github.com/bogolisk/egg
;;
;;  ssh and github: please use key authentication since egg doesn't
;;                  handle login/passwd prompt
;;
;;  gpg and tag : please add "use-agent" option in your gpg.conf
;;                since egg doesn't handle passphrase prompt.
;;;

;; Options
;; If you want to auto-update egg-status on file save,
;;   you set follow value on your .emacs.
;; (setq egg-auto-update t)
;;
;; Set to nonnil for egg-status to switch to the status buffer in the same window.
;; (setq egg-switch-to-buffer t)
;;
;; If you want to change prefix of lunch egg,
;;  you set follow value on your .emacs.
;; (custom-set-variables
;;   '(egg-mode-key-prefix "C-c v"))

(require 'egg-custom)
(eval-when-compile (require 'cl))

;;;========================================================
;;; simple routines
;;;========================================================

(defsubst egg-unquote-posix-regexp (string)
  (while (string-match "\\\\[\\|()]" string)
    (setq string (concat (substring string 0 (match-beginning 0))
			 (substring string (1+ (match-beginning 0))))))
  string)


(defmacro invoked-interactively-p ()
  "wrapper for checking if the function was invoked interactively,
works around the deprecation of 'interactive-p' after Emacs 23.2"
  (if (> emacs-major-version 23)
      '(called-interactively-p 'interactive)
    (if (> emacs-minor-version 2)
	'(called-interactively-p 'interactive)
      '(interactive-p))))


(defmacro egg-text (text face)
  "Format TEXT with face FACE at compile-time or run-time."
  (cond ((stringp text)
         (propertize text 'face (if (symbolp face) face
                                  (nth 1 face))))
        ((null text)
         `(propertize "<internal-bug>" 'face ,face))
        (t `(propertize ,text 'face ,face))))

(defsubst egg-pretty-help-text (&rest strings)
  "Perform key bindings substitutions and highlighting in STRINGS."
  (let* ((map (current-local-map)) last-found)
    (with-temp-buffer
      (use-local-map map)
      (save-match-data
        ;; key substitutions
        (insert (substitute-command-keys
                 (mapconcat 'identity strings "")))
        (goto-char (point-min))
        ;; key highlighting
        (while (re-search-forward "\\(\\<[^\n \t:]+\\|[/+.~*=-]\\):" nil t)
          (put-text-property (match-beginning 1) (match-end 1)'face 'egg-help-key)
          (if last-found
              (put-text-property last-found (1- (match-beginning 0))
                                 'face 'egg-text-help))
          (setq last-found (point)))
        (if last-found
            (put-text-property last-found (line-end-position) 'face 'egg-text-help))
        ;; return the total
        (buffer-string)))))

(defmacro egg-prop (text &rest prop)
  "Propertize TEXT with properties list PROP at compile-time or run-time."
  (if (stringp text)
      (apply 'propertize text
             (mapcar (lambda (sym)
                       (if (consp sym)
                           (nth 1 sym)
                         sym))
                     prop))
    `(propertize ,text ,@prop)))


(autoload  'find-file-at-point "ffap" 
  "Find filename, guessing a default from text around point." t)
(defsubst egg-string-at-point () (current-word t))

(defsubst egg-goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defsubst egg-prepend (str prefix &rest other-properties)
  "Make STR appear to have prefix PREFIX.
If OTHER-PROPERTIES was non-nil, apply it to STR."
  (setq prefix (concat prefix (substring str 0 1)))
  (setq str (apply 'propertize str other-properties))
  (put-text-property 0 1 'display prefix str)
  str)


(defun egg-pick-file-contents (file-name regexp &rest indices)
  "Pick a string out of the contents of the file FILE-NAME.
This function searches for and return the 1st match of REGEXP on the
contents of the file. If indices was not nil, then return the first
successful submatch in the order in INDICES."
  (with-temp-buffer
    (insert-file-contents-literally file-name)
    (goto-char (point-min))
    (when (re-search-forward regexp nil t)
      (if (null indices)
          (match-string-no-properties 0)
        (dolist (idx indices)
          (if (match-beginning idx)
              (return (match-string-no-properties idx))))))))

(defun egg-pick-file-records (file-name start-re end-re)
  "Return a list of strings from the contents of the file FILE-NAME.
START-RE is the regexp to match the beginning of a record.
END-RE is the regexp to match the end of a record."
  (with-temp-buffer
    (insert-file-contents-literally file-name)
    (goto-char (point-min))
    (let ((beg (point-min))
          (end (point-max))
          lst)
      (save-match-data
        (while (and (> end beg)
                    (not (eobp))
                    (re-search-forward start-re nil t))
          (setq beg (match-beginning 0))
          (when (re-search-forward end-re nil t)
            (setq end (match-beginning 0))
            (if (> end beg)
                (setq lst (cons (buffer-substring-no-properties
                                 beg (match-beginning 0))
                                lst)))
            (goto-char end))))
      lst)))

(defsubst egg-rbranch-to-remote (rbranch)
  "Return the remote name in the remote-branch RBRANCH.
E.g: `foo' in `foo/bar'"
  (and (stringp rbranch)
       (> (length rbranch) 0)
       (directory-file-name (file-name-directory rbranch))))

(defsubst egg-rbranch-name (rbranch)
  "Return the ref name in the remote-branch RBRANCH.
E.g: `bar' in `foo/bar'"
  (and (stringp rbranch)
       (> (length rbranch) 0)
       (file-name-nondirectory rbranch)))

(defsubst egg-file-as-string-raw (file-name)
  (with-temp-buffer
    (insert-file-contents-literally file-name)
    (buffer-string)))

(defsubst egg-file-as-string (file-name)
  "return the contents of file FILE-NAME as a string."
  (with-temp-buffer
    (insert-file-contents-literally file-name)
    (buffer-substring-no-properties
     (point-min) (if (> (point-max) (point-min))
                     (1- (point-max)) (point-max)))))

(defvar egg-gpg-agent-info nil)
(defun egg-gpg-agent-info (&optional action-if-not-set)
  (or egg-gpg-agent-info
      (setq egg-gpg-agent-info
	    (let* ((file (and (file-readable-p "~/.gpg-agent-info")
			      (expand-file-name "~/.gpg-agent-info")))
		   (info (and file (egg-pick-file-contents 
				    file "^GPG_AGENT_INFO=\\(.+\\)$" 1)))
		   (env (getenv "GPG_AGENT_INFO"))
		   (info-list (and (stringp info) (save-match-data 
						    (split-string info ":" t))))
		   (socket (and info-list (car info-list)))
		   (agent-pid (and info-list (string-to-number (nth 1 info-list))))
		   (agent-attr (and agent-pid (process-attributes agent-pid)))
		   (agent-cmdline (and agent-attr (cdr (assq 'args agent-attr))))
		   agent-info)
	      (setq agent-info
		    (if (stringp env)
			env ;; trust the environment
		      (when (and info 
				 (file-exists-p socket)
				 (= (aref (nth 8 (file-attributes socket)) 0) ?s)
				 agent-attr
				 (save-match-data
				   (string-match "gpg-agent" agent-cmdline)))
			info)))
	      (when (and (not env) agent-info)
		(cond ((eq action-if-not-set 'set)
		       (setenv "GPG_AGENT_INFO" agent-info))
		      ((stringp action-if-not-set)
		       (if (y-or-n-p (format action-if-not-set agent-info))
			   (setenv "GPG_AGENT_INFO" agent-info)
			 (setq agent-info nil)))
		      ((null action-if-not-set)
		       ;; cancel everything!!!
		       (setq agent-info nil))
		      (t (error "What happened? (action-if-not-set = %s)"
				action-if-not-set))))
	      agent-info))))



;; misc
(defun egg-insert-texi-for-command ()
  (interactive)
  (let* ((func (symbol-at-point))
	 (name (or (and func (symbol-name func)) (completing-read "function: " obarray 'fboundp t nil nil)))
	 (doc (documentation (or func (intern name)))))
    (forward-line 1)
    (insert "@deffn Command " name "\n"
	    "@anchor{" name "}\n"
	    doc "\n"
	    "@end deffn\n")))

(defun egg-update-texi-command-doc ()
  (interactive)
  (let (fn)
    (save-excursion
      (goto-char (line-beginning-position))
      (when (looking-at "@deffn Command \\(egg-.+\\)\\s-*$")
	(setq fn (match-string-no-properties 1))
	(forward-line 1)
	(unless (looking-at "@anchor{")
	  (insert "@anchor{" fn "}\n"))))))

(defun egg-insert-texi-for-map ()
  (interactive)
  (let* ((map (symbol-at-point))
	 (map-name (or (and map (symbol-name map))
		       (completing-read "map: " obarray 'boundp t nil nil)))
	 (doc (documentation-property (or map (intern map-name))
				      'variable-documentation))
	 key command key-cmd-alist)
    (save-excursion
      (with-temp-buffer
	(insert doc)
	(goto-char (point-min))
	(while (re-search-forward "^\\([^\t\n]+\\)\t+\\(\\S-+\\)$" nil t)
	  (setq key (match-string-no-properties 1)
		command (match-string-no-properties 2))
	  (add-to-list 'key-cmd-alist (cons key command)))))
    (forward-line 1)
    (insert "@table @kbd\n")
    (dolist (pair (nreverse key-cmd-alist))
      (setq key (car pair)
	    command (cdr pair))
      (insert "@item " key "\n" "@ref{" command "}\n"))
    (insert "@end table")))

(provide 'egg-base)
