(defvar emacs-root "~/src/emacs/")
(defun add-path (p)
  (add-to-list 'load-path
	       (concat emacs-root p)))
(defun load-file-in-site-lisp (p)
  (load-file (concat emacs-root "site-lisp/" p)))

(add-path "site-lisp/")
(add-path "site-lisp/rinari")
(add-path "site-lisp/evil")

(setq default-directory "~")

;; slowly migrating to package manager
;;; include the package manager
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

;; refresh the package db
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;;; install the packages
(setq packages '(pkg-info
                 paredit
                 clojure-mode
                 dash
                 cider
                 csv-mode
                 fiplr
                 inf-ruby
                 ))
(dolist (package packages)
  (unless (package-installed-p package)
    (package-install package)))

;; evil mode - vim-like...
; (require 'evil)
; (evil-mode 1)

;;; evil mode keybindings
;; (define-key evil-normal-state-map (kbd "\\f") 'fiplr-find-file)
;; (define-key evil-normal-state-map (kbd "\\xf") 'ido-find-file)
;; (define-key evil-normal-state-map (kbd "\\b") 'ido-switch-buffer)
;; (define-key evil-normal-state-map (kbd "\\1") 'delete-other-windows)
;; (define-key evil-normal-state-map (kbd "\\2") 'split-window-below)
;; (define-key evil-normal-state-map (kbd "\\3") 'split-window-right)
;; (define-key evil-normal-state-map (kbd "\\k") 'ido-kill-buffer)
;; (define-key evil-normal-state-map (kbd "\\ <left>")  'windmove-left)
;; (define-key evil-normal-state-map (kbd "\\ <right>") 'windmove-right)
;; (define-key evil-normal-state-map (kbd "\\ <up>")    'windmove-up)
;; (define-key evil-normal-state-map (kbd "\\ <down>")  'windmove-down)
;; (define-key evil-normal-state-map (kbd "\\p")  'project-root-ack)

;; highlight symbol
(require 'highlight-symbol)
(setq highlight-symbol-mode 't)

;; project root
(add-path "site-lisp/project-root")
(require 'project-root)
(setq project-roots
      '(("git" :root-contains-files (".git"))))
(global-set-key (kbd "C-c p f") 'project-root-find-file)
(global-set-key (kbd "C-c p g") 'project-root-grep)
(global-set-key (kbd "C-c p a") 'project-root-ack)
(global-set-key (kbd "C-c p d") 'project-root-goto-root)
(global-set-key (kbd "C-c p l") 'project-root-browse-seen-projects)

;; find file in project
(add-path "site-lisp/grizzl/")
(require 'grizzl)
(setq fiplr-ignored-globs '((directories (".git" ".svn" "vendor"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))
(global-set-key (kbd "C-x f") 'fiplr-find-file)

;; git
(add-path "site-lisp/egg/")
(require 'egg)
(setq egg-buffer-hide-section-type-on-start 
      '((egg-status-buffer-mode . :help)))

;; search
(add-path "site-lisp/ack-el/")
(require 'ack)
(autoload 'pcomplete/ack "pcmpl-ack")
(autoload 'pcomplete/ack-grep "pcmpl-ack")
(load-file-in-site-lisp "smartscan.el")
(add-hook 'find-file-hook 'smartscan-mode)

;; sql
(setq sql-postgres-login-params
      '((user :default "plarochelle")
        (database :default "blurb_production")
        (server :default "oak-bi-postgres02.blurb.com")))

(add-hook 'sql-mode
          (lambda ()
            (message "in sql mode")))

;; LISP stuff
(defun use-paredit ()
  (paredit-mode +1)
  (show-paren-mode +1))
(add-hook 'emacs-lisp-mode-hook       'use-paredit)
(add-hook 'lisp-mode-hook             'use-paredit)
(add-hook 'lisp-interaction-mode-hook 'use-paredit)
(add-hook 'clojure-mode-hook          'use-paredit)
(add-hook 'cider-mode-hook            'use-paredit)
(add-hook 'cider-mode-hook            'eldoc-mode)
(add-hook 'scheme-mode-hook           'use-paredit)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook             'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook          'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook            'rainbow-delimiters-mode)
(setq scheme-program-name "/usr/local/bin/scheme")

;; org-mode
(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" . org-todo) ("STARTED" . "yellow") ("DONE" . org-done)))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . visual-line-mode))

;; Rails
;; (require 'rinari)
;; (load "/usr/share/emacs/site-lisp/nxhtml/autostart.el")
;; (setq
;;  nxhtml-global-minor-mode t
;;  mumamo-chunk-coloring 'submode-colored
;;  nxhtml-skip-welcome t
;;  indent-region-mode t
;;  rng-nxml-auto-validate-flag nil
;;  nxml-degraded t)
;; (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo-mode))

;; Ruby stuff
;;; file matchers
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))

;;; hooks
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))

;; YAML stuff
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Clojure stuff
;;;; clojure-mode
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'cider-mode-hook 'eldoc-mode)

;; java stuff
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 2)))


;; Javascript Stuff
;;; Coffee
(add-path "site-lisp/coffee-mode")
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(add-hook 'coffee-mode-hook (lambda ()
                              ;; CoffeeScript uses two spaces.
                              (make-local-variable 'tab-width)
                              (set 'tab-width 2)))

;; html
(add-path "site-lisp/html5-el")
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files "~/emacs/site-lisp/html5-el/schemas.xml"))
(require 'whattf-dt)
(add-to-list 'auto-mode-alist '("\\.html\\'" . xml-mode))

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;; handlebars
(add-path "site-lisp/handlebars-mode")
(require 'handlebars-mode)
(add-to-list 'auto-mode-alist '("emachine.*\\.html\\.erb\\'" . handlebars-mode))

;;; HAML
(add-path "site-lisp/haml-mode")
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("*\\.haml" . haml-mode))

;;; SASS mode
(require 'sass-mode)

;; Speedbar
(custom-set-variables
 '(speedbar-show-unknown-files t))
;; (when window-system          ; start speedbar if we're using a window system
;;     (speedbar t))

;;
;; ace jump mode major function
;; 
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;
;; Markdown mode
;;
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;
;; Blurb mode
;;
(require 'blurb)
(blurb-mode)

;;
;; dot extensions
;;
(defun graphviz-dot-saved-view ()
  "Runs dot on the currently-opened file and opens the resulting PNG using the 
open command"
  (interactive)
  (let ((current-file (buffer-file-name))
        (output-file (replace-regexp-in-string "\.dot$"
                                        (format ".%s" graphviz-dot-preview-extension)
                                        (buffer-file-name))))
    (let ((generate-command (format "dot %s -T%s -o %s" current-file graphviz-dot-preview-extension output-file))
          (open-command (format "open %s" output-file)))
      (message (format "running: %s" generate-command))
      (shell-command generate-command)
      (message (format "running: %s" open-command))
      (shell-command open-command))))
(add-hook 'graphviz-dot-mode-hook 'graphviz-dot-saved-view nil 'make-it-local)


;; MISC stuff
(require 'buffer-move)

(require 'sudo-save)
(require 'ido)
(eval-after-load "dash" '(dash-enable-font-lock))
(ido-mode)
(autoload 'idomenu "idomenu" nil t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching
(setq ido-case-fold  t)
(global-set-key (kbd "M-i") 'idomenu)
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)
(global-set-key (kbd "C-c o b") 'windmove-left)
(global-set-key (kbd "C-c o f") 'windmove-right)
(global-set-key (kbd "C-c o p") 'windmove-up)
(global-set-key (kbd "C-c o n") 'windmove-down)
(global-set-key (kbd "C-c o <up>") 'buf-move-up)
(global-set-key (kbd "C-c o <down>") 'buf-move-down)
(global-set-key (kbd "C-c o <left>") 'buf-move-left)
(global-set-key (kbd "C-c o <right>") 'buf-move-right)

(setq-default tab-width 2)
(setq tab-width 2)
(setq-default indent-tabs-mode nil)
(global-auto-revert-mode t)
(setq mac-option-modifier 'meta)
(setq mac-command-modifier nil)
(setq c-basic-indent 2)
(setq indent-tabs-mode nil)
(setq js-indent-level 2)
(setq next-line-add-newlines t)

(tool-bar-mode -1)
(menu-bar-mode nil)
(if (boundp 'scroll-bar-mode) (scroll-bar-mode nil))
(setq inhibit-splash-screen t)
(set-default-font "Inconsolata-17")
(global-auto-revert-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
;; own keybindings
(global-set-key (kbd "C-x /") 'comment-or-uncomment-region)

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 line-number-mode t
 column-number-mode t)

;; Add stuff to the search path
(load-file-in-site-lisp "path.el")
(load-file-in-site-lisp "auto-project.el/auto-project.el")

;; mac stuff
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)


;; extra functions
;;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(load-file-in-site-lisp "shell-utils.el")

;;; local stuff
(if (file-exists-p "~/site-lisp/local.el")
    (load-file "~/site-lisp/local.el"))
