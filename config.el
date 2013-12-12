(defvar emacs-root "~/emacs/")
(defun add-path (p)
  (add-to-list 'load-path
	       (concat emacs-root p)))

(add-path "site-lisp/")
(add-path "site-lisp/rinari")

;; autocomplete
;(add-path "site-lisp/auto-complete/")
;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "site-lisp/auto-complete//ac-dict")
;(ac-config-default)

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

;; LISP stuff
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(defun use-paredit ()
  (paredit-mode +1)
  (show-paren-mode +1))
(add-hook 'emacs-lisp-mode-hook       'use-paredit)
(add-hook 'lisp-mode-hook             'use-paredit)
(add-hook 'lisp-interaction-mode-hook 'use-paredit)
(add-hook 'clojure-mode-hook          'use-paredit)
(add-hook 'scheme-mode-hook           'use-paredit)
(setq scheme-program-name "/usr/local/bin/scheme")

;; org-mode
(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" . org-todo) ("STARTED" . "yellow") ("DONE" . org-done)))

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
(add-path "site-lisp/clojure-mode")
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(require 'nrepl)
(add-hook 'nrepl-mode-hook 'paredit-mode)


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

;; CSS
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(require 'less-css-mode)

;; html
(add-path "site-lisp/html5-el")
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files "~/emacs/site-lisp/html5-el/schemas.xml"))
(require 'whattf-dt)
(add-to-list 'auto-mode-alist '("\\.html\\'" . xml-mode))


;;; handlebars
(add-path "site-lisp/handlebars-mode")
(require 'handlebars-mode)
(add-to-list 'auto-mode-alist '("emachine.*\\.html\\.erb\\'" . handlebars-mode))

;;; HAML
(add-path "site-lisp/haml-mode")
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("*\\.haml" . haml-mode))
             

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


;; MISC stuff
(require 'sudo-save)
(require 'ido)
(ido-mode)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching
(setq ido-case-fold  t)
(setq-default tab-width 2)
(setq tab-width 2)
(setq-default indent-tabs-mode nil)
(global-auto-revert-mode t)
(setq mac-option-modifier 'meta)
(setq mac-command-modifier nil)
(setq c-basic-indent 2)
(setq indent-tabs-mode nil)
(setq js-indent-level 2)

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
(setenv "PATH" (concat "/usr/local/bin" path-separator (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; mac stuff
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)


;; extra functions
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
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



;;; local stuff
(if (file-exists-p "~/site-lisp/local.el")
    (load-file "~/site-lisp/local.el"))
