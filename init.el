;;; package --- Summary
;;; Commentary:
;;; Code:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; cl - Common Lisp Extension

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; Trick to reduce startup time:
;; Increase the garbage collection threshold to 500 MB to ease startup

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.


(setq gc-cons-threshold (* 128 1024 1024))
(let ((file-name-handler-alist nil))
  (package-initialize)
  (require 'package)
  (setq package-enable-at-startup nil)
  ;; For God' sake,there are a lot of site you cannot reach in China
  ;; this is mirror of melpa and gnu
  (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			   ("melpa" . "http://elpa.emacs-china.org/melpa/")))

  ;; Display the total loading time in the minibuffer
  (defun display-startup-echo-area-message ()
    "Display startup echo area message."
    (message "Initialized in %s" (emacs-init-time)))

  ;; Benchmark loading time file by file and display it in the *Messages* buffer

  (when init-file-debug
    (require 'benchmark))

  (let ((lisp-dir "~/.emacs.d/elisp"))
    (add-to-list 'load-path lisp-dir)
    (mapc (lambda (fname)
	    (let ((feat (intern (file-name-base fname))))
	      (if init-file-debug
		  (message "Feature '%s' loaded in %.2fs" feat
			   (benchmark-elapse (require feat fname)))
		(require feat fname))))
	  (directory-files lisp-dir t "\\.el")))

  (require 'cl)
  ;; Bootstrap `use-package'
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'init-auto-completion)
  (require 'init-better-editing)
  (require 'init-chinese)
  (require 'init-elisp)
  (require 'init-evil)
  (require 'init-ivy)
  (require 'init-keybindings)
  (require 'init-markdown)
  (require 'init-misc)
  (require 'init-org)
  (require 'init-programming)
  (require 'init-python)
  (require 'init-scheme)
  (require 'init-syntax-checking)
  (require 'init-ui)
  (require 'init-version-control)
  (require 'init-web)
  (setq custom-file (expand-file-name "elisp/custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))
  )
;;; Garbage collector - decrease threshold to 5 MB
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))
(provide 'init)
;;; init.el ends here
