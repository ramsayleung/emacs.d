
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

;;; Define constnat variable for configuration

(defvar samray-additional-packages-path (expand-file-name "additional-packages" user-emacs-directory))

(defvar samray/completion-framework 'ivy)

;;; Define some useful function
(defun samray/mac-os-p ()
  "Check whether Emacs is running on Mac os."
  (string= system-type "darwin")
  )
(defun samray/linux-p ()
  "Check whether Emacs is running on Linux."
  (string= system-type "gnu/linux"))
(defun samray/windows-p ()
  "Detect whether Emacs is running on Windows."
  (eq system-type 'windows-nt))

(defun samray/buffer-too-big-p ()
  "Predicate if buffer is too big."
  (or (> (buffer-size) (* 5000 80))
      (> (line-number-at-pos (point-max)) 50000)))

(defun samray/buffer-too-large-p ()
  "Predicate if buffer is too large."
  (or (> (buffer-size) (* 5000 800))
      (> (line-number-at-pos (point-max)) 100000)))

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defun samray/does-use-ivy ()
  "Return t if use `ivy` as completion framework."
  (if (eq samray/completion-framework 'ivy) t nil))

(setq gc-cons-threshold (* 128 1024 1024))
(let ((file-name-handler-alist nil))
  (setq load-prefer-newer t)            ;avoid load outdated byte-compiled file
  (require 'package)
  (when (version< emacs-version "27.0") (package-initialize))
  ;;(unless package--initialized (package-initialize))

  (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
  			   ("melpa" . "http://elpa.emacs-china.org/melpa/")))

  (setq package-enable-at-startup nil)
  ;; Bootstrap `use-package'
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "additional-packages" user-emacs-directory))

  (require 'cl)
  (require 'init-auto-completion)
  (require 'init-better-editing)
  (require 'init-better-default)
  (require 'init-chinese)
  (require 'init-c-c++)
  (require 'init-dired)
  (require 'init-eshell)
  (require 'init-evil)
  (require 'init-go)
  (if (samray/does-use-ivy)
      (progn
	(message "Use ivy as completion framework")
	(require 'init-ivy))
    (progn
      (message "Use helm as completion framework")
      (require 'init-helm))
    )
  (require 'init-keybindings)
  (require 'init-lisp)
  (require 'init-lsp)
  (require 'init-markdown)
  (require 'init-misc)
  (require 'init-org)
  (require 'init-python)
  (require 'init-programming)
  (require 'init-rust)
  (require 'init-scheme)
  (require 'init-syntax-checking)
  (require 'init-ui)
  (require 'init-version-control)
  (require 'init-web)

  (setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (run-with-idle-timer 1 nil 'load custom-file)
    )
  ;; Display the total loading time in the minibuffer
  (defun display-startup-echo-area-message ()
    "Display startup echo area message."
    (message "Initialized in %s" (emacs-init-time)))
  )
;;; Garbage collector - decrease threshold to 5 MB
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))
(provide 'init)
;;; init.el ends here
