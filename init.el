
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
(message "Start to load init.el")

;;; Define some useful function
(defun ramsay/mac-os-p ()
  "Check whether Emacs is running on Mac os."
  (string= system-type "darwin")
  )

(defun ramsay/linux-p ()
  "Check whether Emacs is running on Linux."
  (string= system-type "gnu/linux"))

(defun ramsay/windows-p ()
  "Detect whether Emacs is running on Windows."
  (eq system-type 'windows-nt))

(defun ramsay/intercepted-by-gfw-p (&optional host)
  "Check if intercepted by GFW with HOST."
  (not (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
			  (if host host "www.google.com")))))

(if (ramsay/intercepted-by-gfw-p)
    ;; Use mirror if it's in China.
    (setq package-archives '(("gnu"   . "http://1.15.88.122/gnu/")
                             ("melpa" . "http://1.15.88.122/melpa/")))
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/"))))

(setq gc-cons-threshold (* 128 1024 1024))
;;; Prompt Emacs to GC whenever it loses focus
(add-function :after
              after-focus-change-function
              (lambda () (unless (frame-focus-state) (garbage-collect))))

(require 'package)
(unless package--initialized (package-initialize))
(setq load-prefer-newer t)            ;avoid load outdated byte-compiled file

(setq package-enable-at-startup nil)
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "additional-packages" user-emacs-directory))

(require 'init-auto-completion)
(require 'init-better-editing)
(require 'init-better-default)
(require 'init-chinese)
(require 'init-c-c++)
(require 'init-dired)
(require 'init-eshell)
(require 'init-evil)
(require 'init-go)
(require 'init-ivy)
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
  (load custom-file))
;; Display the total loading time in the minibuffer

(defun display-startup-echo-area-message ()
  "Display startup echo area message."
  (message "Initialized in %s" (emacs-init-time)))

(provide 'init)
;;; init.el ends here
