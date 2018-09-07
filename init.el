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

(defvar samray/completion-framework 'helm)
(defun samray/does-use-ivy ()
  "Return t if use `ivy` as completion framework."
  (if (eq samray/completion-framework 'ivy) t nil))

(setq gc-cons-threshold (* 128 1024 1024))
(let ((file-name-handler-alist nil))
  (setq load-prefer-newer t)            ;avoid load outdated byte-compiled file
  (package-initialize)
  (require 'package)
  (setq package-enable-at-startup nil)

  (defconst samray/query-public-ip-url "http://myexternalip.com/raw")

  (defun samray/send-get-request (api-url)
    "Send get request, return JSON as an alist if successes."
    (let (json)
      (with-current-buffer (url-retrieve-synchronously api-url)
	(set-buffer-multibyte t)
	(goto-char (point-min))
	(when (not (string-match "200 OK" (buffer-string)))
	  (error "Problem connecting to the server"))
	;; (message (buffer-string))
	(re-search-forward "^$" )
	(setq json
	      (buffer-substring-no-properties (point) (point-max)))
	(kill-buffer (current-buffer)))
      json))
  (defun samray/setup-query-ip ()
    (let* ((query-ip-result (samray/send-get-request samray/query-public-ip-url))
	   (public-ip (samray/remove-eof query-ip-result )))
      (concat "http://ipapi.co/"public-ip"/country/"))
    )

  (defun samray/remove-eof (string-with-eof)
    (replace-regexp-in-string "\n" "" string-with-eof))

  (defun samray/ip-from-china-p ()
    (let* ((query-result (samray/send-get-request (samray/setup-query-ip)))
	   (ip-location (samray/remove-eof query-result)))
      (string= "CN" ip-location)
      ))
  (defun samray/setup-melpa ()
    (if t
	;; It takes too much time to send and receive request, so it slows donw
	;; startup. Improve it unitl emacs 26(multithread).
	;; (if (samray/ip-from-china-p)
	(progn
  	  (message "There is a wall here.")
  	  ;; For God' sake, there are a lot of site you cannot reach in China
  	  ;; this is mirror of melpa and gnu
  	  (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
  				   ("melpa" . "http://elpa.emacs-china.org/melpa/")))
  	  )
      (progn
	(message "You are free.")
	(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
				 ("marmalade" . "https://marmalade-repo.org/packages/")
				 ("melpa" . "https://melpa.org/packages/")))
	))
    )

  (samray/setup-melpa)
  ;; Bootstrap `use-package'
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  ;; Display the total loading time in the minibuffer
  (defun display-startup-echo-area-message ()
    "Display startup echo area message."
    (message "Initialized in %s" (emacs-init-time)))

  ;; Benchmark loading time file by file and display it in the *Messages* buffer

  (when init-file-debug
    (require 'benchmark))

  (let ((lisp-dir "~/.emacs.d/lisp")
        (manual-add-packages "~/.emacs.d/additional-packages")
        )
    (add-to-list 'load-path lisp-dir)
    (add-to-list 'load-path manual-add-packages)
    )

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
    (load custom-file))
  )
;;; Garbage collector - decrease threshold to 5 MB
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))
(provide 'init)
;;; init.el ends here
