;;; init-useless.el --- Useless function record      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Samray, Leung

;; Author: Samray, Leung <samrayleung@gmail.com>
;; Keywords: lisp, 


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
(defvar require-exclude-list '())
(if (samray/does-use-ivy)
    (add-to-list 'require-exclude-list 'init-helm)
  (add-to-list 'require-exclude-list 'init-ivy))
;; Benchmark loading time file by file and display it in the *Messages* buffer
(defun samray/startup-benchmark ()
  (when init-file-debug
    (require 'benchmark))

  (let ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
	(manual-add-packages (expand-file-name "additional-packages" user-emacs-directory))
	(total-load-time 0.0))
    (add-to-list 'load-path lisp-dir)
    (add-to-list 'load-path manual-add-packages)
    (mapc (lambda (fname)
            (let ((feat (intern (file-name-base fname))))
	      (unless (member feat require-exclude-list)
		(if init-file-debug
		    (progn
		      (let ((feat-load-time (benchmark-elapse (require feat fname))))
			(message "Feature '%s' loaded in %.2fs" feat feat-load-time)
			(setq total-load-time (+ total-load-time feat-load-time))))
		  (require feat fname)))))
          (directory-files lisp-dir t "\\.el"))
    (message "All Feature loaded in %.2fs" total-load-time))
  )
