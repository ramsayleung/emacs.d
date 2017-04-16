;;; package --- summary
;;; code:
;;; commentary:

(use-package geiser
  :ensure t
  :defer t
  :init (progn
	  (add-hook 'scheme-mode-hook 'geiser-mode)
	  ;; (add-hook 'geiser-mode-hook 'samray/scheme-run-repl-for-code-complete-startup)
	  )
  :config (progn
	    (setq geiser-default-implementation 'guile)
	    ))

;;; steal from http://www.yinwang.org/blog-cn/2013/04/11/scheme-setup
(require 'cmuscheme)
(setq scheme-program-name "petite")         ;; "petite" "racket"

;; bypass the interactive question and start the default interpreter
(defun samray/scheme-proc ()
  "Return the current Scheme process, starting one if necessary."
  (unless (and scheme-buffer
	       (get-buffer scheme-buffer)
	       (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name)))
  (or (scheme-get-process)
      (error "No current process. See variable `scheme-buffer'")))

(defun samray/scheme-split-window ()
  (cond
   ((= 1 (count-windows))
    (delete-other-windows)
    (split-window-vertically (floor (* 0.68 (window-height))))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window 1))
   ((not (find "*scheme*"
	       (mapcar (lambda (w) (buffer-name (window-buffer w)))
		       (window-list))
	       :test 'equal))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window -1))))

(defun samray/scheme-send-last-sexp-split-window ()
  (interactive)
  (samray/scheme-split-window)
  (scheme-send-last-sexp))

(defun sarmay/scheme-send-definition-split-window ()
  (interactive)
  (samray/scheme-split-window)
  (scheme-send-definition))

(add-hook 'scheme-mode-hook
	  (lambda ()
	    (paredit-mode 1)
	    (define-key scheme-mode-map (kbd "<f8>") 'samray/scheme-send-last-sexp-split-window)
	    (define-key scheme-mode-map (kbd "<f9>") 'samray/scheme-send-definition-split-window)))

(defun samray/scheme-run-repl-for-code-complete-startup ()
  "Run scheme repl for code auto-complete in the startup."
  (interactive)
  (if (get-buffer "* Guile REPL *") (message "Guile Repl has launched")
    (save-excursion
      (progn
	(run-guile)
	(delete-window)
	)))
  )

(provide 'init-scheme)

;;; init-scheme.el ends here
