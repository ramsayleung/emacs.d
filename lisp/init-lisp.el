;;; package --- Summary -*- lexical-binding: t -*-
;;; code:
;;; Commentary:

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init (progn
	  (add-hook 'prog-mode-hook 'enable-paredit-mode)
	  )
  :config (progn
	    ;; Don't add a space before parentheses for non-lisp buffer.
	    (defun paredit/space-for-delimiter-p (endp delm)
	      (not (derived-mode-p 'basic-mode
				   'c++-mode
				   'c-mode
				   'cmake-mode
				   'coffee-mode
				   'csharp-mode
				   'd-mode
				   'dart-mode
				   'go-mode
				   'java-mode
				   'js-mode
				   'lua-mode
				   'objc-mode
				   'pascal-mode
				   'r-mode
				   'python-mode
				   'ruby-mode
				   'rust-mode
				   'typescript-mode)))
	    (add-to-list 'paredit-space-for-delimiter-predicates 'paredit/space-for-delimiter-p)
	    )
  )

(message "loading init-lisp")
(provide 'init-lisp)

;;; init-lisp.el ends here
