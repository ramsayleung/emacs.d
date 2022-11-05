;;; package --- Summary -*- lexical-binding: t -*-
;;; code:
;;; Commentary:

;;; http://danmidwood.com/content/2014/11/21/animated-paredit.html

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init (progn
	  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
	  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
	  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
	  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
	  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
	  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
          ;;; Auto complete pair symbol, such as `()`, `{}`
	  (dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook scheme-mode-hook lisp-interaction-mode-hook python-mode-hook rust-mode-hook c++-mode-hook))
 	    (add-hook hook 'electric-pair-mode))
	  )
  )

(message "loading init-lisp")
(provide 'init-lisp)

;;; init-lisp.el ends here
