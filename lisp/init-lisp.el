;;; package --- Summary -*- lexical-binding: t -*-
;;; code:
;;; Commentary:

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
	  (add-hook 'rust-mode-hook 'electric-pair-mode)
	  (add-hook 'c-mode-hook 'electric-pair-mode)
	  (add-hook 'c++-mode-hook 'electric-pair-mode)
	  (add-hook 'python-mode-hook 'electric-pair-mode)
	  (add-hook 'go-mode-hook 'electric-pair-mode)
	  (add-hook 'cmake-mode-hook 'electric-pair-mode)
	  (add-hook 'inferior-python-mode-hook 'electric-pair-mode)
	  )
  )

(message "loading init-lisp")
(provide 'init-lisp)

;;; init-lisp.el ends here
