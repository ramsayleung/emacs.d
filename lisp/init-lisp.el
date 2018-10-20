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
	  (defvar samray/electic-pair-modes '(emacs-lisp-mode lisp-mode scheme-mode lisp-interaction-mode))
	  (defun samray/inhibit-electric-pair-mode (char)
	    (member major-mode samray/electic-pair-modes))
	  (setq electric-pair-inhibit-predicate #'samray/inhibit-electric-pair-mode)
	  (add-hook 'prog-mode-hook 'electric-pair-mode)
	  )
  )

(message "loading init-lisp")
(provide 'init-lisp)

;;; init-lisp.el ends here
