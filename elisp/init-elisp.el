;;; package --- Summary
;;; code:
;;; Commentary:
(use-package paredit
  :ensure t
  :config (progn
	    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
	    (add-hook 'lisp-mode-hook 'enable-paredit-mode)
	    (add-hook 'scheme-mode-hook 'enable-paredit-mode)
	    ))
(provide 'init-elisp)
;;; init-elisp.el ends here
