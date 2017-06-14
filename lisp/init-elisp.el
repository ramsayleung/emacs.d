;;; package --- Summary
;;; code:
;;; Commentary:

(use-package paredit
  :ensure t
  :defer t
  :init (progn
	  (dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook scheme-mode-hook))
	    (add-hook hook 'enable-paredit-mode))
	  )
  :diminish (paredit-mode . "π")
  )
(provide 'init-elisp)

;;; init-elisp.el ends here
