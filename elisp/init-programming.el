;;; package --- Summary
;;; code:
;;; Commentary:
(provide 'init-programming)
(use-package yasnippet
  :ensure t
  :config (progn
	    (add-hook 'prog-mode-hook #'yas-minor-mode)
	    (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
	    ))
;;; init.programming.el ends here
