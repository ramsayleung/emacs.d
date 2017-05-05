;;; package --- Summary:
;;; Commentary:
;;; Code:
(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :config (progn
	    ))

(use-package go-eldoc
  :ensure t
  :defer t
  :config (progn
	    (add-hook 'go-mode-hook 'go-eldoc-setup)
	    ))
(provide 'init-go)
;;; init-go.el ends here
