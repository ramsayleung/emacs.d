;;; package --- Summary
;;; Code:
;;; Commentary:
(provide 'init-lsp)
(use-package lsp-mode
  :ensure t
  :init (progn
	  (add-hook 'prog-mode-hook 'lsp-mode)
	  ))
(use-package lsp-python
  :ensure t
  )

(use-package lsp-rust
  :ensure t)
;;; init-lsp.el ends here
