;;; package --- Summary
;;; Code:
;;; Commentary:
(provide 'init-lsp)
(use-package lsp-mode
  :ensure t
  :init (progn
	  (dolist (hook '(rust-mode-hook python-mode-hook))
	    (add-hook hook 'lsp-mode))
	  ))
(use-package lsp-python
  :ensure t
  )

(use-package lsp-rust
  :ensure t)
;;; init-lsp.el ends here
