;;; package --- Summary
;;; Code:
;;; Commentary:
(provide 'init-lsp)
(use-package lsp-mode
  :ensure t
  :defer t
  :init (progn
	  (dolist (hook '( python-mode-hook))
	    (add-hook hook 'lsp-mode))
	  ))
(use-package lsp-python
  :ensure t
  :defer t
  :init (progn
	  (add-hook 'python-mode-hook (lambda ()
					(require 'lsp-python))))
  )

;;(use-package lsp-rust
;;  :ensure t
;;  :defer t
;;  :init (progn
;;	  (add-hook 'rust-mode-hook (lambda ()
;;				      (require 'lsp-rust)))))
;;; init-lsp.el ends here

