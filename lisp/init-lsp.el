;;; init-lsp.el ---                                  -*- lexical-binding: t; -*-

;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package lsp-mode
  :ensure t
  :defer t
  :config (progn
	    (require 'lsp-flycheck)
	    (add-hook 'prog-mode-hook 'lsp-mode)
	    ))
(use-package lsp-python
  :ensure t
  :defer t
  :config(progn
	   (add-hook 'python-mode-hook #'lsp-python-enable)
	   ))
(use-package lsp-rust
  :ensure t
  :defer t
  :config (progn
	    (add-hook 'rust-mode-hook #'lsp-rust-enable)
	    (add-hook 'rust-mode-hook #'flycheck-mode)
	    (setq lsp-rust-rls-command '("rustup" "run" "nightly-2017-12-21" "rls"))
	    ))
(provide 'init-lsp)
;;; init-lsp.el ends here
