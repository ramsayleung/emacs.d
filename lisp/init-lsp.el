;;; init-lsp.el ---                                  -*- lexical-binding: t; -*-

;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package lsp-mode
  :ensure t
  :config (progn
	    (require 'lsp-flycheck)
	    ))
;; (use-package lsp-python
;;   :ensure t
;;   :config(progn
;; 	   (add-hook 'python-mode-hook #'lsp-python-enable)
;; 	   ))
(use-package lsp-rust
  :ensure t
  :config (progn
	    (add-hook 'rust-mode-hook #'lsp-rust-enable)
	    (add-hook 'rust-mode-hook #'flycheck-mode)
	    (setq lsp-rust-rls-command '("rustup" "run" "nightly-2017-12-21" "rls"))
	    ))
(provide 'init-lsp)
;;; init-lsp.el ends here
