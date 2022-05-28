;;; init-lsp.el ---                                  -*- lexical-binding: t; -*-


;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c r")
  :hook ((rust-mode . lsp)
	 (c-mode . lsp)
	 (c++-mode . lsp)
	 (shell-mode . lsp)
	 (js-mode . lsp)
	 (php-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-lens-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  :commands lsp)

(message "loading init-lsp")
(provide 'init-lsp)
;;; init-lsp.el ends here
