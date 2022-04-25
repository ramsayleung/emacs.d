;;; init-lsp.el ---                                  -*- lexical-binding: t; -*-


;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package nox
  :load-path "~/.emacs.d/additional-packages/nox.el"
  :config
  (dolist (hook (list
		 'js-mode-hook
		 'rust-mode-hook
		 'python-mode-hook
		 'java-mode-hook
		 'sh-mode-hook
		 'php-mode-hook
		 'c-mode-common-hook
		 'c-mode-hook
		 'c++-mode-hook
		 'prog-mode-hook
		 ))
    (add-hook hook '(lambda () (nox-ensure))))
  (setq nox-python-server "pyright")
  (add-to-list 'nox-server-programs
               '(rust-mode . ("rust-analyzer"))
	       '(c++-mode . ("clangd")))
  )

(message "loading init-lsp")
(provide 'init-lsp)
;;; init-lsp.el ends here
