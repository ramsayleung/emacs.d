;;; init-lsp.el ---                                  -*- lexical-binding: t; -*-


;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package eglot
  :ensure t
  :hook ((python-mode . eglot-ensure)
	 (python-ts-mode . eglot-ensure)
	 (ruby-mode . eglot-ensure)
	 (ruby-ts-mode . eglot-ensure)
	 (rust-mode . eglot-ensure)
	 (rust-ts-mode . eglot-ensure)
	 (shell-mode . eglot-ensure)
	 (javascript-mode . eglot-ensure)
	 (js-ts-mode . eglot-ensure)
	 (c++-mode . eglot-ensure)
	 (c++-ts-mode . eglot-ensure)
	 (go-mode . eglot-ensure)
	 (go-ts-mode . eglot-ensure)
	 (c-mode . eglot-ensure)
	 (c-ts-mode . eglot-ensure))
  )



(message "loading init-lsp")
(provide 'init-lsp)
;;; init-lsp.el ends here
