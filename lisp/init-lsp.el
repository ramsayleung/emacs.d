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
	 (js-mode . eglot-ensure)
	 (js-ts-mode . eglot-ensure)
	 (c++-mode . eglot-ensure)
	 (c++-ts-mode . eglot-ensure)
	 (go-mode . eglot-ensure)
	 (go-ts-mode . eglot-ensure)
	 (c-mode . eglot-ensure)
	 (c-ts-mode . eglot-ensure))
  )


(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package eglot-booster
  :disable
  :init (ramsay/vc-install :fetcher "github" :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode)
  ;; eglot booster doesn't work well with python project with virtual environment
  )

(message "loading init-lsp")
(provide 'init-lsp)
;;; init-lsp.el ends here
