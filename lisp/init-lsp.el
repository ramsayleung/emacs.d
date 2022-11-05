;;; init-lsp.el ---                                  -*- lexical-binding: t; -*-


;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package eglot
  :ensure t
  :init
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
		 ))
    (add-hook hook '(lambda () (eglot-ensure))))
  )



(message "loading init-lsp")
(provide 'init-lsp)
;;; init-lsp.el ends here
