;;; init-lsp.el ---                                  -*- lexical-binding: t; -*-


;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package eglot
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'eglot-ensure)
  )



(message "loading init-lsp")
(provide 'init-lsp)
;;; init-lsp.el ends here
