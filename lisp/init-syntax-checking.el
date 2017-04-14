;;; package --- Summary:
;;; Commentary:
;;; Code:

(use-package flycheck
  :ensure t
  :demand t
  :diminish (flycheck-mode . "ψ")
  :config(global-flycheck-mode t))

(provide 'init-syntax-checking)
;;; init-syntax-checking.el ends here
