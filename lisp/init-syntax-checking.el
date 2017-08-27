;;; package --- Summary:
;;; Commentary:
;;; Code:

(use-package flycheck
  :ensure t
  :demand t
  :diminish (flycheck-mode . "ψ")
  :config(global-flycheck-mode t))

(use-package flycheck-rust
  :ensure t
  :defer t
  :init (progn
	  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
	  ))
(use-package flycheck-irony
  :ensure t
  :defer t
  :init (progn
	  (setq irony-additional-clang-options '("-std=c++11"))
	  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
	  ))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
(provide 'init-syntax-checking)
;;; init-syntax-checking.el ends here
