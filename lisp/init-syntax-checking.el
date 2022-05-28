;;; package --- Summary:
;;; Commentary:
;;; Code:

(use-package flycheck
  :ensure t
  :demand t
  :config(progn
	   (setq flycheck-mode-line-prefix "flyc")
	   (global-flycheck-mode t)))

(use-package flycheck-rust
  :ensure t
  :defer t
  :init (progn
	  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
	  ))

(add-hook 'c++-mode-hook (lambda ()
			   (setq flycheck-clang-language-standard "c++17")
			   (setq flycheck-gcc-language-standard "c++17")
			   ))

(message "loading init-syntax-checking")
(provide 'init-syntax-checking)
;;; init-syntax-checking.el ends here
