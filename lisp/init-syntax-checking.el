;;; package --- Summary:
;;; Commentary:
;;; Code:

(use-package flycheck
  :ensure t
  :demand t
  :config(progn
	   (setq flycheck-mode-line-prefix "FC")
	   (global-flycheck-mode t)))

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
	  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
	  ))

(add-hook 'c++-mode-hook (lambda ()
			   (setq flycheck-clang-language-standard "c++14")
			   (setq irony-additional-clang-options '("-std=c++14"))
			   ))
(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config (progn
	    (set-face-attribute 'flycheck-posframe-warning-face nil
				:inherit nil
				:stipple nil
				:background "black"
				:foreground "yellow")


	    (set-face-attribute 'flycheck-posframe-error-face nil
				:inherit nil
				:stipple nil
				:background "black"
				:foreground "brown")
	    (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
	    )
  )

(message "loading init-syntax-checking")
(provide 'init-syntax-checking)
;;; init-syntax-checking.el ends here
