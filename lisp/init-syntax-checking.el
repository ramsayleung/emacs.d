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

(use-package flycheck-gometalinter
  :ensure t
  :config
  (progn
    (flycheck-gometalinter-setup)
    ;; skips 'vendor' directories and sets GO15VENDOREXPERIMENT=1
    (setq flycheck-gometalinter-vendor t)
    ;; only show errors
    (setq flycheck-gometalinter-errors-only t)
    ;; only run fast linters
    (setq flycheck-gometalinter-fast t)
    ;; use in tests files
    (setq flycheck-gometalinter-test t)
    ;; disable linters
    (setq flycheck-gometalinter-disable-linters '("gotype" "gocyclo"))
    ;; Only enable selected linters
    (setq flycheck-gometalinter-disable-all t)
    (setq flycheck-gometalinter-enable-linters '("golint"))
    ;; Set different deadline (default: 5s)
    (setq flycheck-gometalinter-deadline "10s")))

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
