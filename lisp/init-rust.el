;; package --- Summary
;;; Code:
;;; Commentary:
(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  )

(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'" . toml-mode))
(use-package racer
  :ensure t
  :init (progn
	  ;; Set path to racer binary
	  (setq racer-cmd (expand-file-name "~/.cargo/bin/racer"))
	  ;; Set path to rust src directory
	  (setq racer-rust-src-path  (getenv "RUST_SRC_PATH"))
  	  (add-hook 'rust-mode-hook 'racer-mode)
	  (add-hook 'racer-mode-hook 'eldoc-mode)
	  )
  )
(use-package cargo
  :ensure t
  :defer t
  :init (progn
	  (add-hook 'rust-mode-hook 'cargo-minor-mode)))
(provide 'init-rust)
;;; init-rust.el ends here
