;; package --- Summary
;;; Code:
;;; Commentary:
(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :init (progn
	  (add-hook 'rust-mode-hook '(lambda ()
				       ;; Hook in racer with eldoc to provide documentation
				       (racer-turn-on-eldoc)
				       ;; if dumb-jump-mode enable, just disable it
				       (if dumb-jump-mode
					   (dumb-jump-mode))
				       ))
	  )
  )

(use-package racer
  :ensure t
  :defer t
  :init (progn
	  ;; Set path to racer binary
	  (setq racer-cmd (expand-file-name "~/.cargo/bin/racer"))
	  ;; Set path to rust src directory
	  (setq racer-rust-src-path  (getenv "RUST_SRC_PATH"))
	  (add-hook 'rust-mode-hook 'racer-mode)
	  (add-hook 'racer-mode-hook 'eldoc-mode)
	  )
  )
(provide 'init-rust)
;;; init-rust.el ends here
