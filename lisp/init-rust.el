;;; package --- Summary
;;; Code:
;;; Commentary:
(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  )
(use-package racer
  :ensure t
  :defer t
  :init (progn
	  (add-hook 'rust-mode-hook #'racer-mode)
	  (add-hook 'racer-mode-hook #'eldoc-mode)
	  ))
(provide 'init-rust)
;;; init-rust.el ends here
