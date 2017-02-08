;;; package --- Summary
;;; code:
;;; Commentary:

(use-package window-numbering
  :config(progn
	   (window-numbering-mode t)))
(use-package smartparens
  :config(smartparens-global-mode t))
(use-package hungry-delete
  :config(hungry-delete-mode t))
(provide 'init-better-editing)
;;; init-better-editing.el ends here
