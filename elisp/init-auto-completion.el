;;; package --- Summary:
;;; Commentary:
;;; Code:
;; (use-package company
;;   :ensure t
;;   :config(global-company-mode t))
(use-package auto-complete
  :ensure t
  :config(progn
	   (ac-config-default)
	   (auto-complete-mode t)
	   ))
(provide 'init-auto-completion)
;;; init-company.el ends here
