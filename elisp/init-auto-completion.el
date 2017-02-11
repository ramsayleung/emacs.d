;;; package --- Summary:
;;; Commentary:
;;; Code:
(use-package company
  :ensure t
  :config(global-company-mode t))

(use-package company-quickhelp
  :ensure t
  :config (progn
	    (add-hook 'company-mode-hook 'company-quickhelp-mode)
	    ))

(use-package company-statistics
  :ensure t
  :config (add-hook 'company-mode-hook 'company-statistics-mode))

(use-package company-anaconda
  :ensure t
  :config (progn
	    (eval-after-load "company"
	      '(add-to-list 'company-backends 'company-anaconda))
	    )
  )
(provide 'init-auto-completion)
;;; init-auto-completion.el ends here
