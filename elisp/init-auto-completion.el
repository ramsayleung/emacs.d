;;; package --- Summary:
;;; Commentary:
;;; Code:
(use-package company
  :ensure t
  :demand t
  :commands (company-mode
             company-complete
             company-complete-common
             company-complete-common-or-cycle
             company-files
             company-dabbrev
             company-jedi
             company-tern
             company-web-html
             )
  :init (setq
	 company-minimum-prefix-length 2
	 company-require-match 0
	 company-selection-wrap-around t
	 company-dabbrev-downcase nil
	 company-tooltip-limit 20                      ; bigger popup window
	 company-tooltip-align-annotations 't          ; align annotations to the right tooltip border
	 company-idle-delay .4                         ; decrease delay before autocompletion popup shows
	 company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  
  :config(global-company-mode t))

(use-package company-quickhelp
  :ensure t
  :defer t
  :init (progn
	  (add-hook 'company-mode-hook 'company-quickhelp-mode)
	  ))

(use-package company-statistics
  :ensure t
  :defer t
  :init (add-hook 'company-mode-hook 'company-statistics-mode))

(use-package company-anaconda
  :ensure t
  :defer t
  :init(progn
	 (add-hook 'python-mode-hook
		   (lambda()(add-to-list 'company-backends 'company-anaconda)))
	 ))

;; company-mode completion back-end for python JEDI
(use-package company-jedi
  :ensure t
  :defer t
  :init(progn
	 (add-hook 'python-mode-hook
		   (lambda()(add-to-list 'company-backends 'company-jedi)))
	 ))
;; HTML completion
(use-package company-web
  :ensure t
  :defer t
  :init (add-hook 'web-mode-hook
		  (lambda () (add-to-list 'company-backend 'company-web-html))))
(provide 'init-auto-completion)
;;; init-auto-completion.el ends here
