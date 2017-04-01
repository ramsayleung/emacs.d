;;; package --- Summary:
;;; Commentary:
;;; Code:
(use-package company
  :ensure t
  :diminish (company-mode . "Φ")
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
	 company-global-modes '(not org-mode)
	 company-minimum-prefix-length 2
	 company-require-match 0
	 company-selection-wrap-around t
	 company-dabbrev-downcase nil
	 company-tooltip-limit 20                      ; bigger popup window
	 company-tooltip-align-annotations 't          ; align annotations to the right tooltip border
	 company-idle-delay .4                         ; decrease delay before autocompletion popup shows
	 company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  
  :config (progn
	    (global-company-mode t))
  ;; fix the issue that company is not compatible with fci-mode 
  (defvar-local company-fci-mode-on-p nil)
  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))
  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))
  (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
  )

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
		  (lambda () (add-to-list 'company-backends 'company-web-html))))

;;; Shell Script completion
(use-package company-shell
  :ensure t
  :defer t
  :init (add-hook 'sh-mode-hook
		  (lambda () (add-to-list 'company-backends 'company-shell))))

;;; C/C++ headers completion
(use-package company-c-headers
  :ensure t
  :defer t
  :init (progn (add-hook 'c-mode-hook
			 (lambda () (add-to-list 'company-backends 'company-c-headers))
			 )
	       (add-hook 'c++-mode-hook
			 (lambda () (add-to-list 'company-backends 'company-c-headers)))
	       )
  )
;;; backends for tern
(use-package company-tern
  :ensure t
  :defer t
  :init (progn
          (add-hook 'js2-mode-hook
                    (lambda () (add-to-list 'company-backends 'company-tern)))
          (add-hook 'js-mode-hook
                    (lambda () (add-to-list 'company-backends 'company-tern)))
          ))
(provide 'init-auto-completion)
;;; init-auto-completion.el ends here
