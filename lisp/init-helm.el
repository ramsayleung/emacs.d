;;; package --- Summary
;;; code:
;;; Commentary:

(use-package helm
  :ensure t
  :diminish (helm-mode . " H")
  :config (progn
	    ;; enable helm globally
	    (helm-mode 1)
	    ;; Extra helm configs
	    ;; Use fuzzy match in helm
	    (setq helm-M-x-fuzzy-match t)
	    (setq helm-buffers-fuzzy-matching t)
	    (setq helm-recentf-fuzzy-match t)
	    ;; Make helm can select anything even not match
	    (setq helm-move-to-line-cycle-in-source nil)
	    (setq helm-ff-search-library-in-sexp t)
	    (setq helm-ff-file-name-history-use-recentf t)
	    (setq helm-mini-default-sources '(helm-source-buffers-list
					      helm-source-recentf
					      helm-source-bookmarks
					      helm-source-buffer-not-found))
	    )
  )
(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config
  ;; make projectile use helm as completion system
  (setq projectile-completion-system 'helm)
  ;; start helm-projectile
  (helm-projectile-on))
(use-package helm-swoop
  :ensure t
  )
(use-package helm-rg
  :ensure t
  :config (progn
	    (custom-set-variables
	     '(helm-ag-base-command "rg --no-heading"))
	    ))

(provide 'init-helm)

;;; init-helm.el ends here
