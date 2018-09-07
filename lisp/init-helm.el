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
	    (defun samray/helm-find-files-navigate-back (orig-fun &rest args)
	      "Make helm-find-files ORIG-FUN with ARGS backspace as like Ivy."
	      (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
		  (helm-find-files-up-one-level 1)
		(apply orig-fun args)))
	    (advice-add 'helm-ff-delete-char-backward :around #'samray/helm-find-files-navigate-back)
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
  :config (progn
	    (setq helm-swoop-speed-or-color t)
	    )
  )
(use-package helm-ag
  :ensure t
  :config (progn
	    (custom-set-variables
	     '(helm-ag-base-command "rg --no-heading"))
	    ))

(use-package helm-xref
  :ensure t
  :config (progn
	    (setq xref-show-xrefs-function 'helm-xref-show-xrefs)
	    ))

(provide 'init-helm)

;;; init-helm.el ends here
