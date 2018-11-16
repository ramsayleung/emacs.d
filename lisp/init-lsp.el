;;; init-lsp.el ---                                  -*- lexical-binding: t; -*-

;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package lsp-mode
  ;; :ensure t
  ;; :defer t
  :config (progn
	    (set-face-attribute 'lsp-face-highlight-textual nil
				:background "#666" :foreground "#ffffff")
	    (setq lsp-message-project-root-warning t)
	    ))

(use-package lsp-python
  :ensure t
  :init
  (add-hook 'python-mode-hook #'lsp-python-enable)
  )

(use-package lsp-rust
  :ensure t
  :config (progn
	    (add-hook 'rust-mode-hook #'lsp-rust-enable)
	    ))

(use-package lsp-ui
  :ensure t
  :config (progn
	    ;; It seems that sideline-mode has a bug, just disable it
  	    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
	    ;; bind peek key
	    (define-key lsp-ui-mode-map [remap evil-repeat-pop-next] #'lsp-ui-peek-find-definitions)
	    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
	    (setq
	     lsp-ui-sideline-enable nil
	     lsp-enable-eldoc nil)
  	    )
  )

(use-package ccls
  :ensure t
  :config(progn
	   (require 'company-lsp)
	   (add-hook 'c-mode-hook 'lsp-ccls-enable)
	   (add-hook 'c++-mode-hook 'lsp-ccls-enable)
	   (setq ccls-executable (or (file-exists-p "~/code/cpp/ccls/Release/ccls")
				     (executable-find "ccls")))
	   (setq ccls-sem-highlight-method 'font-lock)
	   (setq ccls-extra-init-params
		 '(:completion (:detailedLabel t) :xref
			       (:container t)
			       :diagnostics (:frequencyMs 5000))
		 )
	   ))
(use-package lsp-go
  :ensure t
  :config (progn
	    (add-hook 'go-mode-hook #'lsp-go-enable)
	    ))
(message "loading init-lsp")
(provide 'init-lsp)
;;; init-lsp.el ends here
