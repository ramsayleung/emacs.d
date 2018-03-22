;;; init-lsp.el ---                                  -*- lexical-binding: t; -*-

;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package lsp-mode
  :ensure t
  :defer t
  :config (progn
	    (add-hook 'python-mode-hook 'lsp-mode)
	    (add-hook 'rust-mode-hook 'lsp-mode)
	    (set-face-attribute 'lsp-face-highlight-textual nil
				:background "#666" :foreground "#ffffff"
				)
	    ))

(use-package lsp-python
  :ensure t
  :init
  (add-hook 'python-mode-hook 'lsp-python-enable)
  )
(use-package lsp-rust
  :ensure t
  :config (progn
	    (add-hook 'rust-mode-hook 'lsp-rust-enable)
	    (add-hook 'rust-mode-hook 'flycheck-mode)
	    ))

(use-package lsp-ui
  :ensure t
  :init (progn
	  (setq
	   lsp-ui-sideline-enable nil
	   ;; Disable eldoc-mode
	   lsp-enable-eldoc nil
	   lsp-ui-doc-header nil
	   lsp-ui-doc-border nil)
	  )
  :config (progn
  	    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
	  ;; bind peek key
	  (define-key lsp-ui-mode-map (kbd "M-.") #'xref-find-definitions)
	  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
	  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  	    )
  )
(provide 'init-lsp)
;;; init-lsp.el ends here
