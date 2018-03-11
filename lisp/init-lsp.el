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
	    (add-hook 'lsp-ui-mode-hook (lambda ()
					  (lsp-ui-sideline-mode -1)
					  (lsp-ui-doc-mode -1)
					  ))
	    ;; bind peek key
	    (define-key lsp-ui-mode-map (kbd "M-.") #'xref-find-definitions)
	    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
	    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

  	    )
  )
;;; cquery cpp
(use-package cquery
  :ensure t
  :config (progn
	    (add-hook 'c-mode-hook 'lsp-cquery-enable)
	    (add-hook 'c++-mode-hook 'lsp-cquery-enable)
	    ;; ;; Arch Linux aur/cquery-git aur/cquery
	    (setq cquery-executable "/usr/bin/cquery")
	    ;; ;; Log file
	    (setq cquery-extra-args '("--log-file=/tmp/cq.log"))
	    ;; Cache directory, both relative and absolute paths are supported
	    (setq cquery-cache-dir ".cquery_cached_index")
	    ;; Initialization options
	    (setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack" :completion (:detailedLabel t)))
	    ))
(provide 'init-lsp)
;;; init-lsp.el ends here
