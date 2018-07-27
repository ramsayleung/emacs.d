;;; init-lsp.el ---                                  -*- lexical-binding: t; -*-

;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package lsp-mode
  ;; :ensure t
  ;; :defer t
  :load-path "~/code/elisp/lsp-mode"
  :config (progn
	    (add-hook 'python-mode-hook 'lsp-mode)
	    (add-hook 'rust-mode-hook 'lsp-mode)
	    (set-face-attribute 'lsp-face-highlight-textual nil
				:background "#666" :foreground "#ffffff"
				)
	    (setq lsp-prompt-project-root t)
	    ))

(use-package lsp-python
  :ensure t
  :init
  (add-hook 'python-mode-hook 'lsp-python-enable)
  )
(use-package lsp-rust
  :ensure t
  :config (progn
	    (add-hook 'rust-mode-hook #'lsp-rust-enable)
	    ))

(use-package lsp-ui
  :ensure t
  :init
  :config (progn
	    ;; It seems that sideline-mode has a bug, just disable it
  	    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
	    ;; bind peek key
	    (define-key lsp-ui-mode-map (kbd "M-.") #'xref-find-definitions)
	    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
	    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
	    (setq
	     lsp-ui-sideline-enable nil
	     lsp-enable-eldoc nil)
  	    )
  )
;; ;; ;;; cquery cpp
(use-package cquery
  :ensure t
  :config (progn
	    (require 'company-lsp)
	    (add-hook 'c-mode-hook 'lsp-cquery-enable)
	    (add-hook 'c++-mode-hook 'lsp-cquery-enable)
	    ;; ;; Arch Linux aur/cquery-git aur/cquery
	    (when (equal system-type 'gnu/linux)
	      (setq cquery-executable "/home/samray/bin/cquery-v20180302-x86_64-unknown-linux-gnu/bin/cquery"))
	    (when (equal system-type 'darwin)
	      (setq cquery-executable "/Users/samray/development/cpp/cquery/Release/cquery"))
	    ;; ;; Log file
	    (setq cquery-extra-args '("--log-file=/tmp/cq.log"))
	    ;; Cache directory, both relative and absolute paths are supported
	    (setq cquery-cache-dir ".cquery_cached_index")
	    ;; Initialization options
	    (setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack" :completion (:detailedLabel t)))
	    ))

;; (use-package lsp-clangd
;;   :ensure t
;;   :init
;;   (when (equal system-type 'darwin)
;;     (setq lsp-clangd-executable "/usr/local/opt/llvm/bin/clangd"))

;;   (add-hook 'c-mode--hook #'lsp-clangd-c-enable)
;;   (add-hook 'c++-mode-hook #'lsp-clangd-c++-enable)
;; (add-hook 'objc-mode-hook #'lsp-clangd-objc-enable))



;; (use-package ccls
;;   :ensure t
;;   :config
;;   (progn
;;     (add-hook 'c-mode-hook 'lsp-ccls-enable)
;;     (add-hook 'c++-mode-hook 'lsp-ccls-enable)
;;     (setq ccls-executable "/home/samray/code/cpp/ccls/release/ccls")
;;     (setq ccls-sem-highlight-method 'font-lock)
;;     (setq ccls-extra-init-params
;; 	  '(:completion (:detailedLabel t) :xref (:container t)
;; 			:diagnostics (:frequencyMs 5000))
;; 	  )))
(provide 'init-lsp)
;;; init-lsp.el ends here
