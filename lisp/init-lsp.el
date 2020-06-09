;;; init-lsp.el ---                                  -*- lexical-binding: t; -*-


;;; package --- Summary
;;; Commentary:
;;; Code:

(use-package company-lsp
  :ensure t
  :config (progn
	    (push 'company-lsp company-backends))
  )

(use-package lsp-mode
  :ensure t
  :defines (lsp-clients-typescript-server lsp-clients-typescript-server-args)
  :hook ((go-mode python-mode rust-mode sh-mode c-mode c++-mode objc-mode) . lsp)
  :init
  (setq lsp-auto-guess-root t)       ; Detect project root
  (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
  (setq lsp-rust-server 'rust-analyzer)
  (add-to-list 'company-lsp-filter-candidates '(gopls . nil))
  (add-hook 'c++-mode-hook (lambda () (flycheck-select-checker 'c/c++-clang)))
  (add-hook 'c-mode-hook (lambda () (flycheck-select-checker 'c/c++-clang)))
  (add-hook 'rust-mode-hook (lambda ()(flycheck-select-checker 'rust-cargo)))
  (add-hook 'python-mode-hook (lambda () (flycheck-select-checker 'python-flake8)))
  (set-face-attribute 'lsp-face-highlight-textual nil
  		      :background "#666" :foreground "#ffffff")
  )


(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config (progn
	    ;; It seems that sideline-mode has a bug, just disable it
	    ;; bind peek key
	    (define-key lsp-ui-mode-map [remap evil-repeat-pop-next] #'lsp-ui-peek-find-definitions)
	    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

	    (setq lsp-ui-peek-fontify 'always)
	    (setq lsp-eldoc-enable-hover nil)
	    (setq lsp-ui-doc-enable nil)
	    (setq
	     lsp-ui-sideline-enable nil
	     lsp-enable-completion-at-point nil
	     lsp-ui-doc-position 'at-point
	     lsp-ui-doc-header nil
	     lsp-ui-doc-border nil)
	    (setq lsp-ui-doc-frame-parameters '((left . -1)
						(top . -1)
						(no-accept-focus . t)
						(min-width . 0)
						(width . 0)
						(min-height . 0)
						(height . 0)
						(internal-border-width . 5)
						(vertical-scroll-bars)
						(horizontal-scroll-bars)
						(left-fringe . 0)
						(right-fringe . 0)
						(menu-bar-lines . 0)
						(tool-bar-lines . 0)
						(line-spacing . 0.1)
						(unsplittable . t)
						(undecorated . t)
						(visibility . nil)
						(mouse-wheel-frame . nil)
						(no-other-frame . t)
						(cursor-type)
						(no-special-glyphs . t)))
	    (setq lsp-ui-doc-include-signature nil)
	    )
  )

(message "loading init-lsp")
(provide 'init-lsp)
;;; init-lsp.el ends here
