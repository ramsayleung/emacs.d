;;; init-lsp.el ---                                  -*- lexical-binding: t; -*-

;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package lsp-mode
  :ensure t
  :commands lsp
  :config (progn
	    (set-face-attribute 'lsp-face-highlight-textual nil
				:background "#666" :foreground "#ffffff")
	    (setq lsp-message-project-root-warning t)
	    (setq lsp-prefer-flymake nil)
	    (add-hook 'python-mode-hook #'lsp)
	    (add-hook 'rust-mode-hook #'lsp)
	    (add-hook 'go-mode-hook #'lsp)
	    ))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config (progn
	    ;; It seems that sideline-mode has a bug, just disable it
	    ;; bind peek key
	    (define-key lsp-ui-mode-map [remap evil-repeat-pop-next] #'lsp-ui-peek-find-definitions)
	    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
	    (setq
	     lsp-ui-sideline-enable nil
	     lsp-enable-completion-at-point t
	     lsp-ui-doc-position 'at-point
	     lsp-ui-doc-header nil
	     lsp-ui-doc-include-signature t
	     lsp-ui-doc-border nil)
	    (setq-default lsp-ui-doc-frame-parameters '((left . -1)
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
	    )
  )

(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config(progn
	   (setq ccls-executable (if (file-exists-p "/home/samray/code/cpp/ccls/Release/ccls")
				     "/home/samray/code/cpp/ccls/Release/ccls"
				   (executable-find "ccls")))
	   (setq ccls-sem-highlight-method 'font-lock)
	   (setq ccls-extra-init-params
		 '(:completion (:detailedLabel t) :xref
			       (:container t)
			       :diagnostics (:frequencyMs 5000))
		 )))
(message "loading init-lsp")
(provide 'init-lsp)
;;; init-lsp.el ends here
