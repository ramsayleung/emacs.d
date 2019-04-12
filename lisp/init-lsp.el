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
  :hook ((go-mode python-mode rust-mode) . lsp)
  :init
  (setq lsp-auto-guess-root t)       ; Detect project root
  (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
  (set-face-attribute 'lsp-face-highlight-textual nil
  		      :background "#666" :foreground "#ffffff")

  ;; Support LSP in org babel
  ;; https://github.com/emacs-lsp/lsp-mode/issues/377
  (cl-defmacro lsp-org-babel-enbale (lang)
    "Support LANG in org source code block."
    ;; (cl-check-type lang symbolp)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (let ((lsp-file (or (->> info caddr (alist-get :file))
                               buffer-file-name)))
             (setq-local buffer-file-name lsp-file)
             (setq-local lsp-buffer-uri (lsp--path-to-uri lsp-file))
             (lsp)))
         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

  (defvar org-babel-lang-list
    '("go" "python" "ipython" "ruby" "js" "css" "sass" "C" "rust" "java"))
  (add-to-list 'org-babel-lang-list (if emacs/>=26p "shell" "sh"))
  (dolist (lang org-babel-lang-list)
    (eval `(lsp-org-babel-enbale ,lang)))
  (setq lsp-eldoc-enable-hover nil)
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
	    (setq
	     lsp-ui-sideline-enable nil
	     lsp-enable-completion-at-point t
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

;; C/C++/Objective-C support
(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode) .
         (lambda ()
	   (require 'ccls)
	   (lsp)))
  :config(progn
	   (setq ccls-executable (if (samray/linux-p)
				     (executable-find "ccls")
				   (expand-file-name "~/code/cpp/ccls/Release/ccls")))
	   (setq ccls-sem-highlight-method 'font-lock)
	   (setq ccls-extra-init-params
		 '(:completion (:detailedLabel t) :xref
			       (:container t)
			       :diagnostics (:frequencyMs 5000))
		 )))
(message "loading init-lsp")
(provide 'init-lsp)
;;; init-lsp.el ends here
