;;; init-lsp.el ---                                  -*- lexical-binding: t; -*-


;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package citre
  :defer t
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  :config
  (setq
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   citre-project-root-function #'projectile-project-root
   ;; Set this if you want to always use one location to create a tags file.
   citre-default-create-tags-file-location 'global-cache
   ;; See the "Create tags file" section above to know these options
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t)

  ;; https://github.com/universal-ctags/citre/wiki/Use-Citre-together-with-lsp-mode
  (define-advice xref--create-fetcher (:around (-fn &rest -args) fallback)
    (let ((fetcher (apply -fn -args))
          (citre-fetcher
           (let ((xref-backend-functions '(citre-xref-backend t)))
             (apply -fn -args))))
      (lambda ()
	(or (with-demoted-errors "%s, fallback to citre"
              (funcall fetcher))
            (funcall citre-fetcher)))))
  (defun lsp-citre-capf-function ()
    "A capf backend that tries lsp first, then Citre."
    (let ((lsp-result (lsp-completion-at-point)))
      (if (and lsp-result
               (try-completion
		(buffer-substring (nth 0 lsp-result)
                                  (nth 1 lsp-result))
		(nth 2 lsp-result)))
          lsp-result
	(citre-completion-at-point))))

  (defun enable-lsp-citre-capf-backend ()
    "Enable the lsp + Citre capf backend in current buffer."
    (add-hook 'completion-at-point-functions #'lsp-citre-capf-function nil t))

  (add-hook 'citre-mode-hook #'enable-lsp-citre-capf-backend)

  (defun company-citre (-command &optional -arg &rest _ignored)
    "Completion backend of Citre.  Execute COMMAND with ARG and IGNORED."
    (interactive (list 'interactive))
    (cl-case -command
      (interactive (company-begin-backend 'company-citre))
      (prefix (and (bound-and-true-p citre-mode)
                   (or (citre-get-symbol) 'stop)))
      (meta (citre-get-property 'signature -arg))
      (annotation (citre-capf--get-annotation -arg))
      (candidates (all-completions -arg (citre-capf--get-collection -arg)))
      (ignore-case (not citre-completion-case-sensitive))))

  (setq company-backends '((company-capf company-citre :with company-yasnippet :separate)))
  )

(use-package lsp-mode
  :ensure t
  :defines (lsp-clients-typescript-server lsp-clients-typescript-server-args)
  :hook
  ((go-mode python-mode rust-mode sh-mode c-mode c++-mode objc-mode) . lsp)
  :init
  (setq lsp-auto-guess-root t)       ; Detect project root
  (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
  (setq lsp-rust-server 'rust-analyzer)
  (setq rustic-lsp-server 'rust-analyzer)
  (setq lsp-rust-analyzer-store-path (executable-find "rust-analyzer"))
  (setq lsp-modeline-code-actions-segments '(name count))
  (add-hook 'c++-mode-hook (lambda () (flycheck-select-checker 'c/c++-clang)))
  (add-hook 'c-mode-hook (lambda () (flycheck-select-checker 'c/c++-clang)))
  ;; (add-hook 'rust-mode-hook (lambda ()(flycheck-select-checker 'rust-cargo)))
  (add-hook 'python-mode-hook (lambda () (flycheck-select-checker 'python-flake8)))

  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)))

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

;; (message "loading init-lsp")
(provide 'init-lsp)
;;; init-lsp.el ends here
