;;; init-lsp.el ---                                  -*- lexical-binding: t; -*-


;;; package --- Summary
;;; Commentary:
;;; Code:


(use-package lsp-mode
  :ensure t
  :defines (lsp-clients-typescript-server lsp-clients-typescript-server-args)
  :hook ((python-mode rust-mode) . lsp)
  :init
  (setq lsp-auto-guess-root t)       ; Detect project root
  (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck

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

  (with-eval-after-load 'lsp
    (set-face-attribute 'lsp-face-highlight-textual nil
  			:background "#666" :foreground "#ffffff")
    )
  (setq lsp-clients-typescript-server "typescript-language-server"
        lsp-clients-typescript-server-args '("--stdio")))

(use-package lsp-ui
  :ensure t
  :config (progn
	    (setqlsp-ui-sideline-enable nil))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)))

(use-package company-lsp
  :ensure t
  )

;; C/C++/Objective-C support
(use-package ccls
  :ensure t
  :defines projectile-project-root-files-top-down-recurring
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda ()
                                                   (require 'ccls)
                                                   (lsp)))
  :config
  (setq ccls-executable (or (file-exists-p "~/code/cpp/ccls/Release/ccls")
			    (executable-find "ccls")))
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
          (append '("compile_commands.json"
                    ".ccls")
                  projectile-project-root-files-top-down-recurring))))

(message "loading init-lsp")
(provide 'init-lsp)
;;; init-lsp.el ends here
