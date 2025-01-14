;;; package --- Summary:
;;; Commentary:
;;; Code:
;;; Commentary:

;;; LSP
(use-package eglot
  :ensure t
  :hook ((python-mode . eglot-ensure)
		 (python-ts-mode . eglot-ensure)
		 (typescript-ts-mode . eglot-ensure)
		 (typescript-ts-base-mode . eglot-ensure)
		 (ruby-mode . eglot-ensure)
		 (ruby-ts-mode . eglot-ensure)
		 (rust-mode . eglot-ensure)
		 (rust-ts-mode . eglot-ensure)
		 (shell-mode . eglot-ensure)
		 (js-mode . eglot-ensure)
		 (js-ts-mode . eglot-ensure)
		 (c++-mode . eglot-ensure)
		 (c++-ts-mode . eglot-ensure)
		 (go-mode . eglot-ensure)
		 (go-ts-mode . eglot-ensure)
		 (c-mode . eglot-ensure)
		 (c-ts-mode . eglot-ensure))
  )

(use-package eglot-booster
  :init (ramsay/vc-install :fetcher "github" :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode)
  )

;;; C family
(use-package clang-format
  :ensure t
  :commands (clang-format-region clang-format-buffer)
  :config
  (setq clang-format-style "llvm")
  )

(use-package cc-mode
  :mode
  (("\\.c\\'" . c-mode)
   ("\\.h\\'" . c++-mode)
   ("\\.hpp\\'" . c++-mode)
   ("\\.cpp\\'" . c++-mode))
  :config
  (eval-after-load "cc-mode"
    '(define-key c-mode-base-map ";" nil))
  )

(use-package cmake-mode
  :ensure t
  :mode (
		 ("CMakeLists\\.txt\\'" . cmake-mode)
		 ("\\.cmake\\'" . cmake-mode)
		 ))

(require 'compile)
;;; Translate ANSI escape sequence
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(setq compilation-scroll-output t)
(defun ramsay/run-with-python ()
  "Set the default \"compile-command\" to run the current file with python."
  (setq-local compile-command
			  (concat "python3 " (when buffer-file-name
								   (shell-quote-argument buffer-file-name)))))
(add-hook 'python-base-mode-hook 'ramsay/run-with-python)
(defun ramsay/compile-rust ()
  "Set the default \"compile-command\" for Rust project."
  (setq-local compile-command "cargo build"))
(add-hook 'rust-ts-mode-hook 'ramsay/compile-rust)

;;; Python
;; Code navigation,documentation lookup and completing for python
(setq tab-width 4)
(set-variable 'py-indent-offset 4)
(set-variable 'python-indent-guess-indent-offset nil)

(use-package python-mode
  ;; :mode("\\.py\\'" . python-mode)
  :ensure t
  :config
  ;; https://github.com/jorgenschaefer/elpy/issues/887
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
		  (python-shell-completion-native-output-timeout
		   python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_")))

  (defun ramsay/pyrightconfig-find-venv-directories (project-root)
	"Find potential virtual environment directories for a project in PROJECT-ROOT."
	(let* ((common-venv-names '(".venv" ".env" "venv" "env"))
           (common-venv-locations (list project-root))
           (potential-paths '()))
      
      ;; Check common locations in project root and home directory
      (dolist (location common-venv-locations)
		(dolist (name common-venv-names)
          (let ((full-path (expand-file-name name location)))
			(when (file-directory-p full-path)
              (push full-path potential-paths)))))
      
      ;; Return found paths
      (reverse potential-paths)))

  (defun ramsay/pyrightconfig-suggest ()
	"Interactively select a virtualenv and write pyrightconfig.json."
	(interactive)
	(let* ((project-root (or (vc-git-root default-directory)
							 default-directory))
           (venv-paths (ramsay/pyrightconfig-find-venv-directories project-root))
           (selected-venv
			(completing-read
			 "Select virtual environment: "
			 (append venv-paths
					 ;; Add option to specify custom path
					 '("[Custom path...]"))
			 nil t)))
      
      (if (string= selected-venv "[Custom path...]")
          ;; If custom path selected, call original function
          (call-interactively #'ramsay/pyrightconfig-write)
		;; Otherwise use selected path
		(ramsay/pyrightconfig-write selected-venv))))

  ;; Derived from https://robbmann.io/posts/emacs-eglot-pyrightconfig/
  (defun ramsay/pyrightconfig-write (virtualenv)
	"Write pyrightconfig.json for the given VIRTUALENV path."
	(interactive "DEnv: ")
	(let* ((venv-dir (tramp-file-local-name (file-truename virtualenv)))
           (venv-file-name (directory-file-name venv-dir))
           (venvPath (file-name-directory venv-file-name))
           (venv (file-name-base venv-file-name))
           (base-dir (vc-git-root default-directory))
           (out-file (expand-file-name "pyrightconfig.json" base-dir))
           (out-contents (json-encode (list :venvPath venvPath :venv venv))))
      (with-temp-file out-file (insert out-contents))))
  )

;;; Rust
(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :config (remove-hook 'rust-mode-hook 'adaptive-wrap-prefix-mode)
  )

(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'" . toml-mode))

;;; https://github.com/rust-lang/rust-mode/issues/208
(setq rust-match-angle-brackets nil)

(use-package cargo
  :diminish cargo-minor-mode
  :ensure t
  :defer t
  :init (progn
		  (add-hook 'rust-mode-hook 'cargo-minor-mode)))

(defvar ramsay/cargo-process--command-script "script")
(defun ramsay/cargo-process-script ()
  "Run cargo script command to compile and run a single file."
  (interactive)
  (cargo-process--start (concat "Script " (buffer-file-name))
                        (concat ramsay/cargo-process--command-script
                                " "
                                (buffer-file-name))))

(defun ramsay/cargo-process-run-current-example ()
  "Run current buffer/file as example."
  (interactive)
  (cargo-process-run-example (file-name-sans-extension (buffer-name))))

;;; Lisp and dialect
(use-package scheme-mode
  ;; I am not sure why `:mode ("\\.rkt\\'" "\\.scm\\'")` doesn't work
  ;; So I choose to implement it with `':hook`, the tricky way
  :hook (scheme-mode . racket-mode))

(use-package racket-mode
  :ensure t
  :hook (racket-mode . racket-xp-mode)
  :mode ("\\.rkt\\'" "\\.scm\\'"))

;;; Web
(use-package php-mode
  :ensure t
  :mode (("\\.php\\'" . php-mode)))

;;; http://danmidwood.com/content/2014/11/21/animated-paredit.html
(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init (progn
		  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
		  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
		  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
		  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
		  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
		  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
		  (add-hook 'racket-mode-hook           #'enable-paredit-mode)
          ;;; Auto complete pair symbol, such as `()`, `{}`
		  (dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook scheme-mode-hook lisp-interaction-mode-hook python-mode-hook rust-mode-hook c++-mode-hook racket-mode-hook))
 			(add-hook hook 'electric-pair-mode))
		  )
  )

(use-package js2-mode
  :ensure t
  :config
  (add-hook 'js-mode-hook 'js2-minor-mode))

(use-package web-mode
  :ensure t
  :mode (
		 ".erb$"
		 ".phtml$"
		 ".php$"
		 ".[agj]sp$"
		 ".as[cp]x$"
		 ".mustache$"
		 ".djhtml$"
		 )
  :init
  (setq web-mode-extra-snippets
		'(("erb" . (("toto" . "<% toto | %>\n\n<% end %>")))
          ("php" . (("dowhile" . "<?php do { ?>\n\n<?php } while (|); ?>")
                    ("debug" . "<?php error_log(__LINE__); ?>")))
		  ))
  (setq web-mode-extra-auto-pairs
		'(("erb"  . (("beg" "end")))
          ("php"  . (("beg" "end")
                     ("beg" "end")))
		  ))
  )

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode))

;;; Generate Html and Css code
(use-package emmet-mode
  :ensure t
  :defer t
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  (add-hook 'js2-mode-hook 'emmet-mode)
  (add-hook 'rjsx-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  )

(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode . " Yas")
  :commands (yas-expand-snippet yas-insert-snippet yas-new-snippet)
  :hook
  (org-mode . yas-minor-mode)
  (prog-mode . yas-minor-mode)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-reload-all)
  )


;; Make Emacs use the $PATH set up by the user's shell
(use-package exec-path-from-shell
  :ensure t
  :if (or (memq window-system '(mac ns x))
		  (daemonp))
  :init
  (setq exec-path-from-shell-debug t)
  (setq exec-path-from-shell-variables '("RUST_SRC_PATH" "PATH" "PYTHONPATH" "GOPATH" "GOROOT"))
  (setq exec-path-from-shell-arguments '("-l")) ;remove -i read form .zshenv
  (exec-path-from-shell-initialize)
  )

;; Emacs extension to increate selected region by semantic units
(use-package expand-region
  :ensure t
  :commands er/expand-region
  )

(use-package deadgrep
  :ensure t
  :commands (deadgrep))

(use-package helpful
  :ensure t
  :init
  (setq helpful-max-buffers 1)
  :commands (helpful-callable helpful-function helpful-macro helpful-command helpful-key helpful-variable helpful-at-point))

(defun comment-auto-fill ()
  "Auto fill comments but not code in programmingModes."
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))
(with-eval-after-load 'prog-mode
  (add-hook 'prog-mode-hook 'comment-auto-fill))

(global-subword-mode t)

(defun ramsay/get-buffer-name ()
  "Get current buffer name."
  (interactive)
  (kill-new (buffer-name))
  )
(defun ramsay/get-buffer-full-name()
  "Get the full path name of current buffer."
  (interactive)
  (kill-new (buffer-file-name))
  )

;;; Enable tree-sitter after Emacs-29
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(message "loading init-programming")
(provide 'init-programming)

;;; init-programming.el ends here
