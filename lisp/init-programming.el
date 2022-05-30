;;; package --- Summary:
;;; Commentary:
;;; Code:
;;; Commentary:

;;; enhanced gud
(use-package realgud
  :ensure t
  :commands (realgud:gdb realgud:pdb)
  :init
  (setq realgud:pdb-command-name "python3 -m pdb")
  )

(use-package groovy-mode
  :ensure t
  :mode "\\.groovy$")

(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode . " yas")
  :commands (yas-expand-snippet yas-insert-snippet yas-new-snippet)
  :config (progn
	    (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
	    (yas-reload-all)
	    (add-hook 'org-mode-hook 'yas-minor-mode)
	    (add-hook 'prog-mode-hook 'yas-minor-mode)
	    ))

;;; velecity mode
(use-package vtl-mode
  :load-path "~/.emacs.d/additional-packages/vtl.el"
  :mode "\\.vm$")

(use-package yaml-mode
  :ensure t
  :mode "\\.yml$")

(use-package es-mode
  :ensure t
  :mode "\\.es$"
  :config (progn
	    (setq es-always-pretty-print t)
	    ))

(use-package json-mode
  :ensure t
  :mode "\\.json$"
  :init (remove-hook 'json-mode 'tern-mode)
  )

(use-package plantuml-mode
  :ensure t
  :mode "\\.plantuml$"
  :config
  (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-executable-path (string-trim (shell-command-to-string "which plantuml")))
  (setq org-plantuml-exec-mode 'plantuml)
  )

;; Make Emacs use the $PATH set up by the user's shell
(use-package exec-path-from-shell
  :ensure t
  :init (progn
	  (exec-path-from-shell-initialize)
	  (setq exec-path-from-shell-variables '("RUST_SRC_PATH" "PATH" "PYTHONPATH" "GOPATH" "GOROOT"))
	  ;; when it is nil, exec-path-from-shell will read environment variable
	  ;; from .zshenv instead of .zshrc, but makes sure that you put all
	  ;; environment variable you need in .zshenv rather than .zshrc
	  (setq exec-path-from-shell-check-startup-files nil) ;
	  (setq exec-path-from-shell-arguments '("-l")) ;remove -i read form .zshenv
	  (when (not (getenv "GOROOT"))
	    (setenv "GOROOT" (replace-regexp-in-string "\n$" "" (shell-command-to-string "go env GOROOT"))))
	  (when (not (getenv "GOPATH"))
	    (setenv "GOPATH" (expand-file-name "~/code/go")))
	  )
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
  :commands (helpful-callable helpful-function helpful-macro helpful-command helpful-key helpful-variable helpful-at-point))

;; Projectile
(use-package projectile
  :if (not emacs/>=28p)
  :ensure t
  :defer t
  :init (progn
	  (setq projectile-indexing-method 'hybrid)
	  (setq projectile-git-submodule-command nil)
	  (setq projectile-mode-line-prefix " proj"))
  :commands (counsel-projectile-switch-project counsel-projectile-find-file)
  :config
  (progn
    (projectile-mode)
    (setq projectile-completion-system 'ivy)
    (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
    (add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index")
    (add-to-list 'projectile-globally-ignored-directories ".vscode")
    (add-to-list 'projectile-globally-ignored-directories "CMakeFiles")
    )
  )

;;; https://www.emacswiki.org/emacs/AutoFillMode
;;; auto format comment to 80-char long
(setq-default fill-column 80)

(defun comment-auto-fill ()
  "Auto fill comments but not code in programmingModes."
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))
(with-eval-after-load 'prog-mode
  (add-hook 'prog-mode-hook 'comment-auto-fill))

;;; Treating terms in CamelCase symbols as separate words makes editing a
;;; little easier for me, so I like to use subword-mode everywhere.
;;;  Nomenclature           Subwords
;; ===========================================================
;; GtkWindow          =>  "Gtk" and "Window"
;; EmacsFrameClass    =>  "Emacs", "Frame" and "Class"
;; NSGraphicsContext  =>  "NS", "Graphics" and "Context"
(global-subword-mode t)
;;; Put *compilation* buffer in the bottom of window which will disappears
;;; automatically,instead shows in other window
(setq compilation-scroll-output t)

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


(defun ramsay/basename (path)
  "Emacs version of `basename`."
  (file-name-nondirectory (directory-file-name path)))

(message "loading init-programming")
(provide 'init-programming)

;;; init-programming.el ends here
