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

(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode . " yas")
  :commands (yas-expand-snippet yas-insert-snippet yas-new-snippet)
  :init (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config (progn
	    (run-with-idle-timer ramsay-idle-time nil 'yas-reload-all)
	    ;; (yas-reload-all)
	    (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
	    ))

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
;; Projectile
(use-package projectile
  :ensure t
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

(defun ramsay/switch-to-buffer (repl-buffer-name)
  "Switch to the  buffer REPL-BUFFER-NAME.
similar to shell-pop"
  (interactive)
  ;; Shell buffer exists?
  (let* ((shell-window (get-buffer-window repl-buffer-name 'visual)))
    (if (get-buffer repl-buffer-name)
	;; Shell buffer is visible?
	(if shell-window
	    ;; Buffer in current window is shell buffer?
	    (if (string= (buffer-name (window-buffer)) repl-buffer-name)
		(if (not (one-window-p))
		    (progn (bury-buffer)
			   (delete-window)))
	      ;; If not, select window which points to shell bufffer.
	      (progn
		(select-window shell-window)
		(when (and (boundp 'evil-mode) evil-mode)
		  (evil-insert-state)))
	      )
	  ;; If shell buffer is not visible, split a window and switch to it.
	  (progn
	    ;; Use `split-window-sensibly` to split window with policy
	    ;; If window cannot be split, force to split split window horizontally
	    (when (not (split-window-sensibly))
	      (ramsay/split-window-below-and-move))
	    (switch-to-buffer repl-buffer-name)
	    (when (and (boundp 'evil-mode) evil-mode)

	      (evil-insert-state))
	    ))
      ;; If shell buffer doesn't exist, create one
      (progn
	(when (not (split-window-sensibly))
	  (ramsay/split-window-below-and-move))
	(run-python)
	(when evil-mode
	  (evil-insert-state))
	)))
  )

(defun ramsay/repl-pop ()
  "Run REPL for different major mode and switch to the repl buffer.
similar to shell-pop"
  (interactive)
  (let* ((repl-modes '((python-mode . "*Python*")
		       (scheme-mode . "* Mit REPL *"))))
    (cond ((or (derived-mode-p 'python-mode) (derived-mode-p 'inferior-python-mode))
	   (progn
;;; To fix issue that there is weird eshell output with ipython
	     (ramsay/switch-to-buffer (cdr (assoc 'python-mode repl-modes)))))
	  ((or (derived-mode-p 'scheme-mode) (derived-mode-p 'geiser-repl-mode))
	   (ramsay/switch-to-buffer (cdr (assoc 'scheme-mode repl-modes))))
	  ((or (derived-mode-p 'prog-mode)(derived-mode-p 'inferior-python-mode))
	   (progn
	     (ramsay/switch-to-buffer (cdr (assoc 'python-mode repl-modes)))))
	  )))
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


(defun ramsay/imenu ()
  "Call lsp-ui-imenu firstly, if lsp-mode is disable, call counsel-imenu instead."
  (interactive)
  (if lsp-mode
      (lsp-ui-imenu)
    (counsel-imenu)))

(defun ramsay/get-buffer-name ()
  "Get current buffer name."
  (interactive)
  (kill-new (buffer-name))
  )


(defun ramsay/basename (path)
  "Emacs version of `basename`."
  (file-name-nondirectory (directory-file-name path)))

(message "loading init-programming")
(provide 'init-programming)

;;; init-programming.el ends here
