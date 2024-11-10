;;; package --- Summary:
;;; Commentary:
;;; Code:
;;; Commentary:
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
  :init
	(exec-path-from-shell-initialize)
	(setq exec-path-from-shell-variables '("RUST_SRC_PATH" "PATH" "PYTHONPATH" "GOPATH" "GOROOT"))
	;; when it is nil, exec-path-from-shell will read environment variable
	;; from .zshenv instead of .zshrc, but makes sure that you put all
	;; environment variable you need in .zshenv rather than .zshrc
	(setq exec-path-from-shell-check-startup-files nil) ;
	(setq exec-path-from-shell-arguments '("-l")) ;remove -i read form .zshenv
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
