;;; package --- Summary -*- lexical-binding: t; -*-
;;; code:
;;; Commentary:

(use-package vterm
  :ensure t)

(use-package shell-pop
  :ensure t
  :custom
  (shell-pop-window-size 50)
  (shell-pop-shell-type '("vterm" "*vterm*" (lambda () (vterm)))))

;;; https://www.emacswiki.org/emacs/EshellFunctions
(defun ramsay/eshell-maybe-bol ()
  "Go to the beginning of command line,or begining of line."
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
	(beginning-of-line))))

(defun ramsay/kill-word-backward ()
  "Let Eshell kill word acting like zsh."
  (interactive)
  (set-mark-command nil)
  (backward-word)
  (call-interactively 'kill-region))

;;; Inspire by http://blog.binchen.org/posts/use-ivy-mode-to-search-bash-history.html
(defun ramsay/parse-bash-history ()
  "Parse the bash history."
  (interactive)
  (when (file-exists-p "~/.bash_history")
    (let (collection bash_history)
      (shell-command "history -r") ; reload history
      (setq collection
            (nreverse
             (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
                                             (buffer-string))
                           "\n"
                           t)))
      (when (and collection (> (length collection) 0)
		 (setq bash_history collection))
	bash_history))))

(defun ramsay/parse-zsh-history ()
  "Parse the bash history."
  (interactive)
  (when (file-exists-p "~/.zsh_history")
    (let (collection zsh_history)
      (shell-command "history -r") ; reload history
      (setq collection
            (nreverse
             (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.zsh_history"))
                                             (replace-regexp-in-string "^:[^;]*;" "" (buffer-string)))
                           "\n"
                           t)))
      (when (and collection (> (length collection) 0)
		 (setq zsh_history collection))
	zsh_history))))

(defun ramsay/esh-history ()
  "Interactive search eshell history."
  (interactive)
  (require 'em-hist)
  (save-excursion
    (let* ((start-pos (eshell-beginning-of-input))
	   (input (eshell-get-old-input))
	   (esh-history (when (> (ring-size eshell-history-ring) 0)
			  (ring-elements eshell-history-ring)))
	   (all-shell-history (append esh-history (ramsay/parse-zsh-history) (ramsay/parse-bash-history)))
	   )
      (let* ((command (ivy-read "Command: "
				(delete-dups all-shell-history)
				:initial-input input
				:require-match t
				:action #'ivy-completion-in-region-action))
	     )
	(eshell-kill-input)
	(insert command)
	)))
  ;; move cursor to eol
  (end-of-line))


(defun ramsay/eshell-clear-buffer ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(use-package eshell
  :commands eshell
  :init
  (setq
   eshell-highlight-prompt nil
   eshell-buffer-shorthand t
   eshell-history-size 5000
   ;;  ;; auto truncate after 12k lines
   eshell-buffer-maximum-lines 12000
   eshell-hist-ignoredups t
   eshell-error-if-no-glob t
   eshell-glob-case-insensitive t
   eshell-scroll-to-bottom-on-input 'all
   eshell-list-files-after-cd t
   eshell-aliases-file (concat user-emacs-directory "eshell/alias")
   eshell-banner-message ""
   ;; eshell-banner-message "What would you like to do?\n\n"
   )
  ;; Visual commands
  (setq eshell-visual-commands '("vi" "screen" "top" "less" "more" "lynx"
				 "ncftp" "pine" "tin" "trn" "elm" "vim"
				 "nmtui" "alsamixer" "htop" "el" "elinks"
				 ))
  (setq eshell-visual-subcommands '(("git" "log" "diff" "show")))
  (require 'esh-mode) ; eshell-mode-map
  :config
  (when (not (functionp 'eshell/rgrep))
    (defun eshell/rgrep (&rest args)
      "Use Emacs grep facility instead of calling external grep."
      (eshell-grep "rgrep" args t)))
  (add-hook 'eshell-mode-hook
	    (lambda ()(eshell-cmpl-initialize)))
  (add-hook 'eshell-mode-hook (lambda ()
				(setq-local global-hl-line-mode nil)))
  
  :bind (:map eshell-mode-map
	      (("C-a" . ramsay/eshell-maybe-bol)
	       ("C-w" . ramsay/kill-word-backward)
	       ("C-k" . paredit-kill)
	       ("C-r" . ramsay/esh-history)
	       ("C-l" . ramsay/eshell-clear-buffer)))
  )

(message "loading init-shell")
(provide 'init-shell)
;;; init-shell.el ends here
