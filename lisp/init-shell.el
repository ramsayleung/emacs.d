;;; package --- Summary -*- lexical-binding: t; -*-
;;; code:
;;; Commentary:

(use-package shell-pop
  :ensure t
  :custom
  (shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
  (shell-pop-term-shell "eshell")
  (shell-pop-window-size 70))

(defvar ramsay/zsh-history-cache nil
  "Cache for zsh history commands.")

(defvar ramsay/zsh-history-last-modified nil
  "Last modification time of the zsh history file.")

(defun ramsay/parse-zsh-history ()
  "Parse the zsh history, updating cache only if the history file has changed."
  (interactive)
  (let* ((history-file (expand-file-name "~/.zsh_history"))
         (current-mod-time (and (file-exists-p history-file)
				(file-attribute-modification-time
                                 (file-attributes history-file)))))
    (when (and current-mod-time
               (or (null ramsay/zsh-history-last-modified)
                   (time-less-p ramsay/zsh-history-last-modified current-mod-time)))
      ;; Update cache only if file has been modified
      (setq ramsay/zsh-history-cache
            (with-temp-buffer
              (insert-file-contents history-file)
              ;; Remove the zsh history metadata (e.g., ": 1738134318:0;")
              (let ((history (replace-regexp-in-string "^:[^;]*;" "" (buffer-string))))
                ;; Split the cleaned history into lines and reverse the order
                (nreverse (split-string history "\n" t))))
            ramsay/zsh-history-last-modified current-mod-time)))
  ramsay/zsh-history-cache)

(defun ramsay/esh-history ()
  "Interactive search eshell history."
  (interactive)
  (require 'em-hist)
  (save-excursion
    (let* ((start-pos (eshell-beginning-of-input))
	   (input (eshell-get-old-input))
	   (esh-history (when (> (ring-size eshell-history-ring) 0)
			  (ring-elements eshell-history-ring)))
	   (all-shell-history (append esh-history (ramsay/parse-zsh-history)))
	   )
      (if (not all-shell-history)
	  (message "No shell history available.")
	(let* ((command (completing-read "Command: "
					 (seq-uniq all-shell-history)
					 nil nil input))
	       )
	  (eshell-kill-input)
	  (insert command)
	  ))))
  ;; move cursor to eol
  (end-of-line))

(defun ramsay/eshell-clear-buffer ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(use-package eshell
  :commands (eshell shell-pop)
  :init
  (setq
   eshell-highlight-prompt nil
   eshell-buffer-shorthand t
   eshell-history-size 50000
   ;;  ;; auto truncate after 12k lines
   eshell-buffer-maximum-lines 12000
   eshell-hist-ignoredups t
   eshell-error-if-no-glob t
   eshell-glob-case-insensitive t
   eshell-scroll-to-bottom-on-input 'all
   eshell-list-files-after-cd t
   eshell-aliases-file (concat user-emacs-directory "eshell/alias")
   eshell-banner-message "Have fun with Eshell ♥  \n\n"
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

  (defun ramsay/eshell-prompt ()
    "Customize the Eshell prompt to mimic the Zsh prompt with colors."
    (let* ((user (propertize (getenv "USER") 'face '(:foreground "green" :weight bold)))
           (host (propertize (car (split-string (system-name) "\\.")) 'face '(:foreground "green" :weight bold)))
           (time (propertize (format-time-string "%H:%M:%S") 'face '(:foreground "blue")))
           (dir (abbreviate-file-name (eshell/pwd)))
           (dir-formatted (propertize dir 'face '(:foreground "gray")))
           (git-branch (when (magit-toplevel dir)
			 (magit-get-current-branch)))
           (dirty (when git-branch
                    (if (magit-anything-modified-p)
			"*"
                      ""))))
      (concat
       user "@" host " "
       "[" time "] "
       "[" dir-formatted "] "
       (if git-branch
           (concat
            (propertize (format "[%s" git-branch) 'face '(:foreground "green"))
            (propertize (format "%s" dirty) 'face '(:foreground "red"))
            (propertize "]" 'face '(:foreground "green"))
	    )
	 "")
       "\n"
       (propertize "->" 'face '(:foreground "green"))
       (propertize " " 'face 'default))))

  (setq eshell-prompt-function 'ramsay/eshell-prompt)
  (setq eshell-prompt-regexp "-> ")


  (add-hook 'eshell-mode-hook (lambda ()(eshell-cmpl-initialize)))
  (add-hook 'eshell-mode-hook (lambda ()(setq-local global-hl-line-mode nil)))
  (define-key eshell-mode-map [remap eshell-list-history] 'ramsay/eshell-clear-buffer)
  (define-key eshell-mode-map (kbd "C-c C-r") 'ramsay/esh-history)
  )

(message "loading init-shell")
(provide 'init-shell)
;;; init-shell.el ends here
