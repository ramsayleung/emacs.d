;;; package --- Summary -*- lexical-binding: t; -*-
;;; code:
;;; Commentary:

;;; https://www.emacswiki.org/emacs/EshellFunctions
(defun ramsay/eshell-maybe-bol ()
  "Go to the beginning of command line,or begining of line."
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
	(beginning-of-line))))

(defun eshell/unpack (file &rest args)
  "Unpack FILE with ARGS using default command."
  (let ((command (some (lambda (x)
                         (if (string-match-p (car x) file)
                             (cadr x)))
                       '((".*\.tar.bz2" "tar xjf")
                         (".*\.tar.gz" "tar xzf")
                         (".*\.bz2" "bunzip2")
                         (".*\.rar" "unrar x")
                         (".*\.gz" "gunzip")
                         (".*\.tar" "tar xf")
                         (".*\.tbz2" "tar xjf")
                         (".*\.tgz" "tar xzf")
                         (".*\.zip" "unzip")
                         (".*\.Z" "uncompress")
                         (".*" "echo 'Could not unpack the file:'")))))
    (let ((unpack-command (concat command " " file " " (mapconcat 'identity args " "))))
      (eshell/printnl "Unpack command: " unpack-command)
      (eshell-command-result unpack-command))
    ))


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
  :init (progn
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
	   ;;  ;; eshell-banner-message "What would you like to do?\n\n"
	   )
	  ;; Visual commands
	  (setq eshell-visual-commands '("vi" "screen" "top" "less" "more" "lynx"
					 "ncftp" "pine" "tin" "trn" "elm" "vim"
					 "nmtui" "alsamixer" "htop" "el" "elinks"
					 ))
	  (setq eshell-visual-subcommands '(("git" "log" "diff" "show")))

	  )
  :config (progn
	    (when (not (functionp 'eshell/rgrep))
	      (defun eshell/rgrep (&rest args)
		"Use Emacs grep facility instead of calling external grep."
		(eshell-grep "rgrep" args t)))
	    (add-hook 'eshell-mode-hook
		      (lambda ()(eshell-cmpl-initialize)))
	    (add-hook 'eshell-mode-hook (lambda ()
					  (setq-local global-hl-line-mode nil)))
	    )
  :bind (:map eshell-mode-map
	      ([remap kill-region] . ramsay/kill-word-backward)
	      ("C-w" . ramsay/kill-word-backward)
	      ([remap evil-insert-digraph] . paredit-kill)
	      ("C-k" . paredit-kill)
	      ([remap evil-paste-from-register] . ramsay/esh-history)
	      ("C-r" . ramsay/esh-history)
	      ("C-l" . ramsay/eshell-clear-buffer))
  )

;;; Steal from
;;; https://www.reddit.com/r/emacs/comments/7a14cp/fishlike_autosuggestions_in_eshell/
;;; Make Eshell complete like fish with history from bash, zsh, eshell
(require 'company)
(if (ramsay/linux-p)
    (require 'cl-lib)
  (require 'cl-extra))
(defun ramsay/company-eshell-autosuggest-candidates (prefix)
  "Select the first eshell history candidate with prefix PREFIX."
  (let* ((esh-history (when (> (ring-size eshell-history-ring) 0)
			(ring-elements eshell-history-ring)))
	 (all-shell-history (append esh-history (ramsay/parse-zsh-history) (ramsay/parse-bash-history)))
	 (history
          (delete-dups
           (mapcar (lambda (str)
                     (string-trim (substring-no-properties str)))
                   all-shell-history)))
         (most-similar (cl-find-if
                        (lambda (str)
                          (string-prefix-p prefix str))
                        history)))
    (when most-similar
      `(,most-similar))))

(defun ramsay/company-eshell-autosuggest--prefix ()
  "Get current eshell input."
  (let ((prefix
         (string-trim-left
          (buffer-substring-no-properties
           (save-excursion
             (eshell-bol))
           (save-excursion (end-of-line) (point))))))
    (if (not (string-empty-p prefix))
        prefix
      'stop)))

(defun ramsay/company-eshell-autosuggest (command &optional arg &rest ignored)
  "`company-mode' backend to provide eshell history suggestion."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-eshell))
    (prefix (and (eq major-mode 'eshell-mode)
                 (ramsay/company-eshell-autosuggest--prefix)))
    (candidates (ramsay/company-eshell-autosuggest-candidates arg))))

(defun ramsay/setup-company-eshell-autosuggest ()
  "Set up company completion for Eshell."
  (with-eval-after-load 'company
    (setq-local company-backends '(ramsay/company-eshell-autosuggest))
    (setq-local company-frontends '(company-preview-frontend))))

(add-hook 'eshell-mode-hook 'ramsay/setup-company-eshell-autosuggest)

(message "loading init-eshell")
(provide 'init-eshell)
;;; init-eshell.el ends here
