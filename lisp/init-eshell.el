;;; package --- Summary -*- lexical-binding: t; -*-
;;; code:
;;; Commentary:

;;; https://www.emacswiki.org/emacs/EshellFunctions
(defun samray/eshell-maybe-bol ()
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
    (let ((unpack-command(concat command " " file " " (mapconcat 'identity args " "))))
      (eshell/printnl "Unpack command: " unpack-command)
      (eshell-command-result unpack-command))
    ))


(defun samray/eshell-sudo-toggle ()
  "Add/Remove sudo in the begining of command line."
  (interactive)
  (save-excursion
    (let ((commands (buffer-substring-no-properties
		     (eshell-bol) (point-max))))
      (if (string-match-p "^sudo " commands)
	  (progn
	    (eshell-bol)
	    (while (re-search-forward "sudo " nil t)
	      (replace-match "" t nil)))
	(progn
	  (eshell-bol)
	  (insert "sudo ")
	  )))))
;;; Inspire by http://blog.binchen.org/posts/use-ivy-mode-to-search-bash-history.html
(defun samray/parse-bash-history ()
  "Parse the bash history."
  (interactive)
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
      bash_history)))

(defun samray/parse-zsh-history ()
  "Parse the bash history."
  (interactive)
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
      zsh_history)))

(defun samray/esh-history ()
  "Interactive search eshell history."
  (interactive)
  (require 'em-hist)
  (save-excursion
    (let* ((start-pos (eshell-beginning-of-input))
	   (input (eshell-get-old-input))
	   (esh-history (when (> (ring-size eshell-history-ring) 0)
			  (ring-elements eshell-history-ring)))
	   (all-shell-history (append esh-history (samray/parse-zsh-history) (samray/parse-bash-history)))
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


(defun samray/eshell-clear-buffer ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun samray/eshell-fasd-z (&rest args)
  "Use fasd to change directory more effectively by passing ARGS."
  (setq args (eshell-flatten-list args))
  (let* ((fasd (concat "fasd " (car args)))
         (fasd-result (shell-command-to-string fasd))
         (path (replace-regexp-in-string "\n$" "" fasd-result))
         )
    (eshell/cd path)
    (eshell/echo path)
    ))

;; help you use shell easily on Emacs
(use-package shell-pop
  :ensure t
  :commands shell-pop
  :config (progn
	    (setq
	     shell-pop-window-position "bottom"
	     shell-pop-window-size 35
	     )
	    ))

(defun samray/shell-pop-dwim()
  "Switch to shell mode and insert mode"
  (shell-pop)
  (evil-insert-state)
  )

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
	    ))

(use-package eshell-prompt-extras
  :ensure t
  :after eshell
  :init (progn
	  (setq eshell-highlight-prompt nil
		eshell-prompt-function 'epe-theme-lambda)
	  ))
(defun setup-company-eshell-autosuggest ()
  (with-eval-after-load 'company
    (setq-local company-backends '(company-eshell-autosuggest))
    (setq-local company-frontends '(company-preview-frontend))))

(add-hook 'eshell-mode-hook 'setup-company-eshell-autosuggest)

(use-package company-eshell-autosuggest
  :load-path "~/.emacs.d/additional-packages/company-eshell-autosuggest.el")
(provide 'init-eshell)
;;; init-eshell.el ends here
