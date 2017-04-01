;;; package --- Summary
;;; code:
;;; Commentary:
(defun af-eshell-here ()
  "Go to eshell and set current directory to the buffer's directory."
  (interactive)
  (let ((dir (file-name-directory (or (buffer-file-name)
                                      default-directory))))
    (if (get-buffer "*eshell*")
	(if (string= (buffer-name) "*eshell*")(delete-window (selected-window))
	  (progn
	    (switch-to-buffer "*eshell*"))
	  )
      (progn
	(split-window-vertically)
	(other-window 1)
	(eshell)
	(eshell/pushd ".")
	(cd dir)
	(goto-char (point-max))
	(eshell-kill-input)
	(eshell-send-input)
	))))

(defun samray/eshell-clear-buffer ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
;; (add-hook 'eshell-mode-hook
;; 	  '(lambda()
;; 	     (local-set-key (kbd "C-l") 'samray/eshell-clear-buffer)))

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
  :config (progn
	    (use-package em-cmpl :ensure nil)
	    (use-package em-prompt :ensure nil)
	    (use-package em-term :ensure nil)
	    (setq
	     eshell-highlight-prompt nil
	     eshell-buffer-shorthand t
	     eshell-cmpl-ignore-case t
	     eshell-cmpl-cycle-completions nil
	     eshell-history-size 500
	     ;; auto truncate after 12k lines
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
	    
	    (defun my/truncate-eshell-buffers ()
	      "Truncates all eshell buffers"
	      (interactive)
	      (save-current-buffer
		(dolist (buffer (buffer-list t))
		  (set-buffer buffer)
		  (when (eq major-mode 'eshell-mode)
		    (eshell-truncate-buffer)))))

	    ;; After being idle for 5 seconds, truncate all the eshell-buffers if
	    ;; needed. If this needs to be canceled, you can run `(cancel-timer
	    ;; my/eshell-truncate-timer)'
	    (setq samray/eshell-truncate-timer
		  (run-with-idle-timer 5 t #'my/truncate-eshell-buffers))

	    (when (not (functionp 'eshell/rgrep))
	      (defun eshell/rgrep (&rest args)
		"Use Emacs grep facility instead of calling external grep."
		(eshell-grep "rgrep" args t)))
	    (defun samray/setup-eshell ()
	      "Eshell init setup"
	      (interactive)
	      (hl-line-mode -1)
	      )
	    (add-hook 'eshell-mode-hook
		      (lambda ()
			(samray/setup-eshell)
			(eshell-cmpl-initialize)
			))
	    ))

;;; Display extra information and color for eshll prompt
(use-package eshell-prompt-extras
  :ensure t
  :config (progn
	    (with-eval-after-load "esh-opt"
	      (use-package virtualenvwrapper :ensure t)
	      (venv-initialize-eshell)
	      (autoload 'epe-theme-lambda "eshell-prompt-extras")
	      (setq eshell-highlight-prompt nil
		    eshell-prompt-function 'epe-theme-lambda))
	    ))


(provide 'init-eshell)
;;; init-eshell.el ends here
