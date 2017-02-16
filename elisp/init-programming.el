;;; package --- Summary
;;; code:
;;; Commentary:
(use-package yasnippet
  :ensure t
  :commands (yas-expand-snippet yas-insert-snippet yas-new-snippet)
  :init (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config (progn
	    (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
	    ))
(use-package yaml-mode
  :ensure t
  :mode "\\.yml$")
(use-package json-mode
  :ensure t
  :mode "\\.json$")

(use-package nginx-mode
  :ensure t
  :commands (nginx-mode))

;; make Emacs use the $PATH set up by the user's shell
(use-package exec-path-from-shell
  :ensure t
  :demand t)

;; help you use shell easily on Emacs
(use-package shell-pop
  :ensure t
  :commands shell-pop
  :config (setq
	   shell-pop-window-position "bottom"
	   shell-pop-window-size 35
	   ))
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
	      (require 'virtualenvwrapper)
	      (venv-initialize-eshell)
	      (autoload 'epe-theme-lambda "eshell-prompt-extras")
	      (setq eshell-highlight-prompt nil
		    eshell-prompt-function 'epe-theme-lambda))
	    ))

;; Emacs extension to increate selected region by semantic units
(use-package expand-region
  :ensure t
  :commands er/expand-region
  )

;;; Evil is not especilly useful in the terminal,so
(evil-set-initial-state 'term-mode 'emacs)

;;; Yanking in the term-mode doesn't quit work
;;; The text from the paste appears in the buffer but isn't
;;; sent to the shell
(defun samray/term-paste (&optional string)
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string
     (current-kill 0)))
  )
(add-hook 'term-mode-hook
	  (lambda ()
	    (goto-address-mode)
	    (setq yas-dont-activate t)))

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
(provide 'init-programming)
;;; init-programming.el ends here
