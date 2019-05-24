;;; package --- Summary
;;; code:
;;; Commentary:

;; Code navigation,documentation lookup and completing for python
(setq tab-width 4)
(set-variable 'py-indent-offset 4)
(set-variable 'python-indent-guess-indent-offset nil)

(use-package python
  ;; :mode("\\.py\\'" . python-mode)
  :ensure t
  :config (progn
	    (setq
	     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
	     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
	     python-shell-completion-setup-code
	     "from IPython.core.completerlib import module_completion"
	     python-shell-completion-module-string-code
	     "';'.join(module_completion('''%s'''))\n"
	     python-shell-completion-string-code
	     "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
	    (when (executable-find "ipython")
	      ;;Ipython settings sections
	      (setq python-shell-interpreter "ipython"
		    python-shell-interpreter-args "--colors=Linux --profile=default"))
	    ;; https://github.com/jorgenschaefer/elpy/issues/887
	    (with-eval-after-load 'python
	      (defun python-shell-completion-native-try ()
		"Return non-nil if can trigger native completion."
		(let ((python-shell-completion-native-enable t)
		      (python-shell-completion-native-output-timeout
		       python-shell-completion-native-try-output-timeout))
		  (python-shell-completion-native-get-completions
		   (get-buffer-process (current-buffer))
		   nil "_"))))

	    
	    (defun samray/python-shell-send-buffer-switch ()
	      "Send buffer content to shell and switch to it in insert mode."
	      (interactive)
	      (python-shell-send-buffer)
	      (python-shell-switch-to-shell)
	      (evil-insert-state)
	      )

	    (defun samray/python-shell-send-defun-switch ()
	      "Send function content to shell and switch to it in insert mode."
	      (interactive)
	      (python-shell-send-defun nil)
	      (python-shell-switch-to-shell)
	      (evil-insert-state))

	    (defun samray/python-send-repl-echo-switch ()
	      "Pop and hide python with this function."
	      (interactive )
	      (let* ((python-buffer-name "*Python*")
		     (current-major-mode (with-current-buffer (current-buffer) major-mode))
		     (python-window (get-buffer-window python-buffer-name 'visible))
		     (python-code (if (region-active-p)
				      (buffer-substring-no-properties (region-beginning) (region-end))
				    (buffer-substring-no-properties (point-min) (point-max))))
		     (python-clear-repl (lambda ()
					  (progn
					    (evil-insert-state)
					    (comint-kill-input)
					    (when (string= 'python-mode current-major-mode)
					      (insert python-code))
					    (comint-send-input)
					    ))))
		;; Python buffer exists?
		(if (get-buffer python-buffer-name)
		    ;; Python buffer is visible?
		    (if python-window
		      ;; Buffer in current window is python buffer?
		      (if (string= (buffer-name (window-buffer)) python-buffer-name)
			  (if (not (one-window-p))
			      (progn (bury-buffer)
				     (delete-window)))
			;; If not, select window which points to python bufffer.
			(select-window python-window)
			(funcall python-clear-repl)
			)
		      ;; If python buffer is not visible, split a window and switch to it.
		      (progn
			;; Use `split-window-sensibly` to split window with policy
			;; If window cannot be split, force to split split window horizontally
			(when (not (split-window-sensibly))
			  (samray/split-window-below-and-move))
			(switch-to-buffer python-buffer-name)
			(funcall python-clear-repl)
			))
		  ;; If python buffer doesn't exist, create one
		  (samray/python-start-or-switch-repl)
		  (funcall python-clear-repl))))

	    (defun samray/python-start-or-switch-repl ()
	      "Start and/or switch to the REPL."
	      (interactive)
	      (let ((shell-process
		     (or (python-shell-get-process)
			 ;; `run-python' has different return values and different
			 ;; errors in different emacs versions. In 24.4, it throws an
			 ;; error when the process didn't start, but in 25.1 it
			 ;; doesn't throw an error, so we demote errors here and
			 ;; check the process later
			 (with-demoted-errors "Error: %S"
			   ;; in Emacs 24.5 and 24.4, `run-python' doesn't return the
			   ;; shell process
			   (call-interactively #'run-python)
			   (python-shell-get-process)))))
		(unless shell-process
		  (error "Failed to start python shell properly"))
		(pop-to-buffer (process-buffer shell-process))
		(evil-insert-state)))

	    (defun samray/python-shell-send-region-switch (start end)
	      "Send region  content from START to END to shell and switch to it in insert mode."
	      (interactive "r")
	      (python-shell-send-region start end)
	      (python-shell-switch-to-shell)
	      (evil-insert-state))

	    (defun samray/python-execute-file (arg)
	      "Execute a python script with ARG in a shell."
	      (interactive "P")
	      ;; set compile command to buffer-file-name
	      ;; universal argument put compile buffer in comint mode
	      (let ((universal-argument t)
		    (compile-command (format "python %s" (file-name-nondirectory
							  buffer-file-name))))
		(if arg
		    (call-interactively 'compile)
		  (compile compile-command t)
		  (with-current-buffer (get-buffer "*compilation*")
		    (inferior-python-mode)))))

	    (defun samray/python-execute-file-focus (arg)
	      "Execute a python script with ARG in a shell and switch to the shell buffer in
`insert state'."
	      (interactive "P")
	      (samray/python-execute-file arg)
	      (switch-to-buffer-other-window "*compilation*")
	      (end-of-buffer)
	      (evil-insert-state))

	    (defun samray/python-format-and-isort-buffer ()
	      "Format and isort buffer with autopep8 and isort."
	      (interactive)
	      (save-excursion
		(py-isort-buffer)
		(py-autopep8-buffer)
		)
	      )
	    (defun samray/setup-python-mode ()
	      "Make sure we add to hook locally so that we do not mess up
completion in other major-modes"
	      (add-hook 'company-completion-finished-hook #'samray/company-insert-parens-function t))

	    (defun samray/company-insert-parens-function (candidate)
	      "This part will be different for different backends."
	      (when (string= (plist-get (text-properties-at 0 candidate) :symbol) "f")
		(insert "()")
		(backward-char)))
	    (add-hook 'python-mode #'samray/setup-python-mode)
	    )
  )

(use-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

;; Use pep8 to format python file
(use-package py-autopep8
  :commands (py-autopep8 py-autopep8-buffer)
  :ensure t)

;;; Use isort to sort import
(use-package py-isort
  :ensure t
  :commands (py-isort-buffer))

;;; virtualenvwrapper for virtualenv
(use-package virtualenvwrapper
  :after python-mode
  :ensure t
  :init (progn
	  (add-hook 'eshell-mode-hook (lambda ()
					(venv-initialize-eshell)
					))
	  (add-hook 'shell-mode-hook (lambda ()
				       (venv-initialize-interactive-shells)
				       ))
	  ;; (add-hook 'venv-postmkvirtualenv-hook
	  ;; 	    (lambda () (shell-command "pip install nose flake8 jedi autopep8 isort")))
	  ))


(message "loading init-python")
(provide 'init-python)
;;; init-python.el ends here
