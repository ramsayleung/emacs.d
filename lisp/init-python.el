;;; package --- Summary
;;; code:
;;; Commentary:

;; Code navigation,documentation lookup and completing for python
(setq tab-width 4)
(set-variable 'py-indent-offset 4)
(set-variable 'python-indent-guess-indent-offset nil)

(use-package python
  :mode("\\.py\\'" . python-mode)
  :ensure t
  :config (progn
	    (when (executable-find "ipython3")
	      ;;Ipython settings sections
	      (setq python-shell-interpreter "ipython3"
		    python-shell-interpreter-args " -i "))
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
	    )
  )

;; Use pep8 to format python file
(use-package py-autopep8
  :commands (py-autopep8 py-autopep8-buffer)
  :ensure t
  )

(use-package py-isort
  :ensure t
  :commands (py-isort-buffer py-isort-region)
  )

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
  (evil-insert-state)
  )

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
  "Execute a python script in a shell."
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
  "Execute a python script in a shell and switch to the shell buffer in
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

(message "loading init-python")
(provide 'init-python)
;;; init-python.el ends here
