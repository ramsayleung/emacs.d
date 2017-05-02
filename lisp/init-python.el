;;; package --- Summary
;;; code:
;;; Commentary:

;; Code navigation,documentation lookup and completing for python
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))
(use-package python
  :mode("\\.py\\'" . python-mode)
  :ensure t
  :init (progn
          (add-hook 'python-mode-hook (lambda () (highlight-indentation-mode 0)))
          )
  )

(use-package anaconda-mode
  :defer t
  :ensure t
  :init(progn
	 (add-hook 'python-mode-hook 'anaconda-mode)
	 (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
	 ))

;; Emacs python development Environment
(use-package elpy
  :ensure t
  :defer t
  :init (add-hook 'python-mode-hook 'elpy-mode)
  :config(progn
	   (elpy-enable)
	   (elpy-use-ipython)
	   )
  )

;; Use pep8 to format python file
(use-package py-autopep8
  :defer t
  :ensure t
  ;; :init(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  )

(use-package py-isort
  :ensure t
  :commands (py-isort-buffer py-isort-region)
  )

;;; similar with fill-column-indicator,but a little bit different
(use-package column-enforce-mode
  :ensure t
  :diminish column-enforce-mode
  :defer t
  :init
  (setq column-enforce-column 79)
  (add-hook 'prog-mode-hook 'column-enforce-mode))

;;; virtualenvwrapper for virtualenv
(use-package virtualenvwrapper
  :ensure t
  :init (progn
	  ( add-hook 'python-mode-hook (lambda()
					 (venv-initialize-interactive-shells)
					 (venv-initialize-eshell)))
	  ;; (add-hook 'venv-postmkvirtualenv-hook
	  ;; 	    (lambda () (shell-command "pip install nose flake8 jedi autopep8 isort")))
	  ))

;;; To fix issue that there is weird eshell output with ipython
;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "--simple-prompt -i")

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
    (py-autopep8-buffer))
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

(provide 'init-python)
;;; init-python.el ends here
