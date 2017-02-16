;;; package --- Summary
;;; code:
;;; Commentary:

;; Code navigation,documentation lookup and completing for python
(use-package python
  :mode("\\.py\\'" . python-mode)
  :ensure t
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
  :init(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))
;; (use-package jedi
;;   :ensure t
;;   :config(add-hook 'python-mode-hook 'jedi:setup))
;; Sort import with isort
(use-package py-isort
  :ensure t
  :defer t
  :init(add-hook 'before-save-hook 'py-isort-before-save))

(use-package fill-column-indicator
  :ensure t
  :commands (fci-mode)
  :init
  (add-hook 'python-mode-hook 'fci-mode)
  :config
  (setq fci-rule-width 1)
  (setq-default fill-column 79)
  (setq fci-rule-color "spring green")
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
  :defer t
  :init (add-hook 'python-mode-hook (lambda()
				      (venv-initialize-interactive-shells)
				      (venv-initialize-eshell))
		  ))

;;; To fix issue that there is weird eshell output with ipython
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")

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

(provide 'init-python)
;;; init-python.el ends here
