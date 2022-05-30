;;; package --- Summary
;;; code:
;;; Commentary:

;; Code navigation,documentation lookup and completing for python
(setq tab-width 4)
(set-variable 'py-indent-offset 4)
(set-variable 'python-indent-guess-indent-offset nil)

(use-package python-mode
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
			)
  )

;; Use pep8 to format python file
(use-package py-autopep8
  :commands (py-autopep8 py-autopep8-buffer)
  :ensure t)

;;; Use isort to sort import
(use-package py-isort
  :ensure t
  :commands (py-isort-buffer))

(message "loading init-python")
(provide 'init-python)
;;; init-python.el ends here
