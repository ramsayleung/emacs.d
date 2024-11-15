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
  :config 
	;; https://github.com/jorgenschaefer/elpy/issues/887
	(defun python-shell-completion-native-try ()
		"Return non-nil if can trigger native completion."
		(let ((python-shell-completion-native-enable t)
		      (python-shell-completion-native-output-timeout
		       python-shell-completion-native-try-output-timeout))
		  (python-shell-completion-native-get-completions
		   (get-buffer-process (current-buffer))
		   nil "_")))
  )

(use-package pyvenv
  :ensure t
  :commands pyvenv-activate)

(message "loading init-python")
(provide 'init-python)
;;; init-python.el ends here
