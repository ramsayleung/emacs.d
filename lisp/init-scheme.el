;;; package --- summary
;;; code:
;;; commentary:

(use-package geiser
  :ensure t
  :defer t
  :init (progn
	  (setq geiser-active-implementations '(mit))
	  (add-hook 'scheme-mode-hook 'geiser-mode)
	  (add-hook 'geiser-mode-hook #'samray/scheme-run-repl-for-code-complete-startup)
	  )
  )

(defun samray/scheme-run-repl-for-code-complete-startup ()
  "Run scheme repl for code auto-complete in the startup."
  (interactive)
  (if (get-buffer "* Mit REPL *") (message "Mit Repl has launched")
    (save-excursion
      (progn
	(run-mit)
	(delete-window)
	)))
  )

(provide 'init-scheme)

;;; init-scheme.el ends here
