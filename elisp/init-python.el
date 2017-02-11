;;; package --- Summary
;;; code:
;;; Commentary:

;; Code navigation,documentation lookup and completing for python
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(use-package anaconda-mode
  :ensure t
  :config(progn
	   (add-hook 'python-mode-hook 'anaconda-mode)
	   (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
	   ))
;; Emacs python development Environment
(use-package elpy
  :ensure t
  :config(progn
	   (elpy-enable)
	   (elpy-use-ipython)
	   )
  )

;; Use pep8 to format python file
(use-package py-autopep8
  :ensure t
  :config(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))
;; company-mode completion back-end for python JEDI
(use-package company-jedi
  :ensure t
  :config(progn
	   (add-hook 'python-mode-hook
		     (lambda()(add-to-list 'company-backends 'company-jedi)))
	   ))
;; (use-package jedi
;;   :ensure t
;;   :config(add-hook 'python-mode-hook 'jedi:setup))
;; Sort import with isort
(use-package py-isort
  :ensure t
  :config(add-hook 'before-save-hook 'py-isort-before-save))

(use-package fill-column-indicator
  :ensure t
  :config (progn
	    (setq fci-rule-width 1)
	    (setq fci-rule-color "skyblue")
	    (setq-default fill-column 79)
	    (add-hook 'python-mode-hook 'fci-mode)))

;;; To fix issue that there is weird eshell output with ipython
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")
(provide 'init-python)
;;; init-python.el ends here
