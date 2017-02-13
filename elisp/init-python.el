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
	    (setq fci-rule-color "darkblue")
	    (setq-default fill-column 79)
	    (add-hook 'python-mode-hook 'fci-mode)))

;;; similar with fill-column-indicator,but a little bit different
(use-package column-enforce-mode
  :ensure t
  :diminish column-enforce-mode
  :init
  (setq column-enforce-column 79)
  :config
  (progn
    (add-hook 'prog-mode-hook 'column-enforce-mode)))
;;; To fix issue that there is weird eshell output with ipython
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")
(provide 'init-python)
;;; init-python.el ends here
