;;; package --- Summary
;;; code:
;;; Commentary:

(use-package window-numbering
  :config(progn
	   (window-numbering-mode t)))
(use-package smartparens
  :config
  (progn
    (smartparens-global-mode t)
    ;; show single quote "'" in emacs and lisp-interaction-mode instead of single quote pair "''"
    (sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "'" nil :actions nil)
    )
  )
(use-package hungry-delete
  :config(hungry-delete-mode t))
(provide 'init-better-editing)
;;; init-better-editing.el ends here
