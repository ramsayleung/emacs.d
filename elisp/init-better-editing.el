;;; package --- Summary
;;; code:
;;; Commentary:

(use-package window-numbering
  :ensure t
  :config(progn
	   (window-numbering-mode t)))
(use-package smartparens
  :ensure t
  :config
  (progn
    (smartparens-global-mode t)
    ;; show single quote "'" in emacs and lisp-interaction-mode instead of single quote pair "''"
    (sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "'" nil :actions nil)
    )
  )

;; delete spaces at once
(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode t))
(use-package which-key
  :ensure t
  :config(progn
	   (which-key-mode t)
	   (setq which-key-idle-delay 0.3)
	   ))
(use-package iedit
  :ensure t)
(provide 'init-better-editing)
;;; init-better-editing.el ends here
