;;; package --- summary
;;; code:
;;; commentary:

(use-package geiser
  :ensure t
  :defer t
  :init
  (setq geiser-active-implementations '(guile))
  (add-hook 'scheme-mode-hook 'geiser-mode)
  (use-package geiser-guile
    :ensure t
    :defer t)
  )

(use-package racket-mode
  :mode("\\.rkt\\'" . racket-mode)
  :ensure t
  )

(message "loading init-scheme")
(provide 'init-scheme)

;;; init-scheme.el ends here
