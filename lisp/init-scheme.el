;;; package --- summary
;;; code:
;;; commentary:

(use-package scheme-mode
  ;; I am not sure why `:mode ("\\.rkt\\'" "\\.scm\\'")` doesn't work
  ;; So I choose to implement it with `':hook`, the tricky way
  :hook (scheme-mode . racket-mode))

(use-package racket-mode
  :ensure t
  :hook (racket-mode . racket-xp-mode)
  :mode ("\\.rkt\\'" "\\.scm\\'"))

(message "loading init-scheme")
(provide 'init-scheme)

;;; init-scheme.el ends here
