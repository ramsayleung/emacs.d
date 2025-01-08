;;; package --- Summary: -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;; Code Completion
(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.05)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert) ; insert previewed candidate
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ([remap evil-complete-next] . corfu-next)
              ([remap evil-complete-previous] . corfu-previous)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("RET" . corfu-insert)
	      )

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)) ; Popup completion info

(use-package corfu-terminal
  :init (ramsay/vc-install :fetcher "codeberg" :repo "akib/emacs-corfu-terminal")
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1))
  )

(message "loading init-auto-completion")
(provide 'init-auto-completion)
;;; init-auto-completion.el ends here
