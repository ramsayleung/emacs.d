;;; package --- Summary
;;; Code:
;;; Commentary:
;;; esc quits
(use-package evil
  :ensure t
  :config
  (progn
    (evil-mode t)
    (evil-set-initial-state 'calendar-mode 'emacs)
    )
  )

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode t)
  )

(use-package evil-nerd-commenter
  :ensure t
  :config
  )
;; evil keybinding in magit
(use-package evil-magit
  :ensure t)

;; Vim matchit ported into Emacs
(use-package evil-matchit
  :ensure t)
(use-package evil-multiedit
  :ensure t
  :config(evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
  )
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(evilnc-default-hotkeys)
(provide 'init-evil)
;;; init-evil.el ends here
