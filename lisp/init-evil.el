;;; package --- Summary
;;; Code:
;;; Commentary:
;;; esc quits

(use-package evil
  :ensure t
  :demand t
  :init (progn
	  (setq evil-want-C-u-scroll t)
	  )
  :config
  (progn
    (evil-mode t)
    (evil-set-initial-state 'calendar-mode 'emacs)
    (evil-set-initial-state 'pdf-view-mode 'emacs)
    (evil-set-initial-state 'dired-mode 'emacs)
    (evil-set-initial-state 'imenu-list-major-mode 'emacs)
    (evil-set-initial-state 'artist-mode 'emacs)
    (evil-set-initial-state 'eshell-mode 'insert)
    (evil-set-initial-state 'term-mode 'emacs)
    ;;; modify evil-state-tag
    (setq evil-normal-state-tag   (propertize "[Normal]")
	  evil-emacs-state-tag    (propertize "[Emacs]")
	  evil-insert-state-tag   (propertize "[Insert]")
	  evil-motion-state-tag   (propertize "[Motion]")
	  evil-visual-state-tag   (propertize "[Visual]")
	  evil-operator-state-tag (propertize "[Operator]"))

    ))

(use-package evil-surround
  :ensure t
  :commands evil-mode
  :init
  (global-evil-surround-mode t)
  )

(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator)
  :ensure t
  )
    
;; Vim matchit ported into Emacs
(use-package evil-matchit
  :commands evil-mode
  :ensure t
  :init (global-evil-matchit-mode 1))

(use-package evil-mc
  :ensure t
  :diminish evil-mc-mode
  :commands evil-mode
  :init
  (global-evil-mc-mode t)
  )


;; (define-key evil-insert-state-map [escape] 'evil-normal-state)
;; (evilnc-default-hotkeys)
(provide 'init-evil)
;;; init-evil.el ends here
