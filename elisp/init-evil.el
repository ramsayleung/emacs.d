;;; package --- Summary
;;; Code:
;;; Commentary:
;;; esc quits
(use-package evil
  :ensure t
  :demand t
  :config
  (progn
    (evil-mode t)
    (evil-set-initial-state 'calendar-mode 'emacs)
    (evil-set-initial-state 'pdf-view-mode 'emacs)
    (evil-set-initial-state 'imenu-list-major-mode 'emacs)
    ))
(use-package evil-surround
  :ensure t
  :commands evil-mode
  :init
  (global-evil-surround-mode t)
  )

(use-package evil-nerd-commenter
  :commands evil-mode
  :ensure t
  )
;; evil keybinding in magit
(use-package evil-magit
  :commands evil-mode
  :ensure t)

;; Vim matchit ported into Emacs
(use-package evil-matchit
  :commands evil-mode
  :ensure t)

;;; Escape everything
(use-package evil-escape
  :after evil
  :ensure t
  :config (setq-default evil-escape-key-sequence "jk"))

(use-package evil-multiedit
  :ensure t
  :commands evil-mode
  :config(evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
  )
;;; refactor variable within a function
;;; http://blog.binchen.org/posts/how-to-refactorrename-a-variable-name-in-a-function-efficiently.html
(defun samray/evilcvn-change-symbol-in-defun ()
  "Mark the region in defun (definition of function) and use string replacing
UI in evil-mode to replace the symbol under cursor"
  (interactive)
  (let ((old (thing-at-point 'symbol)))
    (mark-defun)
    (unless (evil-visual-state-p)
      (evil-visual-state))
    (evil-ex (concat "'<,'>s/" (if (= 0 (length old)) "" "\<\(") old (if (= 0 (length old)) "" "\)\>/"))))
  )



;; (define-key evil-insert-state-map [escape] 'evil-normal-state)
(evilnc-default-hotkeys)
(provide 'init-evil)
;;; init-evil.el ends here
