;;; package --- Summary
;;; Code:
;;; Commentary:
;;; esc quits

(use-package evil
  :ensure t
  :demand t
  :init (progn
	  (setq evil-respect-visual-line-mode t)
	  (setq evil-want-C-u-scroll t))
  :config
  (evil-mode t)
  (evil-set-initial-state 'calendar-mode 'emacs)
  (evil-set-initial-state 'pdf-view-mode 'emacs)
  (evil-set-initial-state 'Info-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'imenu-list-major-mode 'emacs)
  (evil-set-initial-state 'artist-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'insert)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'comint-mode 'emacs)
  (evil-set-initial-state 'neotree-mode 'emacs)
  (evil-set-initial-state 'deadgrep-mode 'emacs)
  (evil-set-initial-state 'helpful-mode 'emacs)
  (evil-set-initial-state 'rectangle-mark-mode 'emacs)
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
  (evil-set-initial-state 'ledger-report-mode 'emacs)
  (evil-set-initial-state 'compilation-mode 'emacs)
  (setq-default evil-buffer-regexps
              '(("**testing snippet:" . insert)
                ("*compile*" . normal)
                ("*Org Src" . insert)
                ("*Org Export Dispatcher*" . insert)
                ("*Async Shell Command*" . normal)
		("\\*Go-Translate*\\*" . emacs)
		("\\*Warnings*\\*" . emacs)
		("")
                ("^ \\*load\\*")))
    ;;; modify evil-state-tag
  (setq evil-normal-state-tag   (propertize "[Normal]")
	evil-emacs-state-tag    (propertize "[Emacs]")
	evil-insert-state-tag   (propertize "[Insert]")
	evil-motion-state-tag   (propertize "[Motion]")
	evil-visual-state-tag   (propertize "[Visual]")
	evil-operator-state-tag (propertize "[Operator]"))
  (setq evil-insert-state-cursor '(box))
  (setq evil-undo-system 'undo-tree)
  )


(use-package evil-surround
  :ensure t
  :init(progn
	 (global-evil-surround-mode t)
	 ))

(message "loading init-evil")
(provide 'init-evil)
;;; init-evil.el ends here
