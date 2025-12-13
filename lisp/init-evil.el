;;; package --- Summary
;;; Code:
;;; Commentary:
;;; esc quits

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-want-C-u-scroll t)
  (setq evil-default-state 'emacs)
  :config
  (evil-mode t)
  (dolist (mode '(mhtml-mode css-mode text-mode markdown-mode org-mode prog-mode fundamental-mode conf-unix-mode))
    (add-to-list 'evil-normal-state-modes mode))

  (dolist (mode '(help-mode Info-mode))
    (evil-set-initial-state mode 'emacs))

;;; modify evil-state-tag
  (setq evil-normal-state-tag   (propertize "[Normal]")
	    evil-emacs-state-tag    (propertize "[Emacs]")
	    evil-insert-state-tag   (propertize "[Insert]")
	    evil-motion-state-tag   (propertize "[Motion]")
	    evil-visual-state-tag   (propertize "[Visual]")
	    evil-operator-state-tag (propertize "[Operator]"))
  (setq evil-insert-state-cursor '(box)))


(use-package evil-surround
  :ensure t
  :init
  (global-evil-surround-mode t)
  )

(message "loading init-evil")
(provide 'init-evil)
;;; init-evil.el ends here
