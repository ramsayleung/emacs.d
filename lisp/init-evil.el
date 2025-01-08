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
  (setq evil-default-state 'emacs)
  (evil-set-initial-state 'prog-mode 'normal)
  (evil-set-initial-state 'fundamental-mode 'normal)
    ;;; modify evil-state-tag
  (setq evil-normal-state-tag   (propertize "[Normal]")
	evil-emacs-state-tag    (propertize "[Emacs]")
	evil-insert-state-tag   (propertize "[Insert]")
	evil-motion-state-tag   (propertize "[Motion]")
	evil-visual-state-tag   (propertize "[Visual]")
	evil-operator-state-tag (propertize "[Operator]"))
  (setq evil-insert-state-cursor '(box))
  )


(use-package evil-surround
  :ensure t
  :init(progn
	 (global-evil-surround-mode t)
	 ))

(message "loading init-evil")
(provide 'init-evil)
;;; init-evil.el ends here
