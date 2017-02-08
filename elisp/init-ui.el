;;; package --- Summary
;;; Code:
;;; Commentary:
;;turn off tool bar
(tool-bar-mode -1)
;; turn off file scroll bar
(scroll-bar-mode -1)
;; no menubar
(menu-bar-mode -1)
;; turn off startup help menu
(setq inhibit-splash-screen t)
;; show line number
(global-linum-mode 1)

;; highlight current line
(global-hl-line-mode t)
(load-theme 'zenburn t)
;;(load-theme 'dracula t)
;;(load-theme 'monokai t)
;; make emacs full-screen at startup
(setq initial-frame-alist (quote ((fullscreen . maximized))))
(setq-default cursor-type 'bar)
(use-package powerline
  :config
  (powerline-default-theme)
  (setq powerline-default-separator 'nil)
  )

(provide 'init-ui)
;;; init-ui.el ends here
