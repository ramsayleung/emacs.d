;;; package --- Summary -*- lexical-binding: t -*-
;;; code:
;;; Commentary:

(use-package ace-window
  :ensure t
  :commands (ace-window)
  )

(use-package wgrep
  :ensure t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.3)
  (which-key-mode t)
  )

;;; Highlight delimiter such as parenthese,brackets or braces
;;; according to their depth
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init(add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;;; treat undo history as a tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :commands undo-tree-visualize
  :init
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (setq undo-tree-auto-save-history t)
  :config (global-undo-tree-mode t))

;;; Emacs always for confirmation whether we really wanna open
;;; large file.However,the default limit is so low,so it prompt often
;;; So increase limit to solve it.
(setq large-file-warning-threshold (* 15 1024 1024)) ;15MB

(when (fboundp 'winner-mode)
  (winner-mode t))

;;; Enable narrow to region feature if it is disable
(put 'narrow-to-region 'disabled nil)

(require 'recentf)
(setq recentf-max-saved-items 1000)
;; "Cleanup verification" is the process where recentf-mode checks
;; whether files in its recent files list still exist on disk, and
;; removes any that don't, which causes TRAMP to attempt remote connections
;; Keep TRAMP files without verifying, verify local files normally
(defun ramsay/recentf-keep-file (file)
  "Keep TRAMP files without verification, verify local files exist."
  (or (file-remote-p file)
      (file-readable-p file)))

(setq recentf-keep '(ramsay/recentf-keep-file))
(recentf-mode 1)

;; Prevent Tramp hangs
(setq auto-revert-remote-files nil)           ; Don't auto-revert remote files
(setq remote-file-name-inhibit-locks t)       ; Don't lock remote files
(setq remote-file-name-inhibit-cache nil)     ; Cache remote file info
(setq tramp-verbose 1)                        ; Less verbose (faster)

;;; Auto-refresh buffers when files have changed on disk
(global-auto-revert-mode t)

(defun ramsay/split-window-right-and-move ()
  "Split window vertically and move to the other window."
  (interactive)
  (split-window-right)
  (other-window 1)
  )

(defun ramsay/split-window-below-and-move ()
  "Split window horizontally and move to the other window!"
  (interactive)
  (split-window-below)
  (other-window 1)
  )
(message "loading init-better-editing")
(provide 'init-better-editing)
;;; init-better-editing.el ends here
