;;; package --- Summary
;;; Code:
;;; Commentary:
(use-package evil
  :config
  (evil-mode t))
;; (evil-mode 1)
(use-package evil-surround
  :config
  (global-evil-surround-mode t))

(use-package evil-leader
  :config(progn
	   (global-evil-leader-mode)
	   (evil-leader/set-leader "<SPC>")
	   (evil-leader/set-key
	     "; ;" 'evilnc-comment-or-uncomment-lines
	     "a d" 'dired
	     "b b" 'helm-mini
	     "b l" 'switch-to-buffer
	     "b o" 'occur-dwim
	     "e l" 'flycheck-list-errors
	     "e n" 'flycheck-next-error
	     "e p" 'flycheck-previous-error
	     "f e d" 'open-my-file
	     "f f" 'counsel-find-file
	     "f r" 'recentf-open-files
	     "f s" 'save-buffer
	     "h d k" 'describe-key
	     "h d f" 'describe-function
	     "h d d" 'apropos-documentation
	     "g s" 'magit-status
	     "j j" 'avy-goto-char
	     "o a" 'org-agenda
	     "q s" 'save-buffers-kill-terminal
	     "p s" 'helm-ag-project-root
	     "v" 'er/expand-region
	     "s i" 'iedit-mode
	     "s s" 'swiper
	     "w d" 'delete-window
	     "w D" 'delete-other-windows
	     "w -" 'split-window-below
	     "w /" 'split-window-right
	     "w h" 'evil-window-left
	     "w j" 'evil-window-down
	     "w k" 'evil-window-up 
	     "w l" 'evil-window-right
	     "0"  'select-window-0
	     "1"  'select-window-1
	     "2"  'select-window-2
	     "3"  'select-window-3
	     )
	   (evil-add-hjkl-bindings occur-mode-map 'emacs
	     (kbd "/")       'evil-search-forward
	     (kbd "n")       'evil-search-next
	     (kbd "N")       'evil-search-previous
	     (kbd "C-d")     'evil-scroll-down
	     (kbd "C-u")     'evil-scroll-up
	     (kbd "C-w C-w") 'other-window))
  :bind(
	:map evil-normal-state-map
	     ("C-e" . evil-end-of-line)
	     ("C-a" . evil-beginnin-of-line)
	     :map evil-visual-state-map
	     ("C-e" . evil-end-of-line)
	     ("C-a" . evil-beginning-of-line)
	     :map evil-insert-state-map
	     ("C-e" . end-of-line)
	     ("C-a" . beginning-of-line)
	     :map evil-motion-state-map
	     ("C-e" . evil-end-of-line)
	     ("C-a" . evil-beginning-of-line))
  )
(use-package evil-nerd-commenter
  :config
  (evil-leader/set-key
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line
    ))
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(evilnc-default-hotkeys)
(provide 'init-evil)
;;; init-evil.el ends here
