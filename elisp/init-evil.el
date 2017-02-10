;;; package --- Summary
;;; Code:
;;; Commentary:
;;; esc quits
(use-package evil
  :ensure t
  :config
  (progn
    (evil-mode t)
    )
  )

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode t)
  )

;; (use-package evil-leader
;;   :ensure t
;;   :config(progn
;; 	   (global-evil-leader-mode)
;;(evil-leader/set-leader "<SPC>")
;;(evil-leader/set-key
  ;;";" 'evilnc-comment-operator
  ;;"'" 'shell-pop
  ;;"TAB" 'samray/alternate-buffer
  ;;"a d" 'dired
  ;;"a o a" 'org-agenda
  ;;"b b" 'ivy-switch-buffer
  ;; 	     "b d" 'kill-this-buffer
  ;; 	     "b l" 'switch-to-buffer
  ;; 	     "b o" 'occur-dwim
  ;; 	     "e l" 'flycheck-list-errors
  ;; 	     "e n" 'flycheck-next-error
  ;; 	     "e p" 'flycheck-previous-error
  ;; 	     "f e d" 'open-my-file
  ;; 	     "f D" 'samray/delete-current-buffer-file
  ;; 	     "f E" 'samray/sudo-edit
  ;; 	     "f f" 'counsel-find-file
  ;; 	     "f r" 'recentf-open-files
  ;; 	     "f R" 'samray/rename-current-buffer-file
  ;; 	     "f s" 'save-buffer
  ;; 	     "h d d" 'apropos-documentation
  ;; 	     "h d f" 'counsel-describe-function
  ;; 	     "h d k" 'describe-key
  ;; 	     "h d v" 'counsel-describe-variable
  ;; 	     "g s" 'magit-status
  ;; 	     "j j" 'avy-goto-char
  ;; 	     "q s" 'save-buffers-kill-terminal
  ;; 	     "p a" 'helm-ag-project-root
  ;; 	     "p f" 'counsel-projectile-find-file
  ;; 	     "p d" 'counsel-projectile-find-dir
  ;; 	     "p b" 'counsel-projectile-switch-to-buffer
  ;; 	     "p s s" 'counsel-projectile-ag
  ;; 	     "p p" 'counsel-projectile-switch-project
  ;; 	     "v" 'er/expand-region
  ;; 	     "s a" 'counsel-ag
  ;; 	     "s g" 'counsel-git
  ;; 	     "s i" 'iedit-mode
  ;; 	     "s s" 'swiper
  ;; 	     "t f" 'fci-mode
  ;; 	     "w d" 'delete-window
  ;; 	     "w D" 'delete-other-windows
  ;; 	     "w -" 'split-window-below
  ;; 	     "w /" 'split-window-right
  ;; 	     "w h" 'evil-window-left
  ;; 	     "w j" 'evil-window-down
  ;; 	     "w k" 'evil-window-up
  ;; 	     "w l" 'evil-window-right
  ;; 	     "w m" 'samray/toggle-maximize-buffer
  ;; 	     "0"  'select-window-0
  ;; 	     "1"  'select-window-1
  ;; 	     "2"  'select-window-2
  ;; 	     "3"  'select-window-3
  ;; 	     "4"  'select-window-4
  ;; 	     "5"  'select-window-5
  ;; 	     )
  ;; 	   (evil-add-hjkl-bindings occur-mode-map 'emacs
  ;; 	     (kbd "/")       'evil-search-forward
  ;; 	     (kbd "n")       'evil-search-next
  ;; 	     (kbd "N")       'evil-search-previous
  ;; 	     (kbd "C-d")     'evil-scroll-down
  ;; 	     (kbd "C-u")     'evil-scroll-up
  ;; 	     (kbd "C-w C-w") 'other-window))
  ;;   ;; :bind(
  ;;   ;; 	:map evil-normal-state-map
  ;;   ;; 	     ("C-e" . evil-end-of-line)
  ;;   ;; 	     ("C-a" . evil-beginnin-of-line)
  ;;   ;; 	     :map evil-visual-state-map
  ;;   ;; 	     ("C-e" . evil-end-of-line)
  ;;   ;; 	     ("C-a" . evil-beginning-of-line)
  ;;   ;; 	     :map evil-insert-state-map
  ;;   ;; 	     ("C-e" . end-of-line)
  ;;   ;; 	     ("C-a" . beginning-of-line)
  ;;   ;; 	     :map evil-motion-state-map
  ;;   ;; 	     ("C-e" . evil-end-of-line)
  ;;   ;; 	     ("C-a" . evil-beginning-of-line))
  ;;   )
  ;; 
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
    :config 
    (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
    )
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (evilnc-default-hotkeys)
  (provide 'init-evil)
;;; init-evil.el ends here
