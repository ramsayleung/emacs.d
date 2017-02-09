;;; package --- Summary
;;; Code:
;;; Commentary:
;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(use-package evil
  :ensure t
  :config
  (progn
    (evil-mode t)
    (define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
    (define-key evil-insert-state-map "\C-e" 'end-of-line)
    (define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
    (define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
    (define-key evil-normal-state-map "\C-a" 'evil-beginning-of-line)
    (define-key evil-insert-state-map "\C-a" 'beginning-of-line)
    (define-key evil-visual-state-map "\C-a" 'evil-beginning-of-line)
    (define-key evil-motion-state-map "\C-a" 'evil-beginning-of-line)
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)
    (define-key evil-normal-state-map "\C-y" 'yank)
    (define-key evil-insert-state-map "\C-y" 'yank)
    (define-key evil-visual-state-map "\C-y" 'yank)
    (define-key evil-normal-state-map "\C-w" 'evil-delete)
    (define-key evil-insert-state-map "\C-w" 'evil-delete)
    (define-key evil-visual-state-map "\C-w" 'evil-delete)
    (define-key evil-insert-state-map "\C-r" 'search-backward)
    )
  )

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode t)
  )

(use-package evil-leader
  :ensure t
  :config(progn
	   (global-evil-leader-mode)
	   (evil-leader/set-leader "<SPC>")
	   (evil-leader/set-key
	     ";" 'evilnc-comment-operator
	     "'" 'shell-pop
	     "TAB" 'samray/alternate-buffer
	     "a d" 'dired
	     "b b" 'ivy-switch-buffer
	     "b d" 'kill-this-buffer
	     "b l" 'switch-to-buffer
	     "b o" 'occur-dwim
	     "e l" 'flycheck-list-errors
	     "e n" 'flycheck-next-error
	     "e p" 'flycheck-previous-error
	     "f e d" 'open-my-file
	     "f f" 'counsel-find-file
	     "f r" 'recentf-open-files
	     "f s" 'save-buffer
	     "h d d" 'apropos-documentation
	     "h d f" 'counsel-describe-function
	     "h d k" 'describe-key
	     "h d v" 'counsel-describe-variable
	     "g s" 'magit-status
	     "j j" 'avy-goto-char
	     "o a" 'org-agenda
	     "q s" 'save-buffers-kill-terminal
	     "p a" 'helm-ag-project-root
	     "p f" 'counsel-projectile-find-file
	     "p d" 'counsel-projectile-find-dir
	     "p b" 'counsel-projectile-switch-to-buffer
	     "p s s" 'counsel-projectile-ag
	     "p p" 'counsel-projectile-switch-project
	     "v" 'er/expand-region
	     "s a" 'counsel-ag
	     "s g" 'counsel-git
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
	     "4"  'select-window-4
	     "5"  'select-window-5
	     )
	   (evil-add-hjkl-bindings occur-mode-map 'emacs
	     (kbd "/")       'evil-search-forward
	     (kbd "n")       'evil-search-next
	     (kbd "N")       'evil-search-previous
	     (kbd "C-d")     'evil-scroll-down
	     (kbd "C-u")     'evil-scroll-up
	     (kbd "C-w C-w") 'other-window))
  ;; :bind(
  ;; 	:map evil-normal-state-map
  ;; 	     ("C-e" . evil-end-of-line)
  ;; 	     ("C-a" . evil-beginnin-of-line)
  ;; 	     :map evil-visual-state-map
  ;; 	     ("C-e" . evil-end-of-line)
  ;; 	     ("C-a" . evil-beginning-of-line)
  ;; 	     :map evil-insert-state-map
  ;; 	     ("C-e" . end-of-line)
  ;; 	     ("C-a" . beginning-of-line)
  ;; 	     :map evil-motion-state-map
  ;; 	     ("C-e" . evil-end-of-line)
  ;; 	     ("C-a" . evil-beginning-of-line))
  )
(use-package evil-nerd-commenter
  :ensure t
  :config
  (evil-leader/set-key
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line
    ))
;; evil keybinding in magit
(use-package evil-magit
  :ensure t)

;; Vim matchit ported into Emacs
(use-package evil-matchit
  :ensure t)
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(evilnc-default-hotkeys)
(provide 'init-evil)
;;; init-evil.el ends here
