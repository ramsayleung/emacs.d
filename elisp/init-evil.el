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
	     "a o a" 'org-agenda
	     "b b" 'ivy-switch-buffer
	     "b d" 'kill-this-buffer
	     "b l" 'switch-to-buffer
	     "b o" 'occur-dwim
	     "e l" 'flycheck-list-errors
	     "e n" 'flycheck-next-error
	     "e p" 'flycheck-previous-error
	     "f e d" 'open-my-file
	     "f D" 'samray/delete-current-buffer-file
	     "f E" 'samray/sudo-edit
	     "f f" 'counsel-find-file
	     "f r" 'recentf-open-files
	     "f R" 'samray/rename-current-buffer-file
	     "f s" 'save-buffer
	     "h d d" 'apropos-documentation
	     "h d f" 'counsel-describe-function
	     "h d k" 'describe-key
	     "h d v" 'counsel-describe-variable
	     "g s" 'magit-status
	     "j j" 'avy-goto-char
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
	     "t f" 'fci-mode
	     "w d" 'delete-window
	     "w D" 'delete-other-windows
	     "w -" 'split-window-below
	     "w /" 'split-window-right
	     "w h" 'evil-window-left
	     "w j" 'evil-window-down
	     "w k" 'evil-window-up
	     "w l" 'evil-window-right
	     "w m" 'samray/toggle-maximize-buffer
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
(use-package evil-multiedit
  :ensure t
  :config (progn
	    ;; Highlights all matches of the selection in the buffer.
	    (define-key evil-visual-state-map "R" 'evil-multiedit-match-all)

	    ;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will
	    ;; incrementally add the next unmatched match.
	    (define-key evil-normal-state-map (kbd "C-<") 'evil-multiedit-match-and-next)
	    ;; Match selected region.
	    (define-key evil-visual-state-map (kbd "C-<") 'evil-multiedit-match-and-next)

	    ;; Same as M-d but in reverse.
	    (define-key evil-normal-state-map (kbd "C->") 'evil-multiedit-match-and-prev)
	    (define-key evil-visual-state-map (kbd "C->") 'evil-multiedit-match-and-prev)

	    ;; OPTIONAL: If you prefer to grab symbols rather than words, use
	    ;; `evil-multiedit-match-symbol-and-next` (or prev).

	    ;; Restore the last group of multiedit regions.
	    (define-key evil-visual-state-map (kbd "C-c C->") 'evil-multiedit-restore)

	    ;; RET will toggle the region under the cursor
	    (define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

	    ;; ...and in visual mode, RET will disable all fields outside the selected region
	    (define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

	    ;; For moving between edit regions
	    (define-key evil-multiedit-state-map (kbd "C-n") 'evil-multiedit-next)
	    (define-key evil-multiedit-state-map (kbd "C-p") 'evil-multiedit-prev)
	    (define-key evil-multiedit-insert-state-map (kbd "C-n") 'evil-multiedit-next)
	    (define-key evil-multiedit-insert-state-map (kbd "C-p") 'evil-multiedit-prev)
	    ;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
	    (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
	    ))
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(evilnc-default-hotkeys)
(provide 'init-evil)
;;; init-evil.el ends here
