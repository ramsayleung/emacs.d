;;; package --- summary
;;; code:
;;; Commentary:
;; define-key module
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
(define-key isearch-mode-map [escape] 'isearch-abort)   ;; isearch
(define-key isearch-mode-map "\e" 'isearch-abort)   ;; \e seems to work better for terminals
(with-eval-after-load 'dired-mode
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

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


(use-package general
  :ensure t
  :config (progn
	    (general-evil-setup t)
	    (defvar my-leader-key "SPC")
	    (general-define-key :states '(normal visual emacs motion)
				:prefix my-leader-key
				";" 'evilnc-comment-operator
				"'" 'shell-pop
				"TAB" 'samray/alternate-buffer
				"a" '(:ignore t :which-key "applications")
				"a d" 'dired
				"a o a" 'org-agenda
				"b" '(:ignore t :which-key "buffers")
				"b b" 'ivy-switch-buffer
				"b d" 'kill-this-buffer
				"b l" 'switch-to-buffer
				"b o" 'occur-dwim
				"c" '(:ignore t :which-key "compile/comments")
				"cl" 'evilnc-quick-comment-or-uncomment-to-the-line
				"cc" 'evilnc-copy-and-comment-lines
				"cp" 'evilnc-comment-or-uncomment-paragraphs
				"cr" 'comment-or-uncomment-region
				"cv" 'evilnc-toggle-invert-comment-line-by-line
				"e" '(:ignore t :which-key "errors")
				"e l" 'flycheck-list-errors
				"e n" 'flycheck-next-error
				"e p" 'flycheck-previous-error
				"f" '(:ignore t :which-key "files")
				"f D" 'samray/delete-current-buffer-file
				"f E" 'samray/sudo-edit
				"f f" 'counsel-find-file
				"f r" 'recentf-open-files
				"f R" 'samray/rename-current-buffer-file
				"f s" 'save-buffer
				"f e" '(:ignore t :which-key "emacs")
				"f e d" 'open-my-file
				"h" '(:ignore t :which-key "helo")
				"h d" '(:ignore t :which-key "help-describe")
				"h d d" 'apropos-documentation
				"h d f" 'counsel-describe-function
				"h d k" 'describe-key
				"h d v" 'counsel-describe-variable
				"g" '(:ignore t :which-key "git/version-control")
				"g s" 'magit-status
				"j" '(:ignore t :which-key "jump/join/split")
				"j j" 'avy-goto-char
				"m" '(:ignore t :which-key "markdown")
				"m -" 'markdown-insert-hr
				"m h" '(:ignore t :which-key "markdown/header")
				"m h i" 'markdown-insert-header-dwim
				"m h I" 'markdown-insert-header-setext-dwim
				"m h 1" 'markdown-insert-header-atx-1
				"m h 2" 'markdown-insert-header-atx-2
				"m h 3" 'markdown-insert-header-atx-3
				"m h 4" 'markdown-insert-header-atx-4
				"m h 5" 'markdown-insert-header-atx-5
				"m h 6" 'markdown-insert-header-atx-6
				"m h !" 'markdown-insert-header-setext-1
				"m h @" 'markdown-insert-header-setext-2
				"m i" '(:ignore t :which-key "markdown/insert")
				"m i l" 'markdown-insert-link
				"m i L" 'markdown-insert-reference-link-dwim
				"m i u" 'markdown-insert-uri
				"m i f" 'markdown-insert-footnote
				"m i w" 'markdown-insert-wiki-link
				"m i i" 'markdown-insert-image
				"m i I" 'markdown-insert-reference-image
				"m x" '(:ignore t :which-key "markdown/text")
				"m x b" 'markdown-insert-bold
				"m x i" 'markdown-insert-italic
				"m x c" 'markdown-insert-code
				"m x C" 'markdown-insert-gfm-code-block
				"m x q" 'markdown-insert-blockquote
				"m x Q" 'markdown-blockquote-region
				"m x p" 'markdown-insert-pre
				"m x P" 'markdown-pre-region
				"p" '(:ignore t :which-key "projects")
				"p a" 'helm-ag-project-root
				"p f" 'counsel-projectile-find-file
				"p d" 'counsel-projectile-find-dir
				"p b" 'counsel-projectile-switch-to-buffer
				"p s s" 'counsel-projectile-ag
				"p p" 'counsel-projectile-switch-project
				"q" '(:ignore t :which-key "quit")
				"q s" 'save-buffers-kill-terminal
				"q d" 'samray/restart-emacs-debug-init
				"q D" 'samray/restart-stock-emacs-with-packages
				"q r" 'samray/restart-emacs-resume-layouts
				"v" 'er/expand-region
				"s" '(:ignore t :which-key "search")
				"s a" 'counsel-ag
				"s g" 'counsel-git
				"s i" 'iedit-mode
				"s s" 'swiper
				"t" '(:ignore t :which-key "toggle")
				"t f" 'fci-mode
				"w" '(:ignore t :which-key "windows")
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
	    (general-define-key :states '(normal visual insert ) 
				"C-e" 'evil-end-of-line
				"C-a" 'evil-beginning-of-line
				"C-y" 'yank
				"C-w" 'evil-delete
				)
	    (general-nvmap
	     "R" 'evil-multiedit-match-all
	     "C-<" 'evil-multiedit-match-and-next
	     "C->" 'evil-multiedit-match-and-prev
	     "C-c C->" 'evil-multiedit-restore
	     )
	    (general-define-key :keymaps 'read-expression-map
				"C-r" 'counsel-expression-history)
	    (general-define-key :keymaps 'evil-multiedit-state-map
				"RET" 'evil-multiedit-toggle-or-restrict-region
				"C-n" 'evil-multiedit-next
				"C-p" 'evil-multiedit-prev)
	    (general-define-key :keymaps 'evil-multiedit-insert-state-map
				"C-n" 'evil-multiedit-next
				"C-p" 'evil-multiedit-prev
				)
	    (general-mmap
	     "RET" 'evil-multiedit-toggle-or-restrict-region)
	    ;; Non-evil ,without a prefix
	    (general-define-key
	     "C-c a" 'org-agenda
	     "C-c g" 'counsel-git
	     "C-c j" 'counsel-grep
	     "C-c k" 'counsel-ag
	     "C-c r" 'org-capture-templates
	     "C-c C-r" 'ivy-resume
	     "C-h f" 'counsel-describe-function
	     "C-h v" 'counsel-describe-variable
	     "C-h l" 'counsel-find-library
	     "C-x C-f" 'counsel-find-file
	     "C-x l" 'counsel-locate
	     "C-x C-r" 'recentf-open-files
	     "C-s" 'swiper
	     "C-=" 'er/expand-region
	     "M-x" 'counsel-M-x
	     "<f1>" 'open-my-file
	     "<f2> i" 'counsel-info-lookup-symbol
	     "<f2> u" 'counsel-unicode-char
	     "<f6>" 'ivy-resume
	     )
	    )
  )
(js2r-add-keybindings-with-prefix "C-c C-m")
(provide 'init-keybindings)
;;; init-keybindings.el ends here
