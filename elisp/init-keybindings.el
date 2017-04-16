;;; Package --- Summary
;;; code:
;;; commentary:
(use-package general
  :ensure t
  :config (progn
	    (general-evil-setup t)
	    (defvar my-leader-key "SPC")
	    (defvar my-second-leader-key ",")
	    (general-define-key :states '(normal visual motion )
				:prefix my-leader-key
				";" 'evilnc-comment-operator
				"'" 'shell-pop
				"." 'samray/repl-pop
				"TAB" 'samray/alternate-buffer
				"a" '(:ignore t :which-key "applications")
				"a d" 'dired
				"a c" 'circe
				"a w" 'wttrin
				"a y" '(:ignore t :which-key "youdao-dictionary")
				"a y i" 'youdao-dictionary-search-from-input
				"a y p" 'youdao-dictionary-search-at-point+
				"a z" '(:ignore t :which-key "ztree")
				"a z d" 'ztree-dir
				"a z c" 'ztree-diff
				"b" '(:ignore t :which-key "buffers")
				"b l" 'samray/ivy-switch-to-buffer-enhanced
				"b d" 'kill-this-buffer
				"b D" 'samray/delete-blank-line-in-buffer
				"b b" 'samray/switch-to-current-open-buffer
				"b o" 'occur-dwim
				"c" '(:ignore t :which-key "compile/comments")
				"cl" 'evilnc-quick-comment-or-uncomment-to-the-line
				"cc" 'evilnc-copy-and-comment-lines
				"cp" 'evilnc-comment-or-uncomment-paragraphs
				"cr" 'comment-or-uncomment-region
				"cv" 'evilnc-toggle-invert-comment-line-by-line
				"e" '(:ignore t :which-key "errors/edit")
				"e l" 'flycheck-list-errors
				"e j" 'samray/move-text-down
				"e k" 'samray/move-text-up
				"e n" 'flycheck-next-error
				"e p" 'flycheck-previous-error
				"f" '(:ignore t :which-key "files")
				"f c" 'samray/copy-current-file-path
				"f d" 'samray/delete-current-buffer-file
                                "f D" 'samray/delete-whitespace-between-english-and-chinese-char
				"f E" 'samray/sudo-edit
				"f f" 'counsel-find-file
				"f g" 'samray/counsel-goto-recent-directory
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
				"g m" 'magit-dispatch-popup
				"g t" 'git-timemachine-switch-branch
				"j" '(:ignore t :which-key "jump/join/split")
				"j j" 'avy-goto-char
				"j l" 'avy-goto-line
				"m" '(:ignore t :which-key "major-mode-cmd")
				"p" '(:ignore t :which-key "projects")
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
				"t g" 'samray/git-timemachine
				"t i" 'imenu-list-smart-toggle
				"t f" 'samray/cycle-font
				"t o" 'origami-toggle-mode
				"t r" 'sr-speedbar-toggle
				"t s" 'selectric-mode
                                "t t" 'samray/cycle-theme
				"t w" 'samray/toggle-window-split
				"w" '(:ignore t :which-key "windows")
				"w d" 'delete-window
				"w D" 'delete-other-windows
				"w h" 'winner-undo
				"w l" 'winner-redo
				"w -" 'samray/split-window-below-and-move
				"w /" 'samray/split-window-right-and-move
				"w h" 'evil-window-left
				"w j" 'evil-window-down
				"w k" 'evil-window-up
				"w l" 'evil-window-right
				"w m" 'samray/toggle-maximize-buffer
				"w <left>" 'evil-window-left
				"w <down>" 'evil-window-down
				"w <up>" 'evil-window-up
				"w <right>" 'evil-window-right
				"0"  'select-window-0
				"1"  'select-window-1
				"2"  'select-window-2
				"3"  'select-window-3
				"4"  'select-window-4
				"5"  'select-window-5
				)
	    (general-define-key :states '(normal visual insert )
				"C-e" 'evil-end-of-line
				"C-a" 'samray/smarter-move-beginning-of-line
				"C-y" 'yank
				"C-w" 'evil-delete
				)
	    ;; org-mode
	    (general-define-key :states '(normal visual motion )
				:keymaps 'org-mode-map
				:prefix my-leader-key
				"a o" '(:ignore t :which-key "org-mode" )
				"a o a" 'org-agenda-list
				"a o c" 'org-capture
				"a o l" 'org-store-link
				"a o i" 'org-clock-in
				"a o m" 'org-tags-view
				"a o o" 'org-agenda
				"a o O" 'org-clock-out
				"a o s" 'org-search-view
				"a o t" 'org-todo-list
				"a o p" 'org-pomodoro
                                "t p" 'org-preview-html-mode
                                "t d" 'org-indent-mode
				)
	    (general-define-key :states '(normal insert visual motion)
				:keymaps 'org-mode-map
				"M-l" '(org-metaright)
				"M-h" '(org-metaleft)
				"M-k" '(org-metaup)
				"M-j" '(org-metadown)
				"M-L" '(org-shiftmetaright)
				"M-H" '(org-shiftmetaleft)
				"M-K" '(org-shiftup)
				"M-J" '(org-shiftdown)
				"M-o" '(org-insert-heading)
				"M-t" '(org-insert-todo-heading)
				)
            (general-define-key :states '(normal visual motion)
                                :keymaps 'org-mode-map
                                ;; "h" 'evil-backward-char
				[remap org-beginning-of-line] 'evil-backward-char
				[remap org-end-of-line] 'evil-forward-char
                                ;; "l" 'evil-forward-char
				"J" 'org-next-visible-heading
				"K" 'org-previous-visible-heading
                                )
	    (general-define-key :states 'normal
				:keymaps 'org-mode-map
				"<tab>" 'org-cycle
				"$" 'org-end-of-line
				"^" 'org-beginning-of-line
				"gh" 'outline-up-heading
				"gj" 'org-forward-heading-same-level
				"gk" 'org-backward-heading-same-level
				"gl" 'outline-next-visible-heading
				"t" 'org-todo
				"h" 'org-beginning-of-line
				"l" 'org-end-of-line)

	    ;; markdown-mode
	    (general-define-key :states '(normal visual motion )
				:keymaps 'markdown-mode-map
				:prefix my-leader-key
				"m -" 'markdown-insert-hr
				"m h" '(:ignore t :which-key "markdown/header")
				"m h i" 'markdown-insert-header-dwim
				"m h i" 'markdown-insert-header-setext-dwim
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
				"m i l" 'markdown-insert-reference-link-dwim
				"m i u" 'markdown-insert-uri
				"m i f" 'markdown-insert-footnote
				"m i w" 'markdown-insert-wiki-link
				"m i i" 'markdown-insert-image
				"m i i" 'markdown-insert-reference-image
				"m x" '(:ignore t :which-key "markdown/text")
				"m x b" 'markdown-insert-bold
				"m x i" 'markdown-insert-italic
				"m x c" 'markdown-insert-code
				"m x c" 'markdown-insert-gfm-code-block
				"m x q" 'markdown-insert-blockquote
				"m x q" 'markdown-blockquote-region
				"m x p" 'markdown-insert-pre
				"m x p" 'markdown-pre-region
				)
	    ;; Python mode
	    (general-define-key :states '(normal visual  motion)
				:keymaps 'python-mode-map
				:prefix my-leader-key
				"m c" '(:ignore t :which-key "excute")
				"m c c" 'samray/python-execute-file
				"m c C" 'samray/python-execute-file-focus
				"m s" '(:ignore t :which-key "Send to REPL")
				"m s b" 'python-shell-send-buffer
				"m s B" 'samray/python-shell-send-buffer-switch
				"m s f" 'python-shell-send-defun
				"m s F" 'samray/python-shell-send-defun-switch
				"m s i" 'samray/python-start-or-switch-repl
				"m s r" 'python-shell-send-region
				"m s R" 'samray/python-shell-send-region-switch
				"m v" 'venv-set-location
				)
	    ;; C mode
	    (general-define-key :keymaps 'c-mode-map
				"<tab>" 'clang-format-buffer)
	    ;; C++ mode
	    (general-define-key :keymaps 'c++-mode-map
				"<tab>" 'clang-format-buffer)
	    ;; Scheme mode
	    (general-define-key :states '(normal visual motion)
				:keymaps 'scheme-mode-map
				:prefix my-leader-key
				"m c" '(:ignore t :which-key "compiling")
				"m c c" 'geiser-compile-current-buffer
				"m c p" 'geiser-add-to-path
				"m i" '(:ignore t :which-key "insertion")
				"m i l" 'geiser-insert-lambda
				"m s" '(:ignore t :which-key "repl")
				"m s i" 'geiser-switch-to-repl
				"m s s" 'geiser-set-scheme
				"m s b" 'geiser-eval-buffer
				"m s B" 'geiser-eval-buffer-and-go
				"m s f" 'geiser-eval-definition
				"m s F" 'geiser-eval-definition-and-go
				"m s e" 'geiser-eval-last-sexp
				"m s r" 'geiser-eval-region
				"m s R" 'geiser-eval-region-and-go
				"m e" '(:ignore t :which-key "Evaluation")
				"m e b" 'geiser-eval-buffer
				"m e e" 'geiser-eval-last-sexp
				"m e f" 'geiser-eval-definition
				"m e l" 'lisp-state-eval-sexp-end-of-line
				"m e r" 'geiser-eval-region
				"m m" '(:ignore t :which-key "macroexpansion")
				"m m e" 'geiser-expand-last-sexp
				"m m f" 'geiser-expand-definition
				"m m r" 'geiser-expand-region
				)

	    ;; origami mode
	    (general-define-key		:states '(normal visual motion)
					:keymaps 'origami-mode-map
					:prefix my-leader-key
					"t o" '(:ignore t :which-key "origami")
					"t o t" 'origami-toggle-node
					"t o f" 'origami-forward-toggle-node
					"t o r" 'origami-recursively-toggle-node
					"t o a" 'origami-toggle-all-nodes
					)
	    ;; origami mode
	    (general-define-key :keymaps 'origami-mode-map
				"C-=" 'origami-recursively-toggle-node
				"C--" 'origami-close-node-recursively
				"C-+" 'origami-open-node-recursively
				"C-c C-o" 'origami-show-only-node
				)
	    ;; company node
	    (general-define-key :keymaps 'company-active-map
				"<tab>" 'company-complete-common-or-cycle
				[remap evil-complete-next] 'company-select-next
				[remap evil-complete-previous] 'company-select-previous
				)
	    ;; eshll-mode
	    (general-define-key :keymaps 'eshell-mode-map
				"C-c C-a" 'samray/eshell-sudo-toggle
				[remap eshell-bol] 'samray/eshell-sudo-toggle
				"C-a" 'samray/eshell-maybe-bol
				"C-l" 'samray/eshell-clear-buffer
				"C-k" 'eshell-kill-input
				[remap samray/smarter-move-beginning-of-line] 'samray/eshell-maybe-bol
				[remap evil-insert-digraph] 'eshell-kill-input)
	    (general-define-key :keymaps 'dumb-jump-mode-map
				"M-g j" 'dumb-jump-go
				"M-g o" 'dumb-jump-go-other-window
				"M-g b" 'dumb-jump-back
				)
	    ;; pdf view mode
	    (general-define-key :states '(normal emacs)
				:keymaps 'pdf-view-mode-map
				"C-b"            'evil-scroll-page-up
				"C-d"            'pdf-view-scroll-up-or-next-page
				"C-e"            'evil-scroll-line-down
				"C-f"            'evil-scroll-page-down
				"C-n"            'image-next-file
				"C-u"            'pdf-view-scroll-down-or-previous-page
				"C-y"            'evil-scroll-line-up
				"C-z"            'evil-emacs-state
				"$"              'image-eol
				"'"              'image-scroll-up
				"/"              'isearch-forward
				"0"              'image-bol
				":"              'evil-ex
				"?"              'isearch-backward
				"G"              'pdf-view-last-page
				"H"              'describe-mode
				"J"              'pdf-view-next-page
				"K"              'pdf-view-previous-page
				"N"              'evil-search-previous
				"O"              'pdf-outline
				"V"              'evil-visual-line
				"d"              'pdf-view-scroll-up-or-next-page
				"h"              'image-backward-hscroll
				"j"              'pdf-view-next-line-or-next-page
				"k"              'pdf-view-previous-line-or-previous-page
				"l"              'image-forward-hscroll
				"n"              'evil-search-next
				"o"              'pdf-links-action-perform
				"r"              'pdf-view-revert-buffer
				"u"              'pdf-view-scroll-down-or-previous-page
				"v"              'evil-visual-char
				"C-S-g"          'revert-buffer
				"z r"            'pdf-view-scale-reset
				"` `"            'pdf-history-backward
				"g g"            'pdf-view-first-page
				"g l"            'pdf-view-goto-label
				"g t"            'pdf-view-goto-page
				)
	    (general-nvmap
	     "Y" 'samray/copy-to-end-of-line
             "(" 'paredit-open-round
	     )
	    (general-define-key :keymaps 'read-expression-map
				"C-r" 'counsel-expression-history)
	    ;; non-evil ,without a prefix
	    (general-define-key
	     ;; remap c-a to `samray/smarter-move-beginning-of-line
	     [remap move-beginning-of-line] 'samray/smarter-move-beginning-of-line
	     "C-c a" 'org-agenda
	     "C-c b" 'samray/counsel-ag-symbol-at-point
	     "C-c g" 'counsel-git
	     "C-c j" 'counsel-grep
	     "C-c k" 'counsel-ag
	     "C-c C-r" 'ivy-resume
	     "C-h f" 'counsel-describe-function
	     "C-h v" 'counsel-describe-variable
	     "C-h l" 'counsel-find-library
	     "C-x C-f" 'counsel-find-file
	     "C-x l" 'counsel-locate
             "C-x b" 'samray/ivy-switch-to-buffer-enhanced
	     "C-x k" 'kill-this-buffer
	     "C-x C-r" 'recentf-open-files
	     "C-x 2" 'samray/split-window-below-and-move
	     "C-x 3" 'samray/split-window-right-and-move
             "C-M-a" 'sp-beginning-of-sexp
             "C-M-e" 'sp-end-of-sexp
             "C-<down>" 'sp-down-sexp
             "C-<up>" 'sp-up-sexp
             "M-<down>" 'sp-backward-down-sexp
             "M-<up>" 'sp-backward-up-sexp
             "C-M-k" 'sp-kill-sexp
             "C-k" 'sp-kill-hybrid-sexp
             "M-k" 'sp-backward-kill-sexp
             "M-[" 'sp-backward-unwrap-sexp
             "M-]" 'sp-unwrap-sexp
	     "M-1" 'projectile-speedbar-open-current-buffer-in-tree
             "C-c (" 'wrap-with-parens
             "C-c [" 'wrap-with-brackets
             "C-c {" 'wrap-with-braces
             "C-c '" 'wrap-with-single-quotes
             "C-c \"" 'wrap-with-double-quotes
             "C-c _" 'wrap-with-underscotes
             "C-c `" 'wrap-with-back-quotes
	     "C-s" 'swiper
	     "C-;" 'samray/projectile-speedbar-toggle
	     "M-x" 'counsel-M-x
	     "<f1>" 'open-my-file
	     "<f2> i" 'counsel-info-lookup-symbol
	     "<f2> u" 'counsel-unicode-char
	     "<f5>" 'revert-buffer
	     "<f6>" 'ivy-resume
	     ))
  ;; Format buffer
  ;; Python mode
  (general-define-key :keymaps 'python-mode-map
                      "C-c C-g" 'elpy-goto-definition
                      "C-M-;" 'py-autopep8-buffer)
  ;; Web mode|Html mode
  (general-define-key :keymaps '(web-mode-map html-mode-map)
                      "C-M-;" 'web-beautify-html)
  ;; Css mode
  (general-define-key :keymaps 'css-mode-map
                      "C-M-;" 'web-beautify-css)
  ;; Js|Js2|Json mode
  (general-define-key :keymaps '(js-mode-map js2-mode-map json-mode-map)
                      "C-M-;" 'web-beautify-js)
  ;; Prog-mode  Org-mode
  (general-define-key :keymaps '(prog-mode-map org-mode-map)
                      "C-M-;" 'samray/indent-buffer)
  (general-define-key :states '(normal visual )
		      :prefix my-second-leader-key
		      "e" '(:ignore t :which-key "eval")
		      "e b" 'evil-buffer
		      "f" '(:ignore t :which-key "file")
		      "f r" 'samray/open-readme-in-git-root-directory
		      "r" '(:ignore t :which-key "refactor")
		      "r s" 'samray/evilcvn-change-symbol-in-defun
		      )
  (general-define-key :keymaps 'counsel-find-file-map
  		      "C-j" 'ivy-next-line
  		      "C-k" 'ivy-previous-line)
  (general-define-key :keymaps 'emacs-lisp-mode-map
                      :states 'insert
        	      "DEL" 'hungry-delete-backward)
  (general-define-key :keymaps 'emacs-lisp-mode-map
  		      "C-c s" 'find-function-at-point)
  (general-define-key :keymaps 'term-raw-map
  		      "C-y" 'samray/term-paste)
  ;; (general-define-key :states '(normal emacs)
  ;; 		      :keymaps 'youdao-dictionary-mode-map
  ;; 		      "q" 'samray/youdao-dictionary-buffer-quit)
  (general-define-key :states '(normal emacs)
  		      :keymaps 'geiser-repl-mode-map
  		      "q" '(progn (bury-buffer) (delete-window)))
  (general-define-key :states '(normal emacs)
  		      :keymaps 'inferior-python-mode-map
  		      "q" '(progn (bury-buffer) (delete-window))
		      )
  (general-define-key :states '(normal emacs)
		      :keymaps 'messages-buffer-mode-map
		      "q" '(progn (bury-buffer) (delete-window)))
  (with-eval-after-load 'popwin
    (general-define-key
     "C-c C-z" popwin:keymap
     )))

(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)
(provide 'init-keybindings)
;;; init-keybindings ends here
