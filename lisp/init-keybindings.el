;;; Package --- Summary
;;; code:
;;; commentary:
(use-package general
  :ensure t
  :config (progn
	    (general-evil-setup t)
	    (defvar ramsay/leader-key "SPC") ;
	    (defvar ramsay/second-leader-key "/")
	    (general-define-key :states '(normal visual motion )
				:prefix ramsay/leader-key
				"" nil
				";" 'comment-dwim
				"'" 'eshell
				"." 'ramsay/python-send-repl-echo-switch
				"a" '(:ignore t :which-key "applications")
				"a t" '(:ignore t :which-key "translator")
				"a t d" 'gts-do-translate
				"b" '(:ignore t :which-key "buffers")
				"b f" 'ramsay/get-buffer-full-name
				"b k" 'ramsay/kill-other-buffers
				"b D" 'ramsay/delete-blank-line-in-buffer
				"b n" 'ramsay/get-buffer-name
				"c" '(:ignore t :which-key "compile/comments")
				"e" #'hydra-flycheck/body
				"f" '(:ignore t :which-key "files")
				"f c" 'ramsay/copy-current-file-path
				"f d" 'ramsay/delete-current-buffer-file
				"f E" 'ramsay/sudo-edit
				"f f"  'counsel-find-file
				"f g" 'ramsay/counsel-goto-recent-directory
				"f r" 'ramsay/rename-current-buffer-file
				"f R" 'recentf-open-files
				"f s" 'save-buffer
				"f e" '(:ignore t :which-key "emacs")
				"f e d" 'open-ramsay/file
				"h" '(:ignore t :which-key "help")
				"h d" '(:ignore t :which-key "help-describe")
				"h d d" 'apropos-documentation
				"h d f" 'counsel-describe-function
				"h d k" 'counsel-describe-key
				"h d v" 'counsel-describe-variable
				"g" '(:ignore t :which-key "git/version-control")
				"g s" 'magit-status
				"g m" 'magit-dispatch-popup
				"j" '(:ignore t :which-key "jump/join/split")
				"j l" 'avy-goto-line
				"j j" 'avy-goto-char
				"j w" 'avy-goto-word-1
				"m" '(:ignore t :which-key "major-mode-cmd")
				"p" '(:ignore t :which-key "projects")
				"p f" (general-predicate-dispatch 'project-find-file (not emacs/>=28p) 'counsel-projectile-find-file)
				"p d" (general-predicate-dispatch 'project-find-dir (not emacs/>=28p) 'counsel-projectile-find-dir)
				"p b" (general-predicate-dispatch 'project-switch-to-buffer (not emacs/>=28p) 'counsel-projectile-switch-to-buffer)
				"p p" (general-predicate-dispatch 'project-switch-project (not emacs/>=28p) 'counsel-projectile-switch-project)
				"p s" '(:ignore t :which-key "project search")
				"p s r" (general-predicate-dispatch 'project-find-regexp (not emacs/>=28p) 'counsel-projectile-rg)
				"q" '(:ignore t :which-key "quit")
				"q s" 'save-buffers-kill-terminal
				"v" 'er/expand-region
				"s" '(:ignore t :which-key "search")
				"s g" 'counsel-grep
				"s r" 'counsel-rg
				"s s" 'isearch-forward
				"1"  'ace-window)
	    
	    (general-define-key :keymaps 'counsel-find-file-map
				:states '(normal visual motion)
				"C-j" 'ivy-next-line
				"C-k" 'ivy-previous-line)

	    ;; Org-mode
	    (general-define-key :states '(normal visual motion)
				:keymaps 'org-mode-map
				:prefix ramsay/leader-key
				"a o" '(:ignore t :which-key "org-mode" )
				"a o a" 'org-agenda-list
				"a o c" 'org-capture
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
				"M-l" 'org-metaright
				"M-h" 'org-metaleft
				"M-k" 'org-metaup
				"M-j" 'org-metadown
				"M-L" 'org-shiftmetaright
				"M-H" 'org-shiftmetaleft
				"M-K" 'org-shiftup
				"M-J" 'org-shiftdown
				"M-o" 'org-insert-heading
				"M-t" 'org-insert-todo-heading
				"C-c c" 'org-capture
				"C-c n i" 'org-roam-insert
				"C-c n I" 'org-roam-insert-immediate
				)

	    (general-define-key :states '(normal visual motion)
				:keymaps 'org-mode-map
				;; "h" 'evil-backward-char
				[remap org-beginning-of-line] 'evil-backward-char
				[remap org-end-of-line] 'evil-forward-char
				;; "l" 'evil-forward-char
				)

	    (general-define-key :states 'normal
				:keymaps 'org-mode-map
				"TAB" 'org-cycle
				"$" 'org-end-of-line
				"^" 'org-beginning-of-line
				"gh" 'outline-up-heading
				"gj" 'org-forward-heading-same-level
				"gk" 'org-backward-heading-same-level
				"gl" 'outline-next-visible-heading
				"t" 'org-todo
				"h" 'org-beginning-of-line
				"l" 'org-end-of-line)

	    ;; C++ mode
	    (general-define-key :states '(normal visual motion)
				:keymaps '(c-mode-map c++-mode-map cmake-mode-map)
				:prefix ramsay/leader-key
				"m c" '(:ignore t :which-key "compiling")
				"m c c" 'ramsay/compile
				"m c C" 'ramsay/compile-clean
				"m c x" 'ramsay/g++-compile-and-run
				)

	    ;; Scheme mode
	    (general-define-key :states '(normal visual motion)
				:keymaps 'scheme-mode-map
				:prefix ramsay/leader-key
				"m c" '(:ignore t :which-key "compiling")
				"m c c" 'geiser-compile-current-buffer
				"m c p" 'geiser-add-to-path
				"m e" '(:ignore t :which-key "Evaluation")
				"m e b" 'geiser-eval-buffer
				"m e e" 'geiser-eval-last-sexp
				"m e f" 'geiser-eval-definition
				"m e l" 'lisp-state-eval-sexp-end-of-line
				"m e r" 'geiser-eval-region
				"m i" '(:ignore t :which-key "insertion")
				"m i l" 'geiser-insert-lambda
				"m m" '(:ignore t :which-key "macroexpansion")
				"m m e" 'geiser-expand-last-sexp
				"m m f" 'geiser-expand-definition
				"m m r" 'geiser-expand-region
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
				"m l" '(:ignore t :which-key "load")
				"m l c" 'geiser-load-current-buffer
				"m l f" 'geiser-load-file
				)

	    ;; Rust mode
	    (general-define-key :states '(normal visual motion)
				:keymaps 'rust-mode-map
				:prefix ramsay/leader-key
				"m c ." 'cargo-process-repeat
				"m c c" 'cargo-process-build
				"m c C" 'cargo-process-clean
				"m c d" 'cargo-process-doc
				"m c f" 'cargo-process-current-test
				"m c i" 'cargo-process-init
				"m c k" 'cargo-process-check
				"m c l" 'cargo-process-clippy
				"m c n" 'cargo-process-new
				"m c o" 'cargo-process-current-file-tests
				"m c r" 'ramsay/cargo-process-script
				"m c s" 'cargo-process-search
				"m c u" 'cargo-process-update
				"m c x" 'cargo-process-run
				"m c X" 'ramsay/cargo-process-run-current-example
				"m t" 'cargo-process-test
				)

	    ;; Company node
	    (general-define-key :keymaps 'company-active-map
				"<tab>" 'company-complete-common-or-cycle
				[remap evil-complete-next] 'company-select-next
				[remap evil-complete-previous] 'company-select-previous
				)

	    (general-define-key :keymaps 'smerge-mode-map
				"C-c ^ ^" 'hydra-smerge/body)

	    ;; profiler-report-mode
	    (general-define-key :keymaps 'profiler-report-mode-map
				"<tab>" 'profiler-report-expand-entry)

	    (general-define-key :states '(emacs)
				:keymaps 'neotree-mode-map
				"C-s" 'isearch-forward
				"C-M-S" 'isearch-forward-regexp
				"C-r" 'isearch-backward
				"C-M-r" 'isearch-backward-regexp)

	    
	    (general-define-key "C-c h" '(:ignore t :which-key "ivy-command-prefix")
				"C-c h a" 'counsel-apropos
				"C-c h b" 'ivy-resume
				"C-c h c" 'counsel-colors-emacs
				"C-c h i" 'counsel-imenu
				"C-c h l" 'counsel-locate
				"C-c h p" 'counsel-list-processes
				)
	    (general-define-key
	     "C-s" 'swiper-isearch
	     "C-x b"  'switch-to-buffer
	     "C-x C-b" 'bs-show
	     "C-x C-r" 'counsel-recentf
	     "C-x C-f" 'counsel-find-file
	     "M-x" 'counsel-M-x)

	    ;; non-evil ,without a prefix
	    (general-define-key
	     [remap evil-repeat-pop-next] 'xref-find-definitions
	     ;; remap C-n to `next-line`
	     [remap evil-complete-next] 'company-select-next
	     [remap evil-complete-previous] 'company-select-previous
	     "<f5>" 'deadgrep
	     "C-c a" 'org-agenda
	     "C-c l" 'org-store-link
	     "C-c e" 'hydra-edit/body
	     "C-c n" 'ramsay/empty-buffer
	     "C-c t d" 'gts-do-translate
	     "C-c w" 'hydra-window-resize/body
	     "C-c p" '(:ignore t :which-key "paredit")
	     "C-c p (" 'paredit-wrap-round
	     "C-c p [" 'paredit-wrap-square
	     "C-c p {" 'paredit-wrap-curly
	     "C-c p <" 'paredit-wrap-angled
	     "C-c p \"" 'paredit-meta-doublequote
	     [remap evil-paste-pop-next] 'next-line
	     "C-n" 'next-line
	     [remap evil-paste-pop] 'previous-line
	     "C-p" 'previous-line
	     "C-x k" 'kill-current-buffer
	     "C-x t" 'ramsay/dired-tmp-dir
	     "C-x u" 'undo-tree-visualize
	     "C-x 2" 'ramsay/split-window-below-and-move
	     "C-x 3" 'ramsay/split-window-right-and-move
	     "M-i" 'symbol-overlay-put
	     "M-RET o" 'srefactor-lisp-one-line
	     "M-RET m" 'srefactor-lisp-format-sexp
	     "M-RET d" 'srefactor-lisp-format-defun
	     "M-RET b" 'srefactor-lisp-format-buffer
	     "S-<f6>" 'lsp-rename)

	    (general-define-key :states '(insert normal visual motion)
				"C-a" 'move-beginning-of-line
				"C-e" 'move-end-of-line
				"C-w" 'kill-region
				"C-y" 'yank)
	    )



  ;; Prog-mode  Org-mode
  (general-define-key :keymaps '(prog-mode-map org-mode-map)
		      "C-M-\\" 'ramsay/indent-region-or-buffer)
  ;; Graphviz-dot-mode
  (general-define-key :keymaps '(graphviz-dot-mode-map)
		      "C-M-\\" 'graphviz-dot-indent-graph
		      "C-c C-u" 'ramsay/graphviz-dot-preview-with-external-viewer)

  (general-define-key :keymaps '(emacs-lisp-mode-map
				 ielm-mode-map
				 lisp-mode-map
				 scheme-mode-map
				 lisp-interaction-mode-map)
		      [remap paredit-convolute-sexp] 'xref-find-references
		      "M-?" 'xref-find-references)
  
  ;; Go-mode
  (general-define-key :keymaps 'go-mode-map
		      "C-M-\\" 'gofmt)
  ;; Rust-mode
  (general-define-key :keymaps 'rust-mode-map
		      "C-M-\\" 'rust-format-buffer)

  ;; Emacs Lisp mode
  (general-define-key :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
		      [remap evil-repeat-pop-next] 'xref-find-definitions
		      "M-." 'xref-find-definitions
		      "M-," 'xref-pop-marker-stack)

  (general-emacs-define-key 'global
    "C-v" 'evil-scroll-down
    "M-v" 'evil-scroll-up)
  
  (general-define-key :keymaps 'term-raw-map
		      "C-y" 'ramsay/term-paste)

  (with-eval-after-load 'popwin
    (general-define-key
     "C-c C-z" popwin:keymap
     ))
  )

(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

(use-package hydra
  :ensure t
  :config (progn
	    (defhydra hydra-flycheck
	      (:pre (progn (setq hydra-hint-display-type 'lv) (flycheck-list-errors))
		    :post (progn (setq hydra-hint-display-type 'lv) (quit-windows-on "*Flycheck errors*"))
		    :hint nil)
	      "errors"
	      ("f"  flycheck-error-list-set-filter                            "Filter")
	      ("j"  flycheck-next-error                                       "Next")
	      ("k"  flycheck-previous-error                                   "Previous")
	      ("y"  flycheck-copy-errors-as-kill                              "Yank/Copy")
	      ("gg" flycheck-first-error                                      "First")
	      ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
	      ("q"  nil))

	    (defhydra hydra-font-resize
	      (global-map "C-M-=")
	      "font-resize"
	      ("-"   ramsay/font-size-decr  "Decrease")
	      ("="   ramsay/font-size-incr  "Increase")
	      ("0"   ramsay/font-size-reset "Reset to default size"))

	    (defhydra hydra-smerge (:color red :hint nil)
	      "
Navigate       Keep               other
----------------------------------------
_p_: previous  _c_: current       _e_: ediff
_n_: next      _m_: mine  <<      _u_: undo
_j_: up        _o_: other >>      _r_: refine
_k_: down      _a_: combine       _q_: quit
               _b_: base
"
	      ("n" smerge-next)
	      ("p" smerge-prev)
	      ("c" smerge-keep-current)
	      ("m" smerge-keep-mine)
	      ("o" smerge-keep-other)
	      ("b" smerge-keep-base)
	      ("a" smerge-keep-all)
	      ("e" smerge-ediff)
	      ("j" previous-line)
	      ("k" forward-line)
	      ("r" smerge-refine)
	      ("u" undo)
	      ("q" nil :exit t))

	    (defhydra hydra-info (:color blue :exit nil)
	      "
Info-mode:

  ^^_]_ forward  (next logical node)       ^^_l_ast (←)        _u_p (↑)                             _f_ollow reference       _T_OC
  ^^_[_ backward (prev logical node)       ^^_r_eturn (→)      _m_enu (↓) (C-u for new window)      _i_ndex                  _d_irectory
  ^^_n_ext (same level only)               ^^_H_istory         _g_oto (C-u for new window)          _,_ next index item      _c_opy node name
  ^^_p_rev (same level only)               _<_/_t_op           _b_eginning of buffer                virtual _I_ndex          _C_lone buffer
  regex _s_earch (_S_ case sensitive)      ^^_>_ final         _e_nd of buffer                      ^^                       _a_propos

  _1_ .. _9_ Pick first .. ninth item in the node's menu.

"
	      ("]"   Info-forward-node)
	      ("["   Info-backward-node)
	      ("n"   Info-next)
	      ("p"   Info-prev)
	      ("s"   Info-search)
	      ("S"   Info-search-case-sensitively)
	      ("l"   Info-history-back)
	      ("r"   Info-history-forward)
	      ("H"   Info-history)
	      ("t"   Info-top-node)
	      ("<"   Info-top-node)
	      (">"   Info-final-node)
	      ("u"   Info-up)
	      ("^"   Info-up)
	      ("m"   Info-menu)
	      ("g"   Info-goto-node)
	      ("b"   beginning-of-buffer)
	      ("e"   end-of-buffer)
	      ("f"   Info-follow-reference)
	      ("i"   Info-index)
	      (","   Info-index-next)
	      ("I"   Info-virtual-index)
	      ("T"   Info-toc)
	      ("d"   Info-directory)
	      ("c"   Info-copy-current-node-name)
	      ("C"   clone-buffer)
	      ("a"   info-apropos)
	      ("1"   Info-nth-menu-item)
	      ("2"   Info-nth-menu-item)
	      ("3"   Info-nth-menu-item)
	      ("4"   Info-nth-menu-item)
	      ("5"   Info-nth-menu-item)
	      ("6"   Info-nth-menu-item)
	      ("7"   Info-nth-menu-item)
	      ("8"   Info-nth-menu-item)
	      ("9"   Info-nth-menu-item)
	      ("?"   Info-summary "Info summary")
	      ("h"   Info-help "Info help")
	      ("q"   Info-exit "Info exit")
	      ("C-g" nil "cancel" :color blue))
	    (define-key Info-mode-map (kbd "?") #'hydra-info/body)

	    (defhydra hydra-window-resize ()
	      "Window resize"
	      ("l" shrink-window-horizontally "shrink-right-window")
	      ("h" enlarge-window-horizontally "shrink-left-windo")
	      ("k" enlarge-window "shrink-up-window")
	      ("j" shrink-window "shrink-below-window")
	      ("q" nil "quit" :exit t)
	      )
	    )
  )


(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "i")
			(lambda () (interactive) (find-alternate-file "..")))))

(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map "z" 'dired-get-size)
(define-key dired-mode-map "F" 'find-name-dired)
(define-key dired-mode-map (kbd "%^") 'dired-flag-garbage-files)

;;; There is a bug with eshell-mode-map, so I change keybinding with add-hook
;;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2016-02/msg01532.html
;;; https://github.com/noctuid/general.el/issues/80
;; Eshll-mode
(defun ramsay/eshell-define-key-dwim (keymap keybinding function)
  "Unset original keybinding in KEYMAP, and then set KEYBINDING with FUNCTION."
  (define-key keymap [remap (lookup-key keymap keybinding)] function)
  (define-key keymap keybinding function)
  )
(add-hook 'eshell-mode-hook
          (lambda ()))
(message "loading init-keybindings")
(provide 'init-keybindings)
;;; init-keybindings ends here
