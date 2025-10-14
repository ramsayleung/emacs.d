;;; Package --- Summary
;;; code:
;;; commentary:
(use-package general
  :ensure t
  :config (progn
	    (general-evil-setup t)
	    (defvar ramsay/leader-key "SPC") ;
	    (general-define-key :states '(normal visual motion )
				:prefix ramsay/leader-key
				"" nil
				";" 'comment-dwim
				"'" 'shell-pop
				"." 'ramsay/python-send-repl-echo-switch
				"a" '(:ignore t :which-key "applications")
				"a t" '(:ignore t :which-key "translator")
				"a t d" 'gts-do-translate
				"b" '(:ignore t :which-key "buffers")
				"b f" 'ramsay/get-buffer-full-name
				"b k" 'ramsay/kill-other-buffers
				"b n" 'ramsay/get-buffer-name
				"e" #'hydra-flycheck/body
				"f" '(:ignore t :which-key "files")
				"f d" 'ramsay/delete-current-buffer-file
				"f E" 'ramsay/sudo-edit
				"f f"  'counsel-find-file
				"f g" 'ramsay/counsel-goto-recent-directory
				"f r" 'rename-buffer
				"f R" 'recentf-open-files
				"f s" 'save-buffer
				"h" '(:ignore t :which-key "help")
				"h d" '(:ignore t :which-key "help-describe")
				"h d d" 'apropos-documentation
				"h d f" 'counsel-describe-function
				"h d k" 'counsel-describe-key
				"h d v" 'counsel-describe-variable
				"g" '(:ignore t :which-key "git/version-control")
				"g s" 'magit-status
				"j" '(:ignore t :which-key "jump/join/split")
				"j l" 'avy-goto-line
				"j j" 'avy-goto-char
				"j w" 'avy-goto-word-1
				"k" #'hydra-flymake/body
				"m" '(:ignore t :which-key "major-mode-cmd")
				"p" '(:ignore t :which-key "projects")
				"p f" 'project-find-file
				"p d" 'project-find-dir
				"p b" 'project-switch-to-buffer
				"p p" 'project-switch-to-buffer
				"p s" '(:ignore t :which-key "project search")
				"p s r" 'project-find-regexp
				"q" '(:ignore t :which-key "quit")
				"q s" 'save-buffers-kill-terminal
				"v" 'er/expand-region
				"s" '(:ignore t :which-key "search")
				"s g" 'counsel-grep
				"s r" 'counsel-rg
				"s s" 'isearch-forward)

	    (general-define-key :states '(normal visual motion)
				:keymaps 'org-mode-map
				;; "h" 'evil-backward-char
				[remap org-beginning-of-line] 'evil-backward-char
				[remap org-end-of-line] 'evil-forward-char
				"C-c k" 'ramsay/insert-key
				)

	    (general-define-key :states 'normal
			:keymaps 'org-mode-map
				"TAB" 'org-cycle
				"t" 'org-todo)

	    (general-define-key :keymaps 'smerge-mode-map
				"C-c ^" 'hydra-smerge/body)

	    ;; profiler-report-mode
	    (general-define-key :keymaps 'profiler-report-mode-map
				"<tab>" 'profiler-report-expand-entry)

	    (general-define-key "C-c h" '(:ignore t :which-key "ivy-command-prefix")
				"C-c h a" 'counsel-apropos
				"C-c h b" 'ivy-resume
				"C-c h c" 'counsel-colors-emacs
				"C-c h i" 'counsel-imenu
				"C-c h l" 'counsel-locate
				"C-c h p" 'counsel-list-processes
				)
	    (general-define-key
	     "C-`" 'shell-pop
	     "C-s" 'swiper-isearch
	     "C-x b" 'switch-to-buffer
	     "C-x C-b" 'ibuffer
	     "C-x C-r" 'counsel-recentf
	     "C-x C-f" 'counsel-find-file
	     "M-x" 'counsel-M-x)

	    ;; non-evil ,without a prefix
	    (general-define-key
	     [remap evil-repeat-pop-next] 'xref-find-definitions
	     "<f5>" 'deadgrep
	     "C-c a" 'org-agenda
	     "C-c c" 'compile
	     "C-c l" 'org-store-link
         "C-c r" 'grep
	     "C-c t d" 'gts-do-translate
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
         "C-x w d" 'hydra-window/body
	     "S-<f6>" 'eglot-rename
	     )

	    (general-define-key :states '(insert normal visual motion)
				"C-a" 'move-beginning-of-line
				"C-e" 'move-end-of-line
				"C-w" 'kill-region
				"C-y" 'yank)
	    )

  (general-define-key :keymaps '(emacs-lisp-mode-map
				 ielm-mode-map
				 lisp-mode-map
				 scheme-mode-map
				 lisp-interaction-mode-map)
		      [remap paredit-convolute-sexp] 'xref-find-references
		      "M-?" 'xref-find-references)

  ;; Loop through the list and set up keybindings
  (dolist (pair '((go-mode-map . gofmt)
		  (rust-mode-map . rust-format-buffer)
		  (rust-ts-mode-map . rust-format-buffer)
		  (prog-mode-map . ramsay/indent-region-or-buffer)))
    (general-define-key :keymaps (car pair)
			"C-M-\\" (cdr pair)))

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

	    (defhydra hydra-flymake
	      (:pre (progn (setq hydra-hint-display-type 'lv) (flymake-show-buffer-diagnostics))
		    :post (progn
			    (setq hydra-hint-display-type 'lv)
			    (dolist (window (window-list))
			      (let ((buffer (window-buffer window)))
				(when (string-prefix-p "*Flymake" (buffer-name buffer))
				  (quit-window nil window)))))		    :hint nil)
	      "errors"
	      ("j"  flymake-goto-next-error                                       "Next")
	      ("k"  flymake-goto-prev-error                                   "Previous")
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

(defhydra hydra-window (:color pink :hint nil :timeout 20)
"
          Move                        Resize                Operation
-------------------------------------------------------------------------
         ^_<up>_^                    ^_M-<up>_^             
          ^^▲^^                         ^^▲^^               
 _<left>_ ◀   ▶ _<right>_    _M-<left>_ ◀   ▶ _M-<right>_   delete: [_d_]
          ^^▼^^                         ^^▼^^               quit: [_q_]  
        ^_<down>_^                  ^_M-<down>_^            
"
  ("<left>" windmove-left)
  ("<down>" windmove-down)
  ("<up>" windmove-up)
  ("<right>" windmove-right)
  ("d" delete-window)
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("M-<left>" enlarge-window-horizontally)
  ("M-<right>" shrink-window-horizontally)
  ("M-<up>" shrink-window)
  ("M-<down>" enlarge-window)
  ("q" nil :exit t))
	    )
  )

(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map "F" 'find-name-dired)
(define-key dired-mode-map (kbd "%^") 'dired-flag-garbage-files)

(message "loading init-keybindings")
(provide 'init-keybindings)
;;; init-keybindings ends here
