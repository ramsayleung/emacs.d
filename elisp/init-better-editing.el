;;; package --- Summary
;;; code:
;;; Commentary:

(use-package window-numbering
  :ensure t
  :config(progn
	   (window-numbering-mode t)))
(use-package smartparens
  :ensure t
  :config
  (progn
    (smartparens-global-mode t)
    ;; show single quote "'" in emacs and lisp-interaction-mode instead of single quote pair "''"
    (sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "'" nil :actions nil)
    )
  )

;; delete spaces at once
(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode t))
(use-package which-key
  :ensure t
  :config(progn
	   (which-key-mode t)
	   (setq which-key-idle-delay 0.3)
	   (which-key-add-key-based-replacements (concat evil-leader/leader " a") "applications")
	   (which-key-add-key-based-replacements (concat evil-leader/leader " a o") "org-mode")
	   (which-key-add-key-based-replacements (concat evil-leader/leader " b") "buffers")
	   (which-key-add-key-based-replacements (concat evil-leader/leader " c") "compile/comments")
	   (which-key-add-key-based-replacements (concat evil-leader/leader " e") "errors")
	   (which-key-add-key-based-replacements (concat evil-leader/leader " f") "files")
	   (which-key-add-key-based-replacements (concat evil-leader/leader " g") "git/version-control")
	   (which-key-add-key-based-replacements (concat evil-leader/leader " h") "help")
	   (which-key-add-key-based-replacements (concat evil-leader/leader " h d") "describe")
	   (which-key-add-key-based-replacements (concat evil-leader/leader " j") "jump/join/split")
	   (which-key-add-key-based-replacements (concat evil-leader/leader " o") "misc")
	   (which-key-add-key-based-replacements (concat evil-leader/leader " p") "projects")
	   (which-key-add-key-based-replacements (concat evil-leader/leader " p s") "project-search")
	   (which-key-add-key-based-replacements (concat evil-leader/leader " q") "quit")
	   (which-key-add-key-based-replacements (concat evil-leader/leader " s") "search/symbol")
	   (which-key-add-key-based-replacements (concat evil-leader/leader " t") "toggles")
	   (which-key-add-key-based-replacements (concat evil-leader/leader " w") "windows")
	   ))
(use-package iedit
  :ensure t)
(provide 'init-better-editing)
;;; init-better-editing.el ends here
