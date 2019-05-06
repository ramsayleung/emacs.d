((magit-blame
  ("-w"))
 (magit-commit nil)
 (magit-gitignore nil)
 (magit-log
  ("-n256" "--graph" "--decorate")
  ("-n256"
   ("--" "~/.emacs.d/lisp/init-lsp.el")
   "--graph" "--decorate" "--patch"))
 (magit-pull nil)
 (magit-push nil)
 (magit:-- "~/.emacs.d/lisp/init-lsp.el" ""))
