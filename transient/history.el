((magit-blame
  ("-w"))
 (magit-commit nil)
 (magit-dispatch nil)
 (magit-gitignore nil)
 (magit-log
  ("-n256" "--graph" "--decorate")
  ("-n256"
   ("--" "~/.emacs.d/lisp/init-lsp.el")
   "--graph" "--decorate" "--patch"))
 (magit-pull nil)
 (magit-push nil)
 (magit-remote
  ("-f"))
 (magit:-- "~/.emacs.d/lisp/init-lsp.el" ""))
