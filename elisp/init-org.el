;;; package --- Summary:
;;; Commentary:
;;; Code:
(use-package org
  :ensure t
  :config(progn
	   (with-eval-after-load 'org
	     (setq org-src-fontify-natively t)
	     (setq org-agenda-files '("~/.emacs.d"))
	     (setq org-capture-templates
		   '(("t" "Todo" entry (file+headline "~/.emacs.d/gtd.org" "工作安排")
		      "* TODO [#B] %?\n  %i\n"
		      :empty-lines 1)))
	     )
	   )
  )
(use-package org-pomodoro
  :ensure t)
(provide 'init-org)
;;; init-org.el ends here
