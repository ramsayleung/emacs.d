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

;;; pomodoro tech
(use-package org-pomodoro
  :ensure t)

;;; show org-mode bullets as UTF-8 character
(use-package org-bullets
  :ensure t
  :config (progn
	    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
		(setq org-bullets-bullet-list '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"))
		 ))
(provide 'init-org)
;;; init-org.el ends here
