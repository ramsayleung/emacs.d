;;; package --- Summary: -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package company
  :ensure t
  :demand t
  :diminish company-mode
  :commands (company-mode
             company-complete
             company-complete-common
             company-complete-common-or-cycle
             company-files
             company-dabbrev
             company-jedi
             company-tern
             company-web-html
             )
  :init (setq
	 company-minimum-prefix-length 2
	 company-require-match nil
	 company-selection-wrap-around t
	 company-echo-delay 0                          ; remove annoying blinking
	 company-tooltip-limit 10                      ; bigger popup window
	 company-tooltip-align-annotations 't          ; align annotations to the right tooltip border
	 company-idle-delay .15                         ; decrease delay before autocompletion popup shows
	 company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (global-company-mode t)
  )

(use-package company-quickhelp
  :ensure t
  :defer t
  :hook (company-mode . company-quickhelp-mode)
  )

(use-package company-posframe
  :diminish company-posframe-mode
  :ensure t
  :init
  (company-posframe-mode 1)
  )


(message "loading init-auto-completion")
(provide 'init-auto-completion)
;;; init-auto-completion.el ends here
