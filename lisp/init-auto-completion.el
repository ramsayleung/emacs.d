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
	 company-global-modes '(not org-mode)
	 company-minimum-prefix-length 2
	 company-require-match 0
	 company-selection-wrap-around t
	 company-dabbrev-downcase nil
	 company-echo-delay 0                          ; remove annoying blinking
	 company-tooltip-limit 20                      ; bigger popup window
	 company-tooltip-align-annotations 't          ; align annotations to the right tooltip border
	 company-idle-delay .4                         ; decrease delay before autocompletion popup shows
	 company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  :config (progn
	    (global-company-mode t)
	    )
  )

(use-package company-quickhelp
  :ensure t
  :defer t
  :hook (company-mode . company-quickhelp-mode)
  :init (progn
	  )
  )

(use-package company-statistics
  :ensure t
  :defer t
  :hook (company-mode . company-statistics-mode)
  )

;; company-mode completion back-end for python JEDI
(use-package company-jedi
  :ensure t
  :defer t
  :init(progn
	 (run-with-idle-timer 1 nil (lambda () (add-to-list 'company-backends 'company-jedi))))
  )

;; HTML completion
(use-package company-web
  :ensure t
  :defer t
  :init(progn
	 (run-with-idle-timer 1 nil (lambda () (add-to-list 'company-backends 'company-web-html))))
  )

;;; Shell Script completion
(use-package company-shell
  :ensure t
  :defer t
  :init(progn
	 (run-with-idle-timer 1 nil (lambda () (add-to-list 'company-backends 'company-shell))))
  )

;;; C/C++ headers completion
(use-package company-c-headers
  :ensure t
  :init (progn
	  (cond ((samray/linux-p)
		 (setq company-c-headers-path-system '("/usr/include/c++/7" "/usr/include" "/usr/local/include")))
		((samray/mac-os-p)
		 (setq company-c-headers-path-system '("/usr/include/c++/4.2.1" "/usr/include" "/usr/local/include")))
		((eq system-type 'windows-nt)
		 ))
	  (run-with-idle-timer 1 nil (lambda () (add-to-list 'company-backends 'company-c-headers)))
	  )
  )

;;; backends for restclient
(use-package company-restclient
  :ensure t
  :defer t
  :init (progn
	  (run-with-idle-timer 1 nil (lambda () (add-to-list 'company-backends 'company-restclient)))
	  ))
;;; backends for go
(use-package company-go
  :ensure t
  :defer t
  :init (progn
	  (run-with-idle-timer 1 nil (lambda () (add-to-list 'company-backends 'company-go)))
	  )
  )
;; backends for rust
(use-package company-racer
  :ensure t
  :defer t
  :init (progn
	  (run-with-idle-timer 1 nil (lambda () (add-to-list 'company-backends 'company-racer)))
	  ))

;;; backends for lsp-mode
(use-package company-lsp
  :ensure t
  :defer t
  :init (progn
	  (run-with-idle-timer 1 nil (lambda () (add-to-list 'company-backends 'company-lsp)))
	  ))

(message "loading init-auto-completion")
(provide 'init-auto-completion)
;;; init-auto-completion.el ends here
