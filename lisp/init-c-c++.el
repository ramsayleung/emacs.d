;;; package --- Summary:
;;; Commentary:
;;; Code:

(use-package irony
  :ensure t
  :init (progn
	  (defun samray/irony-mode-hook ()
	    (define-key irony-mode-map [remap completion-at-point] 'counsel-irony)
	    (define-key irony-mode-map [remap complete-symbol] 'counsel-irony))
	  (add-hook 'c++-mode-hook 'irony-mode)
	  (add-hook 'c-mode-hook 'irony-mode)
	  (add-hook 'irony-mode-hook 'samray/irony-mode-hook)
	  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
  )
(use-package clang-format
  :ensure t
  :commands (clang-format-region clang-format-buffer)
  )
(use-package cmake-ide
  :ensure t
  :init (progn
	  (cmake-ide-setup)
	  )
  )
(use-package rtags
  :ensure t
  :init (progn
	  (setq rtags-completions-enabled t)
	  (eval-after-load 'company
	    '(add-to-list
	      'company-backends 'company-rtags))
	  (setq rtags-autostart-diagnostics t)
	  (rtags-enable-standard-keybindings)
	  )
  )
(provide 'init-c-c++)

;;; init-c-c++.el ends here

