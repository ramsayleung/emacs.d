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
	  (add-hook 'c++-mode-hook (lambda () (cmake-ide-setup)))
	  (add-hook 'c-mode-hook (lambda () (cmake-ide-setup)))
	  )
  )
(use-package cmake-mode
  :ensure t
  :mode (
	   ("CMakeLists\\.txt\\'" . cmake-mode)
	   ("\\.cmake\\'" . cmake-mode)
	  ))
(use-package rtags
  :ensure t
  :init (progn
	  (setq rtags-completions-enabled t)
	  (eval-after-load 'company
	    '(add-to-list
	      'company-backends 'company-rtags))
	  ;; (setq rtags-autostart-diagnostics t)
	  (rtags-enable-standard-keybindings)
	  )
  )
;;; Tell emacs to open .h file in C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(provide 'init-c-c++)

;;; init-c-c++.el ends here

