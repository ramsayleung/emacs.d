;;; package --- Summary:
;;; Commentary:
;;; Code:

(use-package irony
  :ensure t
  :init (progn
	  (defun samray/irony-mode-hook ()
	    (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
	    (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))
	  (add-hook 'c++-mode-hook 'irony-mode)
	  (add-hook 'c-mode-hook 'irony-mode)
	  (add-hook 'irony-mode-hook 'samray/irony-mode-hook)
	  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
  )
(use-package clang-format
  :ensure t
  :commands (clang-format-region clang-format-buffer)
  )
(provide 'init-c-c++)

;;; init-c-c++.el ends here
