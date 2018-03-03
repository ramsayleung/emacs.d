;;; package --- Summary:
;;; Commentary:
;;; Code:

;; (use-package irony
;;   :ensure t
;;   :defer t
;;   :init (progn
;; 	  (defun samray/irony-mode-hook ()
;; 	    (define-key irony-mode-map [remap completion-at-point] 'counsel-irony)
;; 	    (define-key irony-mode-map [remap complete-symbol] 'counsel-irony))
;; 	  (add-hook 'c++-mode-hook 'irony-mode)
;; 	  (add-hook 'c-mode-hook 'irony-mode)
;; 	  (add-hook 'irony-mode-hook 'samray/irony-mode-hook)
;; 	  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
;;   )
(use-package clang-format
  :ensure t
  :commands (clang-format-region clang-format-buffer)
  )
;; (use-package cmake-ide
;;   :ensure t
;;   :defer t
;;   :init (progn
;; 	  (add-hook 'c++-mode-hook (lambda () (cmake-ide-setup)))
;; 	  (add-hook 'c-mode-hook (lambda () (cmake-ide-setup)))
;; 	  )
;;   )
(use-package cmake-mode
  :ensure t
  :mode (
	 ("CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)
	 ))
;; (use-package rtags
;;   :ensure t
;;   :defer t
;;   :init (progn
;; 	  (defun rtags-setup ()
;; 	    (setq rtags-completions-enabled t)
;; 	    (setq rtags-autostart-diagnostics t)
;; 	    (setq rtags-display-result-backend 'ivy)
;; 	    (rtags-enable-standard-keybindings))
;; 	  (add-hook 'c++-mode-hook #'rtags-setup)
;; 	  (add-hook 'c-mode-hook #'rtags-setup))
;;   )
;; (use-package ivy-rtags
;;   :ensure t
;;   :after rtags-mode
;;   )
;;; Tell emacs to open .h file in C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(provide 'init-c-c++)

;;; init-c-c++.el ends here

