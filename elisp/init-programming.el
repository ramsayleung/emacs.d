;;; package --- Summary
;;; code:
;;; Commentary:
(use-package yasnippet
  :ensure t
  :config (progn
	    (add-hook 'prog-mode-hook #'yas-minor-mode)
	    (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
	    ))
;;; https://www.emacswiki.org/emacs/AutoFillMode
;;; auto format comment to 80-char long
(setq-default fill-column 80)
(defun comment-auto-fill ()
  "Auto fill comments but not code in programmingModes."
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))
(with-eval-after-load 'prog-mode
  (add-hook 'prog-mode-hook 'comment-auto-fill))
(provide 'init-programming)
;;; init-programming.el ends here
