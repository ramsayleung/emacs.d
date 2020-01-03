;; package --- Summary
;;; Code:
;;; Commentary:

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :config (remove-hook 'rust-mode-hook 'adaptive-wrap-prefix-mode)
  )

(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'" . toml-mode))

(defun ramsay/get-rust-src-path ()
  "Get rust src path by `rustc`."
  (let* ((command (concat "rustc --print sysroot"))
	 (rustc-sysroot-path (shell-command-to-string command))
	 (strip-path (replace-regexp-in-string "\n$" "" rustc-sysroot-path)))
    (if (eq system-type 'windows-nt)
	(concat strip-path "\\lib\\rustlib\\src\\rust\\src")
      (concat strip-path "/lib/rustlib/src/rust/src"))))
(setenv "RUST_SRC_PATH" (ramsay/get-rust-src-path))
(defun ramsay/set-rust-ld-library-path ()
  "Set rust link library path."
  (let* ((command (concat "rustc --print sysroot"))
	 (rustc-sysroot-path (shell-command-to-string command))
	 (strip-path (replace-regexp-in-string "\n$" "" rustc-sysroot-path)))
    (if (eq system-type 'windows-nt)
	(concat strip-path "\\lib\\")
      (concat strip-path "/lib/"))))
(setenv "LD_LIBRARY_PATH" (ramsay/set-rust-ld-library-path))

;;; https://github.com/rust-lang/rust-mode/issues/208
(setq rust-match-angle-brackets nil)

(use-package cargo
  :diminish cargo-minor-mode
  :ensure t
  :defer t
  :init (progn
	  (add-hook 'rust-mode-hook 'cargo-minor-mode)))
(defvar ramsay/cargo-process--command-script "script")
(defun ramsay/cargo-process-script ()
  "Run cargo script command to compile and run a single file."
  (interactive)
  (cargo-process--start (concat "Script " (buffer-file-name))
                        (concat ramsay/cargo-process--command-script
                                " "
                                (buffer-file-name))))

(defun ramsay/cargo-process-run-current-example ()
  "Run current buffer/file as example."
  (interactive)
  (cargo-process-run-example (file-name-sans-extension (buffer-name))))

(message "loading init-rust")
(provide 'init-rust)
;;; init-rust.el ends here
