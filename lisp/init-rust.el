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

(defun samray/get-rust-src-path ()
  "Get rust src path by `rustc`."
  (let* ((command (concat "rustc --print sysroot"))
	 (rustc-sysroot-path (shell-command-to-string command))
	 (strip-path (replace-regexp-in-string "\n$" "" rustc-sysroot-path)))
    (if (eq system-type 'windows-nt)
	(concat strip-path "\\lib\\rustlib\\src\\rust\\src")
      (concat strip-path "/lib/rustlib/src/rust/src"))))
(setenv "RUST_SRC_PATH" (samray/get-rust-src-path))
(defun samray/set-rust-ld-library-path ()
  (let* ((command (concat "rustc --print sysroot"))
	 (rustc-sysroot-path (shell-command-to-string command))
	 (strip-path (replace-regexp-in-string "\n$" "" rustc-sysroot-path)))
    (if (eq system-type 'windows-nt)
	(concat strip-path "\\lib\\")
      (concat strip-path "/lib/"))))
(setenv "LD_LIBRARY_PATH" (samray/set-rust-ld-library-path))

;;; https://github.com/rust-lang/rust-mode/issues/208
(setq rust-match-angle-brackets nil)

;; (use-package racer
;;   :ensure t
;;   :defer t
;;   :init (progn
;; 	  ;; Set path to racer binary
;; 	  (setq racer-cmd (expand-file-name "~/.cargo/bin/racer"))
;; 	  ;; Set path to rust src directory
;; 	  (setq racer-rust-src-path  (getenv "RUST_SRC_PATH"))
;;   	  (add-hook 'rust-mode-hook 'racer-mode)
;; 	  (add-hook 'racer-mode-hook 'eldoc-mode)
;; 	  )
;;   )
(use-package cargo
  :ensure t
  :defer t
  :init (progn
	  (add-hook 'rust-mode-hook 'cargo-minor-mode)))
(defvar samray/cargo-process--command-script "script")
(defun samray/cargo-process-script ()
  "Run cargo script command to compile and run a single file."
  (interactive)
  (cargo-process--start (concat "Script " (buffer-file-name))
                        (concat samray/cargo-process--command-script
                                " "
                                (buffer-file-name))))

(defun samray/cargo-process-run-current-example ()
  "Run current buffer/file as example."
  (interactive)
  (cargo-process-run-example (file-name-sans-extension (buffer-name))))

(provide 'init-rust)
;;; init-rust.el ends here
