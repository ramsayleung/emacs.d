;;; package --- Summary: -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package clang-format
  :ensure t
  :commands (clang-format-region clang-format-buffer)
  :config (progn
	    (setq clang-format-style "llvm"))
  )

(use-package cc-mode
  :mode
  (("\\.c\\'" . c-mode)
   ("\\.h\\'" . c++-mode)
   ("\\.hpp\\'" . c++-mode)
   ("\\.cpp\\'" . c++-mode))
  :config (progn
	    (eval-after-load "cc-mode"
	      '(define-key c-mode-base-map ";" nil)))
  )

(use-package cmake-mode
  :ensure t
  :mode (
	 ("CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)
	 ))

(defun samray/compile-with-command-and-run (command)
  "Compile c/c++ with COMMAND and run it."
  (let ((file-name (buffer-file-name))
	(output (file-name-base (buffer-file-name))))
    (message output)
    (compile (format "%s %s -o %s && ./%s && rm %s" command file-name output output output))
    )
  )

(defvar samray-default-g++-compile-command "g++ -std=c++17 -Wall")
(defvar samray-default-gcc-compile-command "gcc -std=c99 -Wall")
(defvar samray-default-compile-command "cmake --build . -j 4")
(defvar samray-default-clean-command "cmake --build . --target clean")

(defun samray/g++-compile-and-run (command)
  "Compile cpp with g++ and run it with COMMAND."
  (interactive
   (list (read-string (format "compile and run command [default: %s]: " samray-default-g++-compile-command) nil nil samray-default-g++-compile-command)))
  (samray/compile-with-command-and-run command)
  )

(defun samray/gcc-compile-and-run (command)
  "Compile c with gcc and run it COMMAND."
  (interactive
   (list (read-string (format "compile and run command [default: %s]: " samray-default-gcc-compile-command) nil nil samray-default-gcc-compile-command)))
  (samray/compile-with-command-and-run command)
  )

(defun samray/compile (command)
  "Compile with input COMMAND or samray-default-compile-command."
  (interactive
   (list (read-string (format "compile command [default: %s]: " samray-default-compile-command) nil nil samray-default-compile-command)))
  ;; (setq samray-default-compile-command command)
  (compile command)
  )

(defun samray/compile-clean (command)
  "Clean compiled object with COMMAND."
  (interactive
   (list (read-string (format "clean command [default: %s]: " samray-default-clean-command) nil nil samray-default-compile-command)))
  ;; (setq samray-default-compile-command command)
  (compile command)
  )

(message "loading init-c-c++")
(provide 'init-c-c++)

;;; init-c-c++.el ends here

