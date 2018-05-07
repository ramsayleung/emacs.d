;;; package --- Summary:
;;; Commentary:
;;; Code:


(use-package clang-format
  :ensure t
  :commands (clang-format-region clang-format-buffer)
  )

;;; Google C style
(use-package google-c-style
  :load-path "~/.emacs.d/additional-packages/google-c-style.el")

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
    (compile (format "%s %s -o %s && ./%s" command file-name output output))
    )
  )

(defun samray/compile-clean ()
  "Clean compiled object."
  (interactive)
  (compile "make clean")
  )

(defun samray/g++-compile-and-run ()
  "Compile cpp with g++ and run it."
  (interactive)
  (samray/compile-with-command-and-run "g++")
  )
(defun samray/gcc-compile-and-run ()
  "Compile c with gcc and run it."
  (interactive)
  (samray/compile-with-command-and-run "gcc")
  )

(defvar samray-default-compile-command "make -k")
(defun samray/compile (command)
  "Compile with input COMMAND or samray-default-compile-command."
  (interactive
   (list (read-string (format "compile command [default: %s]: " samray-default-compile-command) nil nil samray-default-compile-command)))
  ;; (setq samray-default-compile-command command)
  (compile command)
  )


;;; Tell emacs to open .h file in C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(provide 'init-c-c++)

;;; init-c-c++.el ends here

