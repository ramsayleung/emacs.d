;;; package --- Summary: -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package clang-format
  :ensure t
  :commands (clang-format-region clang-format-buffer)
  :config (progn
	    (setq clang-format-style "google"))
  )

(use-package cc-mode
  :mode
  (("\\.c\\'" . c-mode)
   ("\\.h\\'" . c++-mode)
   ("\\.hpp\\'" . c++-mode)
   ("\\.cpp\\'" . c++-mode))
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

;;; Syntax check and code-completion with CMake project
(use-package cpputils-cmake
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode)
                (cppcm-reload-all)))))

;;; Semantic Refactor is a C/C++ refactoring tool based on Semantic parser framework.
(use-package srefactor
  :ensure t)

;;; http://coldnew.github.io/coldnew-emacs/#orgheadline267
;;; Use regexp to find header is C++ header or not
(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (string= (file-name-extension (or (buffer-file-name) "")) "h")
                       (or (re-search-forward "#include <\\w+>"
                                              magic-mode-regexp-match-limit t)
                           (re-search-forward "\\W\\(class\\|template\\namespace\\)\\W"
                                              magic-mode-regexp-match-limit t)
                           (re-search-forward "std::"
                                              magic-mode-regexp-match-limit t))))
               . c++-mode))

;;; Extra highlight keywords for C/C++
(dolist (m '(c-mode c++-mode))
  (font-lock-add-keywords
   m
   '(("\\<\\(int8_t\\|int16_t\\|int32_t\\|int64_t\\|uint8_t\\|uint16_t\\|uint32_t\\|uint64_t\\)\\>" . font-lock-keyword-face))))

;;; Comment #if 0 #endif region
(defun samray/cc-mode/highlight-if-0 ()
  "Highlight c/c++ #if 0 #endif macros."
  (setq cpp-known-face 'default)
  (setq cpp-unknown-face 'default)
  (setq cpp-known-writable 't)
  (setq cpp-unknown-writable 't)
  (setq cpp-edit-list '(("0" '(foreground-color . "gray")  default both)
                        ("1" default font-lock-comment-face both)))
  (cpp-highlight-buffer t))

;; Add to c/c++ mode
(defun samray/cc-mode/highlight-if-0-hook ()
  "Highlight if-0 in `cc-mode`."
  (when (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode))
    (samray/cc-mode/highlight-if-0)))
(add-hook 'after-save-hook #'samray/cc-mode/highlight-if-0-hook)

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

(defvar samray-default-g++-compile-command "g++ -std=c++17 -g")
(defvar samray-default-gcc-compile-command "g++ -std=c99 -g")
(defvar samray-default-compile-command "make -k")

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


;;; http://coldnew.github.io/coldnew-emacs/
(defun samray/cc-mode/highlight-if-0 ()
  "Highlight c/c++ #if 0 #endif macros."
  (setq cpp-known-face 'default)
  (setq cpp-unknown-face 'default)
  (setq cpp-known-writable 't)
  (setq cpp-unknown-writable 't)
  (setq cpp-edit-list '(("0" '(foreground-color . "gray")  default both)
                        ("1" default font-lock-comment-face both)))
  (cpp-highlight-buffer t))

;; Add to c/c++ mode
(defun samray/cc-mode/highlight-if-0-hook ()
  (when (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode))
    (samray/cc-mode/highlight-if-0)))
(add-hook 'after-save-hook #'samray/cc-mode/highlight-if-0-hook)
;;; Extra highlight keywords for C/C++
(dolist (m '(c-mode c++-mode))
  (font-lock-add-keywords
   m
   '(("\\<\\(int8_t\\|int16_t\\|int32_t\\|int64_t\\|uint8_t\\|uint16_t\\|uint32_t\\|uint64_t\\)\\>" . font-lock-keyword-face))))
(provide 'init-c-c++)

;;; init-c-c++.el ends here

