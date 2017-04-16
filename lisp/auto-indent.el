;;; auto-indent.el --- Auto indent buffers when idle

;; Filename: auto-indent.el
;; Description: Auto indent buffers when idle
;; Author: Samray Leung samrayleung@gmail.com
;; Maintainer: Samray Leung samrayleung@gmail.com
;; Copyright (C) 2017 ~ 2017, Samray Leung, all rights reserved.
;; Created: 2017-03-08 22:52:00
;; Version: 0.1
;; Last-Updated: 2017-03-08 22:52:00
;;           By: Samray Leung
;; URL:
;; Keywords: autoindent
;; Compatibility: GNU Emacs 25.1.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Auto indent file when Emacs idle
;;

;;; Installation:
;;
;; Put auto-indent.el to your load-path.
;; The load-path is usually ~/lisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/lisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-auto-indent)
;; (auto-indent-enable)
;;
;; Set `auto-indent-slient' with non-nil if want Emacs save files slient:
;; (setq auto-indent-slient t)
;;
;; No need more.

;;; Change log:
;;
;;
;; 2017/03/08
;;      First released.
;;

;;; Acknowledgements:
;; Thanks for Andy Stewart lazycat.manatee@gmail.com who inspire me how to write package
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:
(defgroup auto-indent nil
  "Auto indent buffer when emacs idle"
  :group 'auto-indent
  )

(defcustom auto-indent-idle 20
  "The idle seconds to auto indent file."
  :type 'integer
  :group 'auto-indent)

(defcustom auto-indent-slient nil
  "Nothing to display in minibuffer if this option is non-nil."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-prog-enable nil
  "Only enable auto-indent in mode which derive from 'prog-mode'."
  :type 'boolean
  :group 'auto-indent)

(defcustom auto-indent-excluded-modes-list
  '(python-mode)
  "List Major mode will be excluded."
  :type '(repeat (symbol :tag "Major mode"))
  :group 'auto-indent)

;; auto indent file before save file
(defun indent-buffer()
  (interactive)
  (indent-region (point-min)(point-max)))

(defun auto-indent-buffers ()
  (interactive)
  (let ((autoindent-buffer-list))
    (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (if (buffer-file-name)
            (progn
              (if (and auto-indent-prog-enable (derived-mode-p 'prog-mode))
                  (unless (memq major-mode auto-indent-excluded-modes-list)
                    (progn
                      (push (buffer-name)autoindent-buffer-list)
                      (if auto-indent-slient
                          (with-temp-message ""
                            (indent-buffer))
                        (indent-buffer)))))
              (unless auto-indent-prog-enable
                (unless (memq major-mode auto-indent-excluded-modes-list)
                  (progn
                    (push (buffer-name)autoindent-buffer-list)
                    (if auto-indent-slient
                        (with-temp-message ""
                          (indent-buffer))
                      (indent-buffer))))))))
      (unless auto-indent-slient
        (cond
         ((= (length autoindent-buffer-list) 1)
          (message "# Indented %s" (car autoindent-buffer-list)))
         ((> (length autoindent-buffer-list) 1)
          (message "# Indented :%s"
                   (length autoindent-buffer-list)
                   (mapconcat 'identity autoindent-buffer-list ","))))))))
(defun auto-indent-enable ()
  (interactive)
  (run-with-idle-timer auto-indent-idle t #'auto-indent-buffers))

(provide 'auto-indent)
;;; auto-indent.el ends here
