;;; package --- Summary -*- lexical-binding: t -*-
;;; code:
;;; Commentary:
(use-package sis
  :ensure t
  ;; :hook
  ;; enable the /follow context/ and /inline region/ mode for specific buffers
  ;; (((text-mode prog-mode) . sis-context-mode)
  ;;  ((text-mode prog-mode) . sis-inline-mode))

  :config
  ;; For Linux
  (when (ramsay/linux-p)
    (sis-ism-lazyman-config "1" "2" 'fcitx5))
  ;; For MacOS
  (when (ramsay/mac-os-p)
    (sis-ism-lazyman-config
     
     ;; English input source may be: "ABC", "US" or another one.
     ;; "com.apple.keylayout.ABC"
     "com.apple.keylayout.ABC"

     ;; Other language input source: "rime", "sogou" or another one.
     ;; "im.rime.inputmethod.Squirrel.Rime"
     ;; "com.sogou.inputmethod.sogouWB.wubi" => "wubi"
     "im.rime.inputmethod.Squirrel.Hans"
     ))
  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  (setq sis-default-cursor-color "#000000")
  (setq sis-other-cursor-color "#FF2121")
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t)
  )

(message "loading init-chinese")
(provide 'init-chinese)
;;; init-chinese.el ends here
