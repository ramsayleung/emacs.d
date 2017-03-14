;;; package --- Summary
;;; code:
;;; Commentary:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cfs--current-profile "profile1" t)
 '(cfs--profiles-steps (quote (("profile1" . 4))) t)
 '(company-minimum-prefix-length 1)
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("2997ecd20f07b99259bddba648555335ffb7a7d908d8d3e6660ecbec415f6b95" "d9129a8d924c4254607b5ded46350d68cc00b6e38c39fc137c3cfb7506702c12" "eb0a314ac9f75a2bf6ed53563b5d28b563eeba938f8433f6d1db781a47da1366" default)))
 '(eldoc-idle-delay 0.08)
 '(evil-leader/leader "SPC")
 '(evil-want-C-u-scroll t)
 '(expand-region-contract-fast-key "V")
 '(fci-rule-color "#383838")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(indent-tabs-mode nil)
 '(magit-diff-use-overlays nil)
 '(nil nil t)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files
   (quote
    ("~/SyncDirectory/Org/todo.org" "~/SyncDirectory/Org/agenda.org")))
 '(org-capture-templates
   (quote
    (("a" "Agenda" entry
      (file "~/SyncDirectory/Org/agenda.org" "Agenda")
      "* TODO %?
:PROPERTIES:

:END:
DEADLINE: %^T 
 %i
")
     ("n" "Note" entry
      (file+headline "~/SyncDirectory/Org/notes.org" "Notes")
      "* Note %?
%T")
     ("l" "Link" entry
      (file+headline "~/SyncDirectory/Org/links.org" "Links")
      "* %? %^L %^g 
%T" :prepend t)
     ("b" "Blog idea" entry
      (file+headline "~/SyncDirectory/Org/blog.org" "Blog Topics:")
      "* %?
%T" :prepend t)
     ("t" "To Do Item" entry
      (file+headline "~/SyncDirectory/Org/todo.org" "To Do Items")
      "* TODO  %?
  %i
" :prepend t)
     ("j" "Journal" entry
      (file+datetree "~/SyncDirectory/Org/journal.org")
      "* %?
Entered on %U
  %i
  %a"))) t)
 '(package-selected-packages
   (quote
    (4clojure ivy-historian gruvbox-theme moe-theme theme-changer clues-theme blackboard-theme color-theme-sanityinc-tomorrow highlight-indentation-mode golden-ratio evil-smartparens evil-smartparents auto-compile perspective eyebrowse company-tern org-preview-html org-latex babel clang-format company-c-headers irony irony-mode company-shell highlight-symbol imenu-list diff-hl git-gutter workgroups2 popup project-explorer imenu-anywhere imenus dumb-jump evil-escape keyfreq rainbow-mode rainbow-blocks selectric-mode org-present better-shell virtualenvwrapper try org-download company-anaconda company-quickhelp org-bullets rainbow-delimiters markdown-mode wakatime-mode counsel-projectile org-pomodoro company flycheck swiper counsel smartparens expand-region dired+ js2-mode web-mode magit nodejs-repl exec-path-from-shell popwin js2-refactor monokai-theme zenburn-theme dracula-theme molokai-theme use-package)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(wakatime-api-key "df5310e6-9d1f-4cf6-bd9a-fd6a417a6c93")
 '(wakatime-cli-path "/usr/bin/wakatime")
 '(wakatime-python-bin "/usr/bin/python")
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
;;; custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "aquamarine"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "cadet blue"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "rosy brown"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "medium spring green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "DarkGoldenrod4"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))
