;;; package --- Summary
;;; code:
;;; Commentary:

(use-package youdao-dictionary
  :ensure t
  :commands (youdao-dictionary-search-at-point+ youdao-dictionary-search-from-input)
  :init
  (defun samray/youdao-dictionary-buffer-quit ()
    "Quit youdao-dictionary Buffer and delete window"
    (interactive)
    (kill-this-buffer)
    (delete-window)
    )
  :config (progn
	    (setq url-automatic-caching t)
	    ))

(use-package pyim
  :ensure t
  :config

  ;; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :ensure t
    :config (pyim-basedict-enable))
  (setq default-input-method "pyim")

  ;; 我使用全拼
  (setq pyim-default-scheme 'quanpin)
  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  ;; pyim-probe-program-mode
                  ;; pyim-probe-org-structure-template
  		  ))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  ;; 开启拼音搜索功能
  (setq pyim-isearch-enable-pinyin-search t)
  ;; 使用 pupup-el 来绘制选词框
  (setq pyim-page-tooltip 'popup)
  ;; 选词框显示 5 个候选词
  (setq pyim-page-length 8)
  ;; 让 Emacs 启动时自动加载 pyim 词库
  (add-hook 'emacs-startup-hook
            #'(lambda () (pyim-restart-1 t)))
  :bind
  (("M-n" . pyim-convert-code-at-point) ;与 pyim-probe-dynamic-english 配合
   ("C-c C-;" . pyim-delete-word-from-personal-buffer)))

(defun samray/delete-whitespace-between-english-and-chinese-char ()
  "Because the chinese input method i use,i will left whitespace between letter;
and chinese char,so just delete it"
  (interactive)
  (save-restriction
    (if (region-active-p)
        (narrow-to-region (region-beginning) (region-end)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "\\([a-zA-Z0-9]\\)[ ]+\\(\\cc\\)" nil t)
        (replace-match "\\1\\2" t nil))
      (while (search-forward-regexp "\\(\\cc\\)[ ]+\\([a-zA-Z0-9]\\)" nil t)
        (replace-match "\\1\\2" t nil))  ;because i could not figure out the
                                        ;right regexp,so just use such silly way
      )))

;;; solve ths issue that org-table cannot indent when english char mix with
;;; chinese char
;;; (set-frame-font "Source Code Pro-11")

;; (defun samray/handle-org-table-indent-with-chinese ()
;;   "Deal with  issue that chinese char cannot get along with org-table."
;;   (interactive)
;;   (when window-system
;;     (save-excursion
;;       (progn
;; 	(dolist (charset '(kana han symbol cjk-misc bopomofo))
;; 	  (set-fontset-font (frame-parameter nil 'font)
;; 			    charset
;; 			    (font-spec :family "WenQuanYi Micro Hei")))
;; 	;; tune rescale so that Chinese character width = 2 * English character width
;; 	(setq face-font-rescale-alist '((samray-current-font. 1.0) ("WenQuanYi" . 1.23)))
;; 	)
;;       )))
;; (add-hook 'after-init-hook 'samray/handle-org-table-indent-with-chinese)
;; (add-hook 'after-setting-font-hook 'samray/handle-org-table-indent-with-chinese)
(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                              "xelatex -interaction nonstopmode %f"))
;;; org-mode 导出中文的问题解决
;;org-mode export to latex
;; (with-eval-after-load  'org
;;   (require 'ox-latex)
;;   (setq org-export-latex-listings t)
;;   ;;org-mode source code setup in exporting to latex
;;   (add-to-list 'org-latex-listings '("" "listings"))
;;   (add-to-list 'org-latex-listings '("" "color"))

;;   (add-to-list 'org-latex-packages-alist
;; 	       '("" "xcolor" t))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("" "listings" t))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("" "fontspec" t))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("" "indentfirst" t))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("" "xunicode" t))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("" "geometry"))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("" "float"))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("" "longtable"))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("" "tikz"))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("" "fancyhdr"))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("" "textcomp"))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("" "amsmath"))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("" "tabularx" t))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("" "booktabs" t))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("" "grffile" t))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("" "wrapfig" t))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("normalem" "ulem" t))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("" "amssymb" t))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("" "capt-of" t))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("figuresright" "rotating" t))
;;   (add-to-list 'org-latex-packages-alist
;; 	       '("Lenny" "fncychap" t))
;;   (add-to-list 'org-latex-classes
;; 	       '("samray-org-book"
;; 		 "\\documentclass{book}
;; \\usepackage[slantfont, boldfont]{xeCJK}
;; % chapter set
;; \\usepackage{titlesec}
;; \\usepackage{hyperref}
;; [NO-DEFAULT-PACKAGES]
;; [PACKAGES]
;; \\setCJKmainfont{WenQuanYi Micro Hei} % 设置缺省中文字体
;; \\setCJKsansfont{WenQuanYi Micro Hei}
;; \\setCJKmonofont{WenQuanYi Micro Hei Mono}
;; \\setmainfont{DejaVu Sans} % 英文衬线字体
;; \\setsansfont{DejaVu Serif} % 英文无衬线字体
;; \\setmonofont{DejaVu Sans Mono}
;; %\\setmainfont{WenQuanYi Micro Hei} % 设置缺省中文字体
;; %\\setsansfont{WenQuanYi Micro Hei}
;; %\\setmonofont{WenQuanYi Micro Hei Mono}

;; %如果没有它，会有一些 tex 特殊字符无法正常使用，比如连字符。
;; \\defaultfontfeatures{Mapping=tex-text}

;; % 中文断行
;; \\XeTeXlinebreaklocale \"zh\"
;; \\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt

;; % 代码设置
;; \\lstset{numbers=left,
;; numberstyle= \\tiny,
;; keywordstyle= \\color{ blue!70},commentstyle=\\color{red!50!green!50!blue!50},
;; frame=shadowbox,
;; breaklines=true,
;; rulesepcolor= \\color{ red!20!green!20!blue!20}
;; }
;; [EXTRA]
;; "
;;                  ("\\chapter{%s}" . "\\chapter*{%s}")
;;                  ("\\section{%s}" . "\\section*{%s}")
;;                  ("\\subsection{%s}" . "\\subsection*{%s}")
;;                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;;   (add-to-list 'org-latex-classes
;;                '("samray-org-article"
;;                  "\\documentclass{article}
;; \\usepackage[slantfont, boldfont]{xeCJK}
;; \\usepackage{titlesec}
;; \\usepackage{hyperref}
;; [NO-DEFAULT-PACKAGES]
;; [PACKAGES]
;; \\parindent 2em
;; \\setCJKmainfont{WenQuanYi Micro Hei} % 设置缺省中文字体
;; \\setCJKsansfont{WenQuanYi Micro Hei}
;; \\setCJKmonofont{WenQuanYi Micro Hei Mono}
;; \\setmainfont{DejaVu Sans} % 英文衬线字体
;; \\setsansfont{DejaVu Serif} % 英文无衬线字体
;; \\setmonofont{DejaVu Sans Mono}
;; %\\setmainfont{WenQuanYi Micro Hei} % 设置缺省中文字体
;; %\\setsansfont{WenQuanYi Micro Hei}
;; %\\setmonofont{WenQuanYi Micro Hei Mono}

;; %如果没有它，会有一些 tex 特殊字符无法正常使用，比如连字符。
;; \\defaultfontfeatures{Mapping=tex-text}

;; % 中文断行
;; \\XeTeXlinebreaklocale \"zh\"
;; \\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt

;; % 代码设置
;; \\lstset{numbers=left,
;; numberstyle= \\tiny,
;; keywordstyle= \\color{ blue!70},commentstyle=\\color{red!50!green!50!blue!50},
;; frame=shadowbox,
;; breaklines=true,
;; rulesepcolor= \\color{ red!20!green!20!blue!20}
;; }
;; [EXTRA]
;; "
;;                  ("\\section{%s}" . "\\section*{%s}")
;;                  ("\\subsection{%s}" . "\\subsection*{%s}")
;;                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;   (add-to-list 'org-latex-classes
;;                '("samray-org-beamer"
;;                  "\\documentclass{beamer}
;; \\usepackage[slantfont, boldfont]{xeCJK}
;; % beamer set
;; \\usepackage[none]{hyphenat}
;; \\usepackage[abs]{overpic}

;; [NO-DEFAULT-PACKAGES]
;; [PACKAGES]
;; \\setCJKmainfont{WenQuanYi Micro Hei} % 设置缺省中文字体
;; \\setCJKsansfont{WenQuanYi Micro Hei}
;; \\setCJKmonofont{WenQuanYi Micro Hei Mono}
;; \\setmainfont{DejaVu Sans} % 英文衬线字体
;; \\setsansfont{DejaVu Serif} % 英文无衬线字体
;; \\setmonofont{DejaVu Sans Mono}
;; %\\setmainfont{WenQuanYi Micro Hei} % 设置缺省中文字体
;; %\\setsansfont{WenQuanYi Micro Hei}
;; %\\setmonofont{WenQuanYi Micro Hei Mono}

;; %如果没有它，会有一些 tex 特殊字符无法正常使用，比如连字符。
;; \\defaultfontfeatures{Mapping=tex-text}

;; % 中文断行
;; \\XeTeXlinebreaklocale \"zh\"
;; \\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt

;; % 代码设置
;; \\lstset{numbers=left,
;; numberstyle= \\tiny,
;; keywordstyle= \\color{ blue!70},commentstyle=\\color{red!50!green!50!blue!50},
;; frame=shadowbox,
;; breaklines=true,
;; rulesepcolor= \\color{ red!20!green!20!blue!20}
;; }

;; [EXTRA]
;; "
;;                  ("\\section{%s}" . "\\section*{%s}")
;;                  ("\\subsection{%s}" . "\\subsection*{%s}")
;;                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;   (setq org-latex-pdf-process
;;         '("xelatex -interaction nonstopmode -output-directory %o %f"
;;           ;;"biber %b" "xelatex -interaction nonstopmode -output-directory %o %f"
;;           "bibtex %b"
;;           "xelatex -interaction nonstopmode -output-directory %o %f"
;;           "xelatex -interaction nonstopmode -output-directory %o %f"))
;; )
(setq org-latex-classes
      '(("article"
         "
\\documentclass[12pt,a4paper]{article}
\\usepackage[margin=2cm]{geometry}
\\usepackage{fontspec}
\\setromanfont{cwTeXMing}
\\usepackage{etoolbox}  % Quote 部份的字型設定
\\newfontfamily\\quotefont{cwTeXFangSong}
\\AtBeginEnvironment{quote}{\\quotefont\\small}
\\setmonofont[Scale=0.9]{Courier} % 等寬字型 [FIXME] Courier 中文會爛掉！
\\font\\cwSong=''cwTeXFangSong'' at 10pt
%\\font\\cwHei=''cwTeXHeiBold'' at 10p %不知為何這套字型一用就爆掉...
\\font\\cwYen=''cwTeXYen'' at 10pt
\\font\\cwKai=''cwTeXKai'' at 10pt
\\font\\cwMing=''cwTeXMing'' at 10pt
\\font\\wqyHei=''文泉驛正黑'' at 10pt
\\font\\wqyHeiMono=''文泉驛等寬正黑'' at 10pt
\\font\\wqyHeiMicro=''文泉驛微米黑'' at 10pt
\\XeTeXlinebreaklocale ``zh''
\\XeTeXlinebreakskip = 0pt plus 1pt
\\linespread{1.36}
% [FIXME] ox-latex 的設計不良導致 hypersetup 必須在這裡插入
\\usepackage{hyperref}
\\hypersetup{
  colorlinks=true, %把紅框框移掉改用字體顏色不同來顯示連結
  linkcolor=[rgb]{0,0.37,0.53},
  citecolor=[rgb]{0,0.47,0.68},
  filecolor=[rgb]{0,0.37,0.53},
  urlcolor=[rgb]{0,0.37,0.53},
  pagebackref=true,
  linktoc=all,}
"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
        ))
;; [FIXME]
;; 原本是不要讓 org 插入 hypersetup（因為 org-mode 這部份設計成沒辦法自訂，或許可以去 report 一下？）
;; 改成自行插入，但這樣 pdfcreator 沒辦法根據 Emacs 版本插入，pdfkeyword 也會無效...幹。
(setq org-latex-with-hyperref t)
;; 把預設的 fontenc 拿掉
;; 經過測試 XeLaTeX 輸出 PDF 時有 fontenc[T1]的話中文會無法顯示。
;; hyperref 也拿掉，改從 classes 處就插入，原因見上面 org-latex-with-hyperref 的說明。
(setq org-latex-default-packages-alist
      '(("" "hyperref" nil)
        ("AUTO" "inputenc" t)
        ("" "fixltx2e" nil)
        ("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "marvosym" t)
        ("" "wasysym" t)
        ("" "multicol" t)  ; 這是我另外加的，因為常需要多欄位文件版面。
        ("" "amssymb" t)
        "\\tolerance=1000"))
;; Use XeLaTeX to export PDF in Org-mode
(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))
(setq tex-compile-commands '(("xelatex %r")))
(setq tex-command "xelatex")
(setq-default TeX-engine 'xelatex)
;; 在创建 org-mode buffer, 自动添加 latex class 解决生成中文pdf 问题
(eval-after-load 'autoinsert
  '(define-auto-insert '(org-mode . "Chinese Org skeleton")
     '("Description: "
       "#+LATEX_CLASS: samray-org-article" \n
       "#+LATEX_CLASS_OPTIONS: [oneside,A4paper,12pt]" \n)))
(auto-insert-mode)  ;;; Adds hook to find-files-hook
(setq auto-insert-query nil) ;;; If you don't want to be prompted before insertion

(provide 'init-chinese)
;;; init-chinese.el ends here
