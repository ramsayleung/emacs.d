;;; package --- Summary -*- lexical-binding: t -*-
;;; code:
;;; Commentary:
(use-package youdao-dictionary
  :ensure t
  :commands (youdao-dictionary-search-at-point+ youdao-dictionary-search-from-input)
  :init
  (progn
    (defun ramsay/youdao-dictionary-buffer-quit ()
      "Quit youdao-dictionary Buffer and delete window"
      (interactive)
      (kill-this-buffer)
      (delete-window)))
  :config (progn
	    (setq url-automatic-caching t)
	    ))

;;; org-mode 导出中文的问题解决
;; org-mode export to latex
(require 'ox-latex)
(setq org-export-latex-listings t)

(add-to-list 'org-latex-packages-alist
	     '("" "xcolor" t))
(add-to-list 'org-latex-packages-alist
	     '("" "listings" t))
(add-to-list 'org-latex-packages-alist
	     '("" "fontspec" t))
(add-to-list 'org-latex-packages-alist
	     '("" "indentfirst" t))
(add-to-list 'org-latex-packages-alist
	     '("" "xunicode" t))
(add-to-list 'org-latex-packages-alist
	     '("" "geometry"))
(add-to-list 'org-latex-packages-alist
	     '("" "float"))
(add-to-list 'org-latex-packages-alist
	     '("" "longtable"))
(add-to-list 'org-latex-packages-alist
	     '("" "tikz"))
(add-to-list 'org-latex-packages-alist
	     '("" "fancyhdr"))
(add-to-list 'org-latex-packages-alist
	     '("" "textcomp"))
(add-to-list 'org-latex-packages-alist
	     '("" "amsmath"))
(add-to-list 'org-latex-packages-alist
	     '("" "tabularx" t))
(add-to-list 'org-latex-packages-alist
	     '("" "booktabs" t))
(add-to-list 'org-latex-packages-alist
	     '("" "grffile" t))
(add-to-list 'org-latex-packages-alist
	     '("" "wrapfig" t))
(add-to-list 'org-latex-packages-alist
	     '("normalem" "ulem" t))
(add-to-list 'org-latex-packages-alist
	     '("" "amssymb" t))
(add-to-list 'org-latex-packages-alist
	     '("" "capt-of" t))
(add-to-list 'org-latex-packages-alist
	     '("figuresright" "rotating" t))
(add-to-list 'org-latex-packages-alist
	     '("Lenny" "fncychap" t))

(add-to-list 'org-latex-classes
	     '("ramsay-org-book"
	       "\\documentclass{book}
\\usepackage[slantfont, boldfont]{xeCJK}
% chapter set
\\usepackage{titlesec}
\\usepackage{minted}
\\usepackage{hyperref}

[NO-DEFAULT-PACKAGES]
[PACKAGES]



\\setCJKmainfont{WenQuanYi Micro Hei} % 设置缺省中文字体
\\setCJKsansfont{WenQuanYi Micro Hei}
\\setCJKmonofont{WenQuanYi Micro Hei Mono}

\\setmainfont{DejaVu Sans} % 英文衬线字体
\\setsansfont{DejaVu Serif} % 英文无衬线字体
\\setmonofont{DejaVu Sans Mono}
%\\setmainfont{WenQuanYi Micro Hei} % 设置缺省中文字体
%\\setsansfont{WenQuanYi Micro Hei}
%\\setmonofont{WenQuanYi Micro Hei Mono}

%如果没有它，会有一些 tex 特殊字符无法正常使用，比如连字符。
\\defaultfontfeatures{Mapping=tex-text}

% [FIXME] ox-latex 的設計不良導致 hypersetup 必須在這裡插入
\\usepackage{hyperref}
\\hypersetup{
colorlinks=true, %把紅框框移掉改用字體顏色不同來顯示連結
linkcolor=[rgb]{0,0.37,0.53},
citecolor=[rgb]{0,0.47,0.68},
filecolor=[rgb]{0,0.37,0.53},
urlcolor=[rgb]{0,0.37,0.53},
linktoc=all,}
% 中文断行
\\XeTeXlinebreaklocale \"zh\"
\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt

% 代码设置
\\lstset{numbers=left,
numberstyle= \\tiny,
keywordstyle= \\color{ blue!70},commentstyle=\\color{red!50!green!50!blue!50},
frame=shadowbox,
breaklines=true,
rulesepcolor= \\color{ red!20!green!20!blue!20}
}

[EXTRA]
"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; \\usepackage[slantfont, boldfont]{xeCJK}
(add-to-list 'org-latex-classes
             '("ramsay-org-article"
               "\\documentclass{article}
\\usepackage[CJKspace]{xeCJK}
\\usepackage{titlesec}
\\usepackage{minted}


[NO-DEFAULT-PACKAGES]
[PACKAGES]

\\setCJKmainfont{WenQuanYi Micro Hei} % 设置缺省中文字体
\\setCJKsansfont{WenQuanYi Micro Hei}
\\setCJKmonofont{WenQuanYi Micro Hei Mono}

\\setmainfont{DejaVu Sans} % 英文衬线字体
\\setsansfont{DejaVu Serif} % 英文无衬线字体
\\setmonofont{DejaVu Sans Mono}
%\\setmainfont{WenQuanYi Micro Hei} % 设置缺省中文字体
%\\setsansfont{WenQuanYi Micro Hei}
%\\setmonofont{WenQuanYi Micro Hei Mono}

%如果没有它，会有一些 tex 特殊字符无法正常使用，比如连字符。
\\defaultfontfeatures{Mapping=tex-text}

% [FIXME] ox-latex 的設計不良導致 hypersetup 必須在這裡插入
\\usepackage{hyperref}
\\hypersetup{
colorlinks=true, %把紅框框移掉改用字體顏色不同來顯示連結
linkcolor=[rgb]{0,0.37,0.53},
citecolor=[rgb]{0,0.47,0.68},
filecolor=[rgb]{0,0.37,0.53},
urlcolor=[rgb]{0,0.37,0.53},
linktoc=all,}
% 中文断行
\\XeTeXlinebreaklocale \"zh\"
\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt

% 代码设置
\\lstset{numbers=left,
numberstyle= \\tiny,
keywordstyle= \\color{ blue!70},commentstyle=\\color{red!50!green!50!blue!50},
frame=shadowbox,
breaklines=true,
rulesepcolor= \\color{ red!20!green!20!blue!20}
}

[EXTRA]
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("ramsay-org-beamer"
               "\\documentclass{beamer}
\\usepackage[slantfont, boldfont]{xeCJK}
% beamer set
\\usepackage{minted}
\\usepackage[none]{hyphenat}
\\usepackage[abs]{overpic}

[NO-DEFAULT-PACKAGES]
[PACKAGES]

\\setCJKmainfont{WenQuanYi Micro Hei} % 设置缺省中文字体
\\setCJKsansfont{WenQuanYi Micro Hei}
\\setCJKmonofont{WenQuanYi Micro Hei Mono}

\\setmainfont{DejaVu Sans} % 英文衬线字体
\\setsansfont{DejaVu Serif} % 英文无衬线字体
\\setmonofont{DejaVu Sans Mono}
%\\setmainfont{WenQuanYi Micro Hei} % 设置缺省中文字体
%\\setsansfont{WenQuanYi Micro Hei}
%\\setmonofont{WenQuanYi Micro Hei Mono}

%如果没有它，会有一些 tex 特殊字符无法正常使用，比如连字符。
\\defaultfontfeatures{Mapping=tex-text}

% 中文断行
\\XeTeXlinebreaklocale \"zh\"
\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt

% [FIXME] ox-latex 的設計不良導致 hypersetup 必須在這裡插入
\\usepackage{hyperref}
\\hypersetup{
colorlinks=true, %把紅框框移掉改用字體顏色不同來顯示連結
linkcolor=[rgb]{0,0.37,0.53},
citecolor=[rgb]{0,0.47,0.68},
filecolor=[rgb]{0,0.37,0.53},
urlcolor=[rgb]{0,0.37,0.53},
linktoc=all,}
% 代码设置
\\lstset{numbers=left,
numberstyle= \\tiny,
keywordstyle= \\color{ blue!70},commentstyle=\\color{red!50!green!50!blue!50},
frame=shadowbox,
breaklines=true,
rulesepcolor= \\color{ red!20!green!20!blue!20}
}

[EXTRA]
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;; Adds hook to find-files-hook
(run-with-idle-timer ramsay-idle-time nil 'auto-insert-mode)
(setq auto-insert-query nil) ;;; If you don't want to be prompted before insertion

(message "loading init-chinese")
(provide 'init-chinese)
;;; init-chinese.el ends here
