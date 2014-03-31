;;;
;;; TeX mode
;;;
;(setq auto-mode-alist
;      (append '(("\\.tex$" . latex-mode)) auto-mode-alist))
;(setq tex-default-mode 'latex-mode)
;(setq tex-start-commands "\\nonstopmode\\input")
;;(setq tex-run-command "ptex2pdf -e -ot '-synctex=1 -interaction=nonstopmode'")
;;(setq tex-run-command "ptex2pdf -e -u -ot '-synctex=1 -interaction=nonstopmode'")
;;(setq tex-run-command "pdftex -synctex=1 -interaction=nonstopmode")
;;(setq tex-run-command "luatex -synctex=1 -interaction=nonstopmode")
;;(setq tex-run-command "luajittex -synctex=1 -interaction=nonstopmode")
;(setq tex-run-command "xetex -synctex=1 -interaction=nonstopmode")
;;(setq latex-run-command "ptex2pdf -l -ot '-synctex=1 -interaction=nonstopmode'")
;;(setq latex-run-command "ptex2pdf -l -u -ot '-synctex=1 -interaction=nonstopmode'")
;;(setq latex-run-command "pdflatex -synctex=1 -interaction=nonstopmode")
;;(setq latex-run-command "lualatex -synctex=1 -interaction=nonstopmode")
;;(setq latex-run-command "luajitlatex -synctex=1 -interaction=nonstopmode")
;(setq latex-run-command "xelatex -synctex=1 -interaction=nonstopmode")
;;(setq tex-bibtex-command "pbibtex")
;;(setq tex-bibtex-command "upbibtex")
;;(setq tex-bibtex-command "bibtex")
;(setq tex-bibtex-command "bibtexu")
;(require 'tex-mode)
;(defun tex-view ()
;  (interactive)
;  (tex-send-command "evince" (tex-append tex-print-file ".pdf") " &"))
;(defun tex-print (&optional alt)
;  (interactive "P")
;  (if (tex-shell-running)
;      (tex-kill-job)
;    (tex-start-shell))
;  (tex-send-command "acroread" (tex-append tex-print-file ".pdf") " &"))
;(setq tex-compile-commands
;      '(("ptex2pdf -l -ot '-synctex=1 -interaction=nonstopmode' %f" "%f" "%r.pdf")
;        ("platex -synctex=1 -interaction=nonstopmode %f && dvips -Ppdf -z -f %r.dvi | convbkmk -g > %r.ps && ps2pdf %r.ps" "%f" "%r.pdf")
;        ("ptex2pdf -l -u -ot '-synctex=1 -interaction=nonstopmode' %f" "%f" "%r.pdf")
;        ("uplatex -synctex=1 -interaction=nonstopmode %f && dvips -Ppdf -z -f %r.dvi | convbkmk -u > %r.ps && ps2pdf %r.ps" "%f" "%r.pdf")
;        ("pdflatex -synctex=1 -interaction=nonstopmode %f" "%f" "%r.pdf")
;        ("lualatex -synctex=1 -interaction=nonstopmode %f" "%f" "%r.pdf")
;        ("luajitlatex -synctex=1 -interaction=nonstopmode %f" "%f" "%r.pdf")
;       ("xelatex -synctex=1 -interaction=nonstopmode %f" "%f" "%r.pdf")
;        ("latexmk %f" "%f" "%r.pdf")
;        ("latexmk -e '$latex=q/platex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/pbibtex %%O %%B/' -e '$makeindex=q/mendex %%O -o %%D %%S/' -e '$dvipdf=q/dvipdfmx %%O -o %%D %%S/' -norc -gg -pdfdvi %f" "%f" "%r.pdf")
;        ("latexmk -e '$latex=q/platex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/pbibtex %%O %%B/' -e '$makeindex=q/mendex %%O -o %%D %%S/' -e '$dvips=q/dvips %%O -z -f %%S | convbkmk -g > %%D/' -e '$ps2pdf=q/ps2pdf %%O %%S %%D/' -norc -gg -pdfps %f" "%f" "%r.pdf")
;        ("latexmk -e '$latex=q/uplatex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$makeindex=q/mendex %%O -o %%D %%S/' -e '$dvipdf=q/dvipdfmx %%O -o %%D %%S/' -norc -gg -pdfdvi %f" "%f" "%r.pdf")
;        ("latexmk -e '$latex=q/uplatex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$makeindex=q/mendex %%O -o %%D %%S/' -e '$dvips=q/dvips %%O -z -f %%S | convbkmk -u > %%D/' -e '$ps2pdf=q/ps2pdf %%O %%S %%D/' -norc -gg -pdfps %f" "%f" "%r.pdf")
;        ("latexmk -e '$pdflatex=q/pdflatex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/bibtex %%O %%B/' -e '$makeindex=q/makeindex %%O -o %%D %%S/' -norc -gg -pdf %f" "%f" "%r.pdf")
;        ("latexmk -e '$pdflatex=q/lualatex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/bibtexu %%O %%B/' -e '$makeindex=q/texindy %%O -o %%D %%S/' -norc -gg -lualatex %f" "%f" "%r.pdf")
;        ("latexmk -e '$pdflatex=q/luajitlatex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/bibtexu %%O %%B/' -e '$makeindex=q/texindy %%O -o %%D %%S/' -norc -gg -lualatex %f" "%f" "%r.pdf")
;        ("latexmk -e '$pdflatex=q/xelatex %%O -synctex=1 -interaction=nonstopmode %%S/' -e '$bibtex=q/bibtexu %%O %%B/' -e '$makeindex=q/texindy %%O -o %%D %%S/' -norc -gg -xelatex %f" "%f" "%r.pdf")
;        ("pbibtex %r" "%r.aux" "%r.bbl")
;        ("upbibtex %r" "%r.aux" "%r.bbl")
;        ("bibtex %r" "%r.aux" "%r.bbl")
;       ("bibtexu %r" "%r.aux" "%r.bbl")
;       ("biber %r" "%r.bcf" "%r.bbl")
;       ("mendex %r" "%r.idx" "%r.ind")
;       ("makeindex %r" "%r.idx" "%r.ind")
;       ("texindy %r" "%r.idx" "%r.ind")
;       ((concat "\\doc-view" " \"" (car (split-string (format "%s" (tex-main-file)) "\\.")) ".pdf\"") "%r.pdf")
;       ("evince %r.pdf &" "%r.pdf")
;        ("okular --unique %r.pdf &" "%r.pdf")
;        ("zathura -s -x \"emacsclient --no-wait +%%{line} %%{input}\" %r.pdf &" "%r.pdf")
;        ("qpdfview --unique %r.pdf &" "%r.pdf")
;        ("pdfviewer %r.pdf &" "%r.pdf")
;        ("texworks %r.pdf &" "%r.pdf")
;        ("mupdf %r.pdf &" "%r.pdf")
;        ("firefox -new-window %r.pdf &" "%r.pdf")
;        ("chromium --new-window %r.pdf &" "%r.pdf")
;        ("acroread %r.pdf &" "%r.pdf")
;        ("pdfopen -viewer ar9-tab %r.pdf &" "%r.pdf")))
; 
;(defun evince-forward-search ()
;  (interactive)
;  (let* ((ctf (buffer-name))
;         (mtf (tex-main-file))
;         (pf (concat (car (split-string mtf "\\.")) ".pdf"))
;         (ln (format "%d" (line-number-at-pos)))
;         (cmd "fwdevince")
;         (args (concat "\"" pf "\" " ln " \"" ctf "\"")))
;    (message (concat cmd " " args))
;    (process-kill-without-query
;     (start-process-shell-command "fwdevince" nil cmd args))))
; 
;(add-hook 'latex-mode-hook
;          '(lambda ()
;             (define-key latex-mode-map (kbd "C-c e") 'evince-forward-search)))
; 
;(require 'dbus)
; 
;(defun un-urlify (fname-or-url)
;  "A trivial function that replaces a prefix of file:/// with just /."
;  (if (string= (substring fname-or-url 0 8) "file:///")
;      (substring fname-or-url 7)
;    fname-or-url))
; 
;(defun evince-inverse-search (file linecol &rest ignored)
;  (let* ((fname (un-urlify file))
;         (buf (find-file fname))
;         (line (car linecol))
;         (col (cadr linecol)))
;    (if (null buf)
;        (message "[Synctex]: %s is not opened..." fname)
;      (switch-to-buffer buf)
;      (goto-line (car linecol))
;      (unless (= col -1)
;        (move-to-column col)))))
; 
;(dbus-register-signal
; :session nil "/org/gnome/evince/Window/0"
; "org.gnome.evince.Window" "SyncSource"
; 'evince-inverse-search)
; 
;(defun okular-forward-search ()
;  (interactive)
;  (let* ((ctf (buffer-file-name))
;         (mtf (tex-main-file))
;         (pf (concat (car (split-string mtf "\\.")) ".pdf"))
;         (ln (format "%d" (line-number-at-pos)))
;         (cmd "okular")
;         (args (concat "--unique \"file:" pf "#src:" ln " " ctf "\"")))
;    (message (concat cmd " " args))
;    (process-kill-without-query
;     (start-process-shell-command "okular" nil cmd args))))
; 
;(add-hook 'latex-mode-hook
;          '(lambda ()
;             (define-key latex-mode-map (kbd "C-c o") 'okular-forward-search)))
; 
;(defun qpdfview-forward-search ()
;  (interactive)
;  (let* ((ctf (buffer-name))
;         (mtf (tex-main-file))
;         (pf (concat (car (split-string mtf "\\.")) ".pdf"))
;         (ln (format "%d" (line-number-at-pos)))
;         (cmd "qpdfview")
;         (args (concat "--unique \"" pf "#src:" ctf ":" ln ":0\"")))
;    (message (concat cmd " " args))
;    (process-kill-without-query
;     (start-process-shell-command "qpdfview" nil cmd args))))
; 
;(add-hook 'latex-mode-hook
;          '(lambda ()
;             (define-key latex-mode-map (kbd "C-c q") 'qpdfview-forward-search)))
; 
;(defun pdfviewer-forward-search ()
;  (interactive)
;  (let* ((ctf (buffer-name))
;         (mtf (tex-main-file))
;         (pf (concat (car (split-string mtf "\\.")) ".pdf"))
;         (ln (format "%d" (line-number-at-pos)))
;         (cmd "pdfviewer")
;         (args (concat "\"file:" pf "#src:" ln " " ctf "\"")))
;    (message (concat cmd " " args))
;    (process-kill-without-query
;     (start-process-shell-command "pdfviewer" nil cmd args))))
; 
;(add-hook 'latex-mode-hook
;          '(lambda ()
;             (define-key latex-mode-map (kbd "C-c p") 'pdfviewer-forward-search)))
; 
;;;
;;; RefTeX with TeX mode
;;;
;(add-hook 'latex-mode-hook 'turn-on-reftex)
