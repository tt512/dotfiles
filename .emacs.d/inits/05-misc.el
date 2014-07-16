;; スタートアップ非表示
(setq inhibit-startup-screen t)
 
;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)
;; タブ幅
(custom-set-variables '(tab-width 4))
;; yes or no を y or n に
(fset 'yes-or-no-p 'y-or-n-p)

;; クリップボードを使う
(defun copy-from-osx ()
 (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
 (let ((process-connection-type nil))
     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
       (process-send-string proc text)
       (process-send-eof proc))))

(cond (eq system-type 'linux)
      ((setq x-select-enable-clipboard t)
       (global-set-key "\C-y" 'x-clipboard-yank))

      (eq system-type 'darwin)
      ((setq interprogram-cut-function 'paste-to-osx)
       (setq interprogram-paste-function 'copy-from-osx)))
 
;; language and encoding
;(prefer-coding-system 'utf-8)
;(set-language-environment 'utf-8)
;(set-default-coding-systems 'utf-8)
 
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
