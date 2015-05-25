;; クリップボードを使う
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(set-clipboard-coding-system 'utf-8)

(cond ((eq system-type 'gnu/linux)
       (setq x-select-enable-clipboard t)
       (global-set-key "\C-y" 'x-clipboard-yank))
 
      ((eq system-type 'darwin)
       (setq interprogram-cut-function 'paste-to-osx)
       (setq interprogram-paste-function 'copy-from-osx)))
