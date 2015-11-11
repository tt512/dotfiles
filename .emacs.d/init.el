(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; el-get lazy install
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get-bundle init-loader)
(el-get-bundle helm)
(el-get-bundle auto-complete)
(el-get-bundle ddskk)
(el-get-bundle open-junk-file)
(el-get-bundle f)
(el-get-bundle masaaki1001/helm-open-junk-files)
(el-get-bundle adoc-mode)

(require 'init-loader)
(setq init-loader-show-log-after-init 'error-only)
(init-loader-load)
