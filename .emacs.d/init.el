;; ~/.emacs.d/site-lisp 以下全部読み込み
;(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
;  (add-to-list 'load-path default-directory)
;  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
;      (normal-top-level-add-subdirs-to-load-path)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; install packages automatically
(require 'cl)
(defvar installing-package-list
  '(
    helm
    auto-complete
    markdown-mode
    ))

(let ((not-installed (loop for x in installing-package-list
                           when (not (package-installed-p x))
                           collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))

(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits")
