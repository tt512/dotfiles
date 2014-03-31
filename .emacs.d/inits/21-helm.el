
;; helm
(require 'helm-config)
(helm-mode 1)

(global-set-key (kbd "C-x C-r") 'helm-recentf)

;; For find-file etc.
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;; For helm-find-files etc.
;(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
