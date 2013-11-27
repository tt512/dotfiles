(require 'simplenote)
(simplenote-setup)

;; Emacsでsimplenote-modeを使ってみる
;; http://blog.on-net.jp/tf/2011/01/emacssimplenote-mode.html
(defun simplenote-sync-after-save ()
  "If there is the buffer on simplenote-directory, sync the buffer to simplenote."
  (interactive)
  (when (string-match simplenote-directory default-directory)
    (simplenote-sync-notes)
    ;; only when create new note.
    (let (simplenote-new-note-dir)
      (setq simplenote-new-note-dir (concat (file-name-as-directory simplenote-directory) "new"))
      (when (string-match simplenote-new-note-dir default-directory)
        (kill-buffer (get-buffer (current-buffer)))
        (simplenote-browse)))
    ))

(add-hook 'after-save-hook 'simplenote-sync-after-save)
