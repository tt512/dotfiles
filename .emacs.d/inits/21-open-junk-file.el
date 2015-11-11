;; open-junk-file
(require 'open-junk-file)
(require 'helm-open-junk-files)

(setq open-junk-file-format "~/Copy/junk/%Y-%m%d-%H%M%S.")
(global-set-key "\C-xj" 'open-junk-file)

(setq helm-open-junk-files-dir "~/Copy/junk")
(global-set-key "\C-x\M-j" 'helm-open-junk-files)
