;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; open-junk-file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package open-junk-file
  :config
  (defvar my-open-junk-file-base-dir "~/memo/junk/")
  ;; junk ファイル名フーマット
  (setq open-junk-file-format (concat my-open-junk-file-base-dir
                                      "%Y.%m.%d-%H.%M.%S."))
  :bind
  (:map keymap-ctrl-meta-space
        ("C-j" . open-junk-file)))


