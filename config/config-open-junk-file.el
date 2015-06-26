;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; open-junk-file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'open-junk-file)
(defvar my-open-junk-file-base-dir "~/memo/junk/")

;;; junk ファイル名フーマット
(setq open-junk-file-format (concat my-open-junk-file-base-dir
                                    "%Y.%m.%d-%H.%M.%S."))

;;; キーバインド
(global-set-key (kbd "M-O j") 'open-junk-file)

(provide 'config-open-junk-file)
