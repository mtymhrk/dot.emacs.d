;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-save-buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'auto-save-buffers)
(run-with-idle-timer 0.5 t 'auto-save-buffers)

  ;;; buffer-file-name が空文字列か、拡張子が .gpg のファイルの場合、
  ;;; auto-save-buffers を無効にする。
  ;;; EasyPG と auto-save-buffers の相性がよくないため、.gpg ファイル
  ;;; を無効しにている。
(setq auto-save-buffers-exclude-regexp "^\\(.*\\.gpg\\)?$")


(provide 'config-auto-save-buffers)
