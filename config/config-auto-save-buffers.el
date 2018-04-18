;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-save-buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package auto-save-buffers-enhanced
  :config
  ;; buffer-file-name が空文字列か、拡張子が .gpg のファイルの場合、
  ;; auto-save-buffers を無効にする。
  ;; EasyPG と auto-save-buffers の相性がよくないため、.gpg ファイル
  ;; を無効しにている。
  (setq auto-save-buffers-enhanced-exclude-regexp "^\\(.*\\.gpg\\)?$")

  ;; auto-save-buffers の有効/無効を C-x a s で切り替える
  (global-set-key (kbd "C-x a s") 'auto-save-buffers-enhanced-toggle-activity)

  (auto-save-buffers-enhanced t))

(provide 'config-auto-save-buffers)
