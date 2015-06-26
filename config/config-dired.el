;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired-x)
(require 'wdired)
(load "sorter")

;;; dired-find-alternate-file コマンドを有効化
(put 'dired-find-alternate-file 'disabled nil)

;;; C-m で新規バッファを作成せずにディレクトリ/ファイルを開く
(define-key dired-mode-map (kbd "C-m") 'dired-find-alternate-file)
;;; a で新規バッファを作成してディレクトリ/ファイルを開く
(define-key dired-mode-map (kbd "a") 'dired-find-file)
(define-key dired-mode-map (kbd "e") 'wdired-change-to-wdired-mode)

(provide 'config-dired)
