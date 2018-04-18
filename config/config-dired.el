;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package dired-x
  :config
  (use-package wdired)
  (load "sorter")

  ;; dired-find-alternate-file コマンドを有効化
  (put 'dired-find-alternate-file 'disabled nil)

  ;; C-m で新規バッファを作成せずにディレクトリ/ファイルを開く
  (bind-key "C-m" 'dired-find-alternate-file dired-mode-map)

  ;; a で新規バッファを作成してディレクトリ/ファイルを開く
  (bind-key "a" 'dired-find-file dired-mode-map)
  (bind-key "e" 'wdired-change-to-wdired-mode dired-mode-map))


