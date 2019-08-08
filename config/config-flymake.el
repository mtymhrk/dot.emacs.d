;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package flymake
  :delight
  :defer t
  :config
  ;; flymake の行表示の色を設定
  (set-face-attribute 'flymake-errline nil
                      :background nil
                      :foreground nil
                      :underline '(:color "Red1" :style wave)
                      :inherit nil)

  (set-face-attribute 'flymake-warnline nil
                      :background nil
                      :foreground nil
                      :underline '(:color "DarkOrange" :style wave)
                      :inherit nil)

  ;; gcc の日本語警告メッセージを警告と判別できるようにする
  (setq flymake-warning-re "^\\([wW]arning\\|警告\\)")

  ;; C ヘッダファイルにエラーがあった場合にエラー行がうまくパースできず、
  ;; flymake がエラーになる問題への対処
  (push '("^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 4)
        flymake-err-line-patterns))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 参考
;;; http://www.02.246.ne.jp/~torutk/cxx/emacs/flymake.html
;;; http://d.hatena.ne.jp/khiker/20070630/emacs_ruby_flymake
;;; http://d.hatena.ne.jp/khiker/20070720/emacs_flymake
