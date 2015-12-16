;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'flymake)

;;; flymake の行表示の色を設定
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

;;; gcc の日本語警告メッセージを警告と判別できるようにする
(setq flymake-warning-re "^\\([wW]arning\\|警告\\)")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake のエラーと警告を表示させるコマンドの定義
;;;   popup.el を require できるなら popup-tip で表示し、できないなら
;;;   minibuffer に表示する

;;; 2013/03/25: この機能は display-line-info へ移行

;;; popup.el を require できるかで表示用関数を差し替える
(defvar my-flymake-display-error-function nil)
(if (require 'popup nil t)
    (setq my-flymake-display-error-function
          (lambda (msglst) (popup-tip (mapconcat #'identity msglst "\n"))))
  (setq my-flymake-display-error-function
        (lambda (msglst) (message "%s" (mapconcat #'identity msglst "\n")))))

;;; エラーと警告メッセージを表示させるコマンド
;;;   ref: http://d.hatena.ne.jp/khiker/20100203/popup_flymake
;;;   ref: http://d.hatena.ne.jp/khiker/20070720/emacs_flymake
(defun my-flymake-display-error ()
  "Displays the error/warning for the current line in the popup-tip"
  (interactive)
  (let* ((line-no            (flymake-current-line-no))
         (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info
                                                           line-no)))
         (count              (length line-err-info-list)))
    (when (and line-err-info-list (> count 0))
      (funcall
       my-flymake-display-error-function
       (mapcar (lambda (line-err-info)
                 (let ((file (flymake-ler-file line-err-info))
                       (full-file (flymake-ler-full-file line-err-info))
                       (text (flymake-ler-text line-err-info))
                       (line (flymake-ler-line line-err-info)))
                   (format "[%s] %s" line text)))
               line-err-info-list)))))
;;; 2013/03/25:
;;;   my-flymake-display-error の機能を display-line-info に移行させたので、
;;;   この advice をコメントアウト
;; ;;; my-flymake-display-err コマンドの実行で popup-fill-string が呼出される
;; ;;; 場合、その引数の width を nil に設定する advice。
;; ;;; width が nil でない場合、fill-column によって改行が消されてしまうため、
;; ;;; それを回避する advice。
;; (defadvice popup-fill-string (before set-width-of-popup-fill-string-to-nil)
;;   (when (and (eq this-command 'my-flymake-display-error)
;;              (> (length (ad-get-args 0)) 1))
;;     (ad-set-arg 1 nil)))

;; (ad-activate 'popup-fill-string)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C ヘッダファイルにエラーがあった場合にエラー行がうまくパースできず、
;;; flymake がエラーになる問題への対処
(push '("^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 4)
      flymake-err-line-patterns)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm for flymake

(eval-after-load 'config-helm
  '(progn
     (require 'helm-flymake-or-flycheck)

     ;; (set-face-background 'helm-flymake-errline "red4")
     ;; (set-face-background 'helm-flymake-warnline "midnight blue")

     ;;; 元の helm-flymake のソースの recenter 属性の指定に不備があり、ジャン
     ;;; プしたエラー行がウィンドウの一番上に表示されるようになっていたので、
     ;;; ソースを修正
     (let ((attr (assq 'recenter helm-source-flymake-warning)))
       (when attr (setcdr attr t)))

     (let ((attr (assq 'recenter helm-source-flymake-error)))
       (when attr (setcdr attr t)))

     (define-key my:helm-command-keymap (kbd "`") 'helm-flymake-or-flycheck)
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-flymake)

;;; 参考
;;; http://www.02.246.ne.jp/~torutk/cxx/emacs/flymake.html
;;; http://d.hatena.ne.jp/khiker/20070630/emacs_ruby_flymake
;;; http://d.hatena.ne.jp/khiker/20070720/emacs_flymake
