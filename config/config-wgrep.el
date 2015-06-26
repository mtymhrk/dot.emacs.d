;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  wgrep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'wgrep)

(custom-set-variables
 ;; wgrep-mode 有効化キーの変更
 '(wgrep-enable-key "e")
 ;; 編集終了後にバッファーを自動的に保存しない
 '(wgrep-auto-save-buffer nil)
 ;; readonly ファイルは変更を反映しない
 '(wgrep-change-readonly-file nil))

;;; C-c C-c で変更を反映
(define-key wgrep-mode-map (kbd "C-c C-c") 'wgrep-finish-edit)
;;; C-c ESC で変更を破棄
(define-key wgrep-mode-map (kbd "C-c ESC") 'wgrep-abort-changes)

;;; wgrep-finish-edit と wgrep-abort-changes にバインドしているキーバインド
;;; を解除(上記キーバインドをミニバッファに表示させるため)。
(define-key wgrep-mode-map (kbd "C-c C-e") nil)
(define-key wgrep-mode-map (kbd "C-x C-s") nil)
(define-key wgrep-mode-map (kbd "C-c C-k") nil)


(eval-after-load 'ag
  '(require 'wgrep-ag))

(eval-after-load 'helm-config
  '(require 'wgrep-helm))

;;; C-c C-e: Apply the changes to file buffers.
;;; C-c C-u: All changes are unmarked and ignored.
;;; C-c C-d: Mark as delete to current line (including newline).
;;; C-c C-r: Remove the changes in the region (these changes are not applied to the files. Of course, the remaining changes can still be applied to the files.)
;;; C-c C-p: Toggle read-only area.
;;; C-c C-k: Discard all changes and exit.
;;; C-x C-q: Exit wgrep mode.

(provide 'config-wgrep)
