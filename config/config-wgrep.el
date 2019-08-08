;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  wgrep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package wgrep
  :custom
  ;; wgrep-mode 有効化キーの変更
  (wgrep-enable-key "e")
  ;; 編集終了後にバッファーを自動的に保存しない
  (wgrep-auto-save-buffer nil)
  ;; readonly ファイルは変更を反映しない
  (wgrep-change-readonly-file nil)
  :bind
  (:map wgrep-mode-map
        ;; C-c C-c で変更を反映
        ("C-c C-c" . wgrep-finish-edit)
        ;; wgrep-finish-edit をバインドしているキーバインド
        ;; を解除(上記キーバインドをミニバッファに表示させるため)。
        ("C-c C-e" . nil)
        ("C-x C-s" . nil))
  :config
  (with-eval-after-load 'ag
    (require 'wgrep-ag)))

;;; C-c C-e: Apply the changes to file buffers.
;;; C-c C-u: All changes are unmarked and ignored.
;;; C-c C-d: Mark as delete to current line (including newline).
;;; C-c C-r: Remove the changes in the region (these changes are not applied to the files. Of course, the remaining changes can still be applied to the files.)
;;; C-c C-p: Toggle read-only area.
;;; C-c C-k: Discard all changes and exit.
;;; C-x C-q: Exit wgrep mode.


