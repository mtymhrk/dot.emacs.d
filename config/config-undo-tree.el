;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; undo-tree.el
;;;   http://www.emacswiki.org/emacs/UndoTree
;;;   http://www.dr-qubit.org/undo-tree/undo-tree.el
;;;   http://d.hatena.ne.jp/khiker/20100123/undo_tree
;;;   http://d.hatena.ne.jp/kitokitoki/20100211/p1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Undo 情報を tree 状に管理する

;;; オリジナル
;;;   (install-elisp "http://www.dr-qubit.org/undo-tree/undo-tree.el")
;;;
;;; 2011/01/23 記 下の改造版は現在では使用していない
;;; ;;; id:kitokitok 改造版 (こっちをインストール)
;;; ;;; (auto-install-from-gist "301447")
;;; ;;;   オリジナルからの変更点
;;; ;;;     1. q で *undo-tree* があったウィンドウも削除し，呼び出しもとのウィンドウ・バッファ
;;; ;;;        にポイントを移動
;;; ;;;     2. 上下分割から左右分割に変更

(eval-when-compile (require 'use-package))

(use-package undo-tree
  :delight
  :commands undo-tree-mode global-undo-tree-mode
  :config
  (use-package mod-popwin
    :config
    (mod-popwin:add-display-config
     `(,undo-tree-visualizer-buffer-name :height 0.45 :position bottom :stick t)))
  )

(global-undo-tree-mode)



