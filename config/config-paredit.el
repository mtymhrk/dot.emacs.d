;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ParEdit
;;;  S 式編集用のマイナーモード
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ParEdit チュートリアル
;;;   http://www.daregada.sakuraweb.com/paredit_tutorial_ja.html

(require 'paredit)
  ;;; M-↑、M-↓ にバインドされているコマンドを矢印キーがなくても実行できるよ
  ;;; に別のキーにもバインドする
(define-key paredit-mode-map
  (kbd "C-<") 'paredit-splice-sexp-killing-backward)
(define-key paredit-mode-map
  (kbd "C->") 'paredit-splice-sexp-killing-forward)

  ;;; M-J が paredit に食われるので paredit のキーマップから M-J のバインドを
  ;;; 削除し、別のキーにバインドする
(define-key paredit-mode-map (kbd "M-J") nil)
(define-key paredit-mode-map (kbd "M-S") nil)
(define-key paredit-mode-map (kbd "M-c") 'paredit-join-sexps)
(define-key paredit-mode-map (kbd "M-d") 'paredit-split-sexp)


(add-hook 'lisp-mode-hook 'enable-paredit-mode)

(provide 'config-paredit)
