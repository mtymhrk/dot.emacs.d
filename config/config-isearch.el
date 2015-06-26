;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; isearch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;; C-j で検索一致文字列の先頭へ移動する
;; (defun isearch-exit-and-move-to-match-beginning ()
;;   (interactive)
;;   (let ((forward isearch-forward)
;;         (other-end isearch-other-end))
;;     (isearch-exit)
;;     (when forward (goto-char other-end))))

;; (define-key isearch-mode-map (kbd "C-j") 'isearch-exit-and-move-to-match-beginning)

;;; isearc-forward でのカーソルの移動先を一致文字列の先頭にする
(defvar my-isearch-start nil)
(defvar my-isearch-end nil)

(defun my-isearch-set-point ()
  (setq my-isearch-start nil
        my-isearch-end nil)
  (when isearch-other-end
    (if isearch-forward
        (setq my-isearch-start isearch-other-end
              my-isearch-end (point))
      (setq my-isearch-start (point)
            my-isearch-end isearch-other-end))))

(defun isearch-update-post-hook--pin ()
  (my-isearch-set-point)
  (when (and isearch-other-end isearch-forward)
    (goto-char isearch-other-end)))

(add-hook 'isearch-update-post-hook 'isearch-update-post-hook--pin)

;;; 上記のカーソル位置の変更の影響が isearch-yank-XXX に出るのうまく動くよう
;;; advice を設定する
(defmacro move-to-isearch-end-befor-yank (func)
  `(defadvice ,func (before move-to-isearch-end-befor-yank activate)
     (when (and my-isearch-end isearch-forward)
       (goto-char my-isearch-end))))

(move-to-isearch-end-befor-yank isearch-yank-word-or-char)
(move-to-isearch-end-befor-yank isearch-yank-char)
(move-to-isearch-end-befor-yank isearch-yank-word)

(eval-after-load 'migemo
  '(progn
    (move-to-isearch-end-befor-yank migemo-isearch-yank-char)
    (move-to-isearch-end-befor-yank migemo-isearch-yank-word)))

;;; C-j で一致文字列の終端にカーソルを移動して isearch を終了する
(defun isearch-exit-and-move-to-isearch-end ()
  (interactive)
  (isearch-exit)
  (goto-char my-isearch-end))

(define-key isearch-mode-map (kbd "C-j") 'isearch-exit-and-move-to-isearch-end)

(provide 'config-isearch)
