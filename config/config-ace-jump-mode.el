;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ace-jump-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ace-jump-mode)
;; (global-set-key (kbd "M-J") 'ace-jump-mode)

;;; jump 対象を同一ウィンドウ内に限定する
(setq ace-jump-mode-scope 'window)

;;; ヒント文字の色を変更
(set-face-foreground 'ace-jump-face-foreground "#ff4646")

;;; ヒントに使う文字にアルファベットと同じ段にある記号も含める
(setq ace-jump-mode-move-keys
      (nconc ace-jump-mode-move-keys '(59 ; 59 = ?;
                                       ?' ?, ?. ?/ ?[ ?] ?:
                                       34 ; 34 = ?"
                                       ?< ?> ?? ?{ ?})))

;;; ace-jump-mode が mark を操作するのが邪魔なので mark を操作しないよう指定
;;; できるようにする
(defvar my-ace-jump-push-mark-disable t)

(defadvice ace-jump-push-mark (around disable-ace-jump-push-mark activate)
  (unless my-ace-jump-push-mark-disable
    ad-do-it))

;;; isearch の途中から ace-jump-mode を起動する
(defvar my-ace-jump-mode-invoked-window nil)
(defvar my-ace-jump-mode-invoked-point nil)

(defun ace-jump-search-filter--forward ()
  (or (not (eq (selected-window) my-ace-jump-mode-invoked-window))
      (>= (match-end 0) my-ace-jump-mode-invoked-point)))

(defun ace-jump-search-filter--backward ()
  (or (not (eq (selected-window) my-ace-jump-mode-invoked-window))
      (<= (match-beginning 0) my-ace-jump-mode-invoked-point)))

(defvar my-ace-jump-ignore-isearch-direction t)

(defun isearch-invoke-ace-jump-mode ()
  (interactive)
  (if (= (length isearch-string) 0)
      (isearch-printing-char)
    (let ((ace-jump-search-filter (and (not my-ace-jump-ignore-isearch-direction)
                                       (if isearch-forward
                                           'ace-jump-search-filter--forward
                                         'ace-jump-search-filter--backward)))
          (my-ace-jump-mode-invoked-window (selected-window))
          (my-ace-jump-mode-invoked-point (point))
          (ace-jump-mode-scope 'window))
      (isearch-exit)
      (ace-jump-do (regexp-quote isearch-string)))))

(define-key isearch-mode-map (kbd ";") 'isearch-invoke-ace-jump-mode)


;; (defun add-keys-to-ace-jump-mode (prefix c &optional mode)
;;   (define-key global-map
;;     (read-kbd-macro (concat prefix (string c)))
;;     `(lambda ()
;;        (interactive)
;;        (funcall (if (eq ',mode 'word)
;;                     #'ace-jump-word-mode
;;                   #'ace-jump-char-mode) ,c))))

;; ;;; H-{任意のキー} の入力でそのキーがある場所へジャンプする
;; (loop for c from ?0 to ?9 do (add-keys-to-ace-jump-mode "H-" c 'word))
;; (loop for c from ?a to ?z do (add-keys-to-ace-jump-mode "H-" c 'word))
;; (loop for c from ?! to ?~ do (add-keys-to-ace-jump-mode "H-" c 'word))

;; (loop for c from ?0 to ?9 do (add-keys-to-ace-jump-mode "H-M-" c))
;; (loop for c from ?a to ?z do (add-keys-to-ace-jump-mode "H-M-" c))
;; (loop for c from ?! to ?~ do (add-keys-to-ace-jump-mode "H-M-" c))

(provide 'config-ace-jump-mode)
