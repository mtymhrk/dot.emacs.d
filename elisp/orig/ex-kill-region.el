;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; リージョンを選択していないときの C-w、 M-w の動作を変更する設定
;;;   http://dev.ariel-networks.com/Members/matsuyama/tokyo-emacs-02
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 行をコピーするコマンド
(defun ex-kill-region:copy-line (&optional arg)
  (interactive "p")
  (copy-region-as-kill
   (line-beginning-position)
   (line-beginning-position (1+ (or arg 1))))
  (message "Line copied"))

(defun ex-kill-region:copy-backward-word ()
  (save-excursion
    (copy-region-as-kill
     (point)
     (progn (backward-word) (point)))
    (message "word copied")))

;;; リージョンを選択している場合はリージョンを kill し、選択していない場合は
;;; 変数に設定された関数を実行するコマンド
(defvar ex-kill-region:kill-region-or-do-something nil)
(defun ex-kill-region:kill-region-or-do-something (beg end &optional region)
  (interactive (list (point) (mark) 'region))
  (cond ((region-active-p)
         (kill-region beg end region))
        (ex-kill-region:kill-region-or-do-something
         (funcall ex-kill-region:kill-region-or-do-something))
        (t                              ; nothing to do
         )))

;;; リージョン選択されていない場合は word を kill する
(setq ex-kill-region:kill-region-or-do-something
      (lambda () (backward-kill-word 1)))
(global-set-key (kbd "C-w") 'ex-kill-region:kill-region-or-do-something)


;;; リージョンを選択している場合はリージョンを kill-ring へコピーし、選択し
;;; ていない場合は変数に設定された関数を実行するコマンド
(defvar ex-kill-region:kill-ring-save-or-do-something nil)
(defun ex-kill-region:kill-ring-save-or-do-something (beg end &optional region)
  (interactive (list (mark) (point)
                     (prefix-numeric-value current-prefix-arg)))
  (cond ((region-active-p)
         (kill-ring-save beg end region))
        (ex-kill-region:kill-ring-save-or-do-something
         (funcall ex-kill-region:kill-ring-save-or-do-something))
        (t                              ; nothing to do
         )))

;;; リージョン選択されていない場合は word を kill-ring へコピーする
(setq ex-kill-region:kill-ring-save-or-do-something
      'ex-kill-region:copy-backward-word)
;; (global-set-key (kbd "M-w") 'ex-kill-region:kill-ring-save-or-do-something)

(provide 'ex-kill-region)
