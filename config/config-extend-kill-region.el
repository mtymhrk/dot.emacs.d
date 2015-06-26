;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; リージョンを選択していないときの C-w、 M-w の動作を変更する設定
;;;   http://dev.ariel-networks.com/Members/matsuyama/tokyo-emacs-02
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 行をコピーするコマンド
(defun copy-line (&optional arg)
  (interactive "p")
  (copy-region-as-kill
   (line-beginning-position)
   (line-beginning-position (1+ (or arg 1))))
  (message "Line copied"))

(defun copy-backward-word ()
  (save-excursion
    (copy-region-as-kill
     (point)
     (progn (backward-word) (point)))
    (message "word copied")))

;;; リージョンを選択していないときにワードを kill し、選択している場合はリー
;;; ジョンを kill するコマンド
(defun kill-region-or-backward-kill-word (beg end &optional region)
  (interactive (list (point) (mark) 'region))
  (if (region-active-p)
      (kill-region beg end region)
    (backward-kill-word 1)))

(global-set-key (kbd "C-w") 'kill-region-or-backward-kill-word)

;;; リージョンを選択していないときにワードを kill-ring にコピーし、選択してい
;;; る場合はリージョンを kill-ring へコピーするコマンド
(defun kill-ring-save-or-copy-backward-word (beg end &optional region)
  (interactive (list (mark) (point)
                     (prefix-numeric-value current-prefix-arg)))
  (if (region-active-p)
      (kill-ring-save beg end region)
    (copy-backward-word)))

(global-set-key (kbd "M-w") 'kill-ring-save-or-copy-backward-word)

(provide 'config-extend-kill-region)
