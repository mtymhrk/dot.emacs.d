;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; isearch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; isearc-forward でのカーソルの移動先を一致文字列の先頭にする

(defvar mod-isearch:isearch-start nil)
(defvar mod-isearch:isearch-end nil)

(defun mod-isearch:set-point ()
  (setq mod-isearch:isearch-start nil
        mod-isearch:isearch-end nil)
  (when isearch-other-end
    (if isearch-forward
        (setq mod-isearch:isearch-start isearch-other-end
              mod-isearch:isearch-end (point))
      (setq mod-isearch:isearch-start (point)
            mod-isearch:isearch-end isearch-other-end))))

(defun mod-isearch:isearch-update-post-hook--pin ()
  (mod-isearch:set-point)
  (when (and isearch-other-end isearch-forward)
    (goto-char isearch-other-end)))

(add-hook 'isearch-update-post-hook 'mod-isearch:isearch-update-post-hook--pin)

;;; 上記のカーソル位置の変更の影響が isearch-yank-XXX に出るのうまく動くよう
;;; advice を設定する
(defmacro mod-isearch:move-to-isearch-end-befor-yank (func)
  `(defadvice ,func (before mod-isearch:move-to-isearch-end-befor-yank activate)
     (when (and mod-isearch:isearch-end isearch-forward)
       (goto-char mod-isearch:isearch-end))))

(mod-isearch:move-to-isearch-end-befor-yank isearch-yank-word-or-char)
(mod-isearch:move-to-isearch-end-befor-yank isearch-yank-char)
(mod-isearch:move-to-isearch-end-befor-yank isearch-yank-word)

(eval-after-load 'migemo
  '(progn
    (mod-isearch:move-to-isearch-end-befor-yank migemo-isearch-yank-char)
    (mod-isearch:move-to-isearch-end-befor-yank migemo-isearch-yank-word)))

;;; C-j で一致文字列の終端にカーソルを移動して isearch を終了する
(defun mod-isearch:exit-and-move-to-isearch-end ()
  (interactive)
  (isearch-exit)
  (goto-char mod-isearch:isearch-end))

(define-key isearch-mode-map (kbd "C-j")
  #'mod-isearch:exit-and-move-to-isearch-end)

(provide 'mod-isearch)
