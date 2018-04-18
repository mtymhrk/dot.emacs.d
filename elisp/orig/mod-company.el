;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mod-company.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(with-eval-after-load 'company
  ;; company-complete-common の挙動を変更(1と2はデフォルトの動作、3の動作を加える)
  ;; 1. 候補が1つの場合はそれを選択する。
  ;; 2. 候補が複数の場合、共通する部分を補完。
  ;; 3. 候補が複数あり、共通する部分が無ければ、company-select-nextを実行する。
  (defun mod-company:insert-candiate (candidate)
    (when (> (length candidate) 0)
      (setq candidate (substring-no-properties candidate))
      (cond
       ((equal company-prefix candidate)
        (company-select-next))
       ;; XXX: Return value we check here is subject to change.
       ((eq (company-call-backend 'ignore-case) 'keep-prefix)
        (insert (company-strip-prefix candidate)))
       (t
        (delete-region (- (point) (length company-prefix)) (point))
        (insert candidate)))))

  (defvar mod-company:orig-company--insert-candidate
    (symbol-function 'company--insert-candidate))

  (fset 'company--insert-candidate
        (symbol-function 'mod-company:insert-candiate)))

(provide 'mod-company)
