;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; company.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'company)

(global-company-mode)
(custom-set-variables
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 2)
 '(company-selection-wrap-around t))

(let ((map company-active-map))
  (define-key map (kbd "M-n") nil)
  (define-key map (kbd "M-p") nil)
  (define-key map (kbd "C-n") 'company-select-next)
  (define-key map (kbd "C-p") 'company-select-previous)
  (define-key map (kbd "C-h") nil))


;; TABの挙動を変更(1と2はデフォルトの動作、3の動作を加える)
;; 1. 候補が1つの場合はそれを選択する。
;; 2. 候補が複数の場合、共通する部分を補完。
;; 3. 候補が複数あり、共通する部分が無ければ、company-select-nextを実行する。
(defun my-company--insert-candiate (candidate)
  (when (> (length candidate) 0)
    (setq candidate (substring-no-properties candidate))
    ;; XXX: Return value we check here is subject to change.
    (if (eq (company-call-backend 'ignore-case) 'keep-prefix)
        (insert (company-strip-prefix candidate))
      (cond
       ((equal company-prefix candidate)
        (company-select-next))
       (t
        (delete-region (- (point) (length company-prefix)) (point))
        (insert candidate))))))

(defvar orig-company--insert-candidate
  (symbol-function 'company--insert-candidate))

(fset 'company--insert-candidate
      (symbol-function 'my-company--insert-candiate))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; quickhelp
(require 'company-quickhelp)

(company-quickhelp-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-company)
