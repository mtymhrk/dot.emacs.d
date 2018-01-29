;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; smart-tab.el
;;;   https://github.com/genehack/smart-tab/raw/master/smart-tab.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'smart-tab)

;; シンボル直後にカーソルがある場合の tab の動作
;; company-mode が有効になっている場合は company の補完を起動し、それ以外はデ
;; フォルトの動作をする
(defun my-smart-tab-completion-func ()
  (cond
   ((and (featurep 'company) company-mode)
    (call-interactively #'company-complete))
   (t
    (smart-tab-default))))

(custom-set-variables
 '(smart-tab-completion-functions-alist ())
 '(smart-tab-user-provided-completion-function #'my-smart-tab-completion-func))

(global-smart-tab-mode)

(provide 'config-smart-tab)

