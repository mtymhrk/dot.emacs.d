;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'flycheck)

(custom-set-variables
 ;;; mode-line での flycheck の情報表示を無効に
 '(flycheck-mode-line "")
 ;;; カーソル位置のエラー情報をエコーエリアに表示しない
 '(flycheck-display-errors-function nil))

;;; 無効化する checker の設定
;;; emacs-lisp-checkdoc を無効化する
(setq-default flycheck-disabled-checkers
              (cons 'emacs-lisp-checkdoc
                    (default-value 'flycheck-disabled-checkers)))


(eval-after-load 'config-auto-save-buffers
  '(progn
     (defun flycheck-mode-hook--remove-after-save-hook ()
       (remove-hook 'after-save-hook 'flycheck-handle-save 'local))

     (add-hook 'flycheck-mode-hook
               'flycheck-mode-hook--remove-after-save-hook)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm for flycheck

(eval-after-load 'config-helm
  '(progn
     (require 'helm-flymake-or-flycheck)

     (define-key my:helm-command-keymap (kbd "`") 'helm-flymake-or-flycheck)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-flycheck)
