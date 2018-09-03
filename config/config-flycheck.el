;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when-compile (require 'use-package))

(use-package flycheck
  :commands flycheck-mode
  :custom
  ;; mode-line での flycheck の情報表示を無効に
  ;; (flycheck-mode-line "")
  ;; カーソル位置のエラー情報をエコーエリアに表示しない
  (flycheck-display-errors-function nil)

  :config
  ;; 無効化する checker の設定
  ;; emacs-lisp-checkdoc を無効化する
  (setq-default flycheck-disabled-checkers
                (cons 'emacs-lisp-checkdoc
                      (default-value 'flycheck-disabled-checkers)))


  ;; (eval-after-load 'config-auto-save-buffers
  ;;   '(progn
  ;;      (defun flycheck-mode-hook--remove-after-save-hook ()
  ;;        (remove-hook 'after-save-hook 'flycheck-handle-save 'local))

  ;;      (add-hook 'flycheck-mode-hook
  ;;                'flycheck-mode-hook--remove-after-save-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm for flycheck

  (use-package helm-flycheck
    :after (mod-helm)
    :commands helm-flycheck
    :hook
    ((flycheck-mode . (lambda ()
                        (bind-key "C-`" 'helm-flycheck keymap-ctrl-meta-space))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


