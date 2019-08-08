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
  ;; (flycheck-display-errors-function nil)
  ;; カーソル位置のエラーを表示するまでの時間
  ;; 手動表示のみしたいが、できないので大きな時間を設定して自動表示させないよう
  ;; にする
  (flycheck-display-errors-delay 10000.0)

  :config
  ;; 無効化する checker の設定
  ;; emacs-lisp-checkdoc を無効化する
  (setq-default flycheck-disabled-checkers
                (cons 'emacs-lisp-checkdoc
                      (default-value 'flycheck-disabled-checkers)))


  (use-package flycheck-popup-tip
    :hook
    ((flycheck-mode . flycheck-popup-tip-mode)))

  ;; (eval-after-load 'config-auto-save-buffers
  ;;   '(progn
  ;;      (defun flycheck-mode-hook--remove-after-save-hook ()
  ;;        (remove-hook 'after-save-hook 'flycheck-handle-save 'local))

  ;;      (add-hook 'flycheck-mode-hook
  ;;                'flycheck-mode-hook--remove-after-save-hook)))

  (use-package mod-popwin
    :config
    ;; エラーリストをポップアップで表示
    (mod-popwin:add-display-config '(flycheck-error-list-mode :noselect t :stick t)))

  (use-package hydra
    :config
    (defhydra hydra-flycheck ()
      "
Flycheck
"
      ("n" flycheck-next-error             "next")
      ("p" flycheck-previous-error         "previous")
      ("h" flycheck-display-error-at-point "display")
      ("e" flycheck-explain-error-at-point "explain")
      ("l" flycheck-list-errors            "list")
      ("q" nil                             "quit")))

  (bind-keys :map keymap-for-code-navigation
             ("c" . flycheck-buffer)
             ("n" . hydra-flycheck/flycheck-next-error)
             ("p" . hydra-flycheck/flycheck-previous-error)
             ("h" . hydra-flycheck/flycheck-display-error-at-point)
             ("e" . hydra-flycheck/flycheck-explain-error-at-point)
             ("l" . hydra-flycheck/flycheck-list-errors))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


