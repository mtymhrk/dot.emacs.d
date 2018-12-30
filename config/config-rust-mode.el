;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rust-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package rust-mode
  :commands rust-mode
  :config

  ;; rust-modeでrust-format-on-saveをtにすると自動でrustfmtが走る
  (setq-default rust-format-on-save t)

  ;; rustのファイルを編集するときにflycheckを起動する
  (use-package flycheck
    :config
    ;; lsp-mode の flycheck 設定を使用するため、flycheck-rust-setup は hook に追
    ;; 加しない
    ;; (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    :hook
    ((rust-mode . flycheck-mode)))

  ;; ;; Racer - code completion for Rust
  ;; (use-package racer
  ;;   :commands racer-mode
  ;;   :delight
  ;;   :hook
  ;;   ;; rustのファイルを編集するときにracerを起動する
  ;;   ((rust-mode . racer-mode)
  ;;    ;; racerのeldocサポートを使う
  ;;    (racer-mode . eldoc-mode)
  ;;    ;; ;; racerの補完サポートを使う
  ;;    (racer-mode . company-mode)))

  (use-package lsp-mode
    :config

    (use-package mod-lsp-mode
      :config
      ;; clippy によるチェックを常に行うよう設定
      (defun my-rls-after-initialize-fn (workspace)
        (mod-lsp:rls-set-settings '(("clippy_preference" . "on"))))
      (mod-lsp:add-after-initialized-hook 'rls 'my-rls-after-initialize-fn))

    :hook
    ((rust-mode . lsp)))

  (use-package fill-column-indicator
    :init
    (defun my-hook-rust-mode-common--fci ()
      (setq fill-column 80)
      (fci-mode 1))
    :hook
    ((rust-mode . my-hook-rust-mode-common--fci))))
