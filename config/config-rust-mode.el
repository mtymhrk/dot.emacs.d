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

  ;; Rust Language Server
  (use-package lsp-rust
    :hook
    ((rust-mode . lsp-rust-enable))
    ;; :custom
    ;; (lsp-rust-rls-command  '("rustup" "run" "nightly" "rls"))
    :config
    ;; clippy による lint を常に有効にする
    ;; (lsp-rust-set-config "clippy_preference" "on")
    )

  (use-package fill-column-indicator
    :init
    (defun my-hook-rust-mode-common--fci ()
      (setq fill-column 80)
      (fci-mode 1))
    :hook
    ((rust-mode . my-hook-rust-mode-common--fci))))
