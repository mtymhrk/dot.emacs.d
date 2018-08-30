;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rust-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package rust-mode
  :commands rust-mode
  :config

  (use-package racer
    :commands racer-mode
    :delight)

  ;; rust-modeでrust-format-on-saveをtにすると自動でrustfmtが走る
  (setq-default rust-format-on-save nil)

  ;; rustのファイルを編集するときにflycheckを起動する
  (use-package flycheck
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    :hook
    ((rust-mode . flycheck-mode)))

  ;; rustのファイルを編集するときにracerを起動する
  (add-hook 'rust-mode-hook #'racer-mode)

  ;; racerのeldocサポートを使う
  (add-hook 'racer-mode-hook #'eldoc-mode)

  ;; racerの補完サポートを使う
  (use-package company
    :hook
    ((racer-mode . company-mode)))

  ;; ;; Rust Language Server
  ;; (use-package lsp-rust
  ;;   :hook
  ;;   ((rust-mode . lsp-rust-enable))
  ;;   :custom
  ;;   (lsp-rust-rls-command  '("rustup" "run" "nightly" "rls")))

  (use-package fill-column-indicator
    :init
    (defun my-hook-rust-mode-common--fci ()
      (setq fill-column 80)
      (fci-mode 1))
    :hook
    ((rust-mode . my-hook-rust-mode-common--fci))))
