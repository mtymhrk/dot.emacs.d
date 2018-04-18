;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rust-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package rust-mode
  :commands rust-mode
  :config
  ;; rust-modeでrust-format-on-saveをtにすると自動でrustfmtが走る
  (setq-default rust-format-on-save t)

  ;; rustのファイルを編集するときにracerを起動する
  (add-hook 'rust-mode-hook #'racer-mode)

  ;; rustのファイルを編集するときにflycheckを起動する
  (use-package flycheck
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    :hook
    ((rust-mode . flycheck-mode)))

  ;; racerのeldocサポートを使う
  (add-hook 'racer-mode-hook #'eldoc-mode)

  ;; racerの補完サポートを使う
  (use-package company
    :hook
    ((racer-mode . company-mode))))

(provide 'config-rust-mode)
