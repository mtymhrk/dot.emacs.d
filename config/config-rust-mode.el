;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rust-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; rust-modeでrust-format-on-saveをtにすると自動でrustfmtが走る
(eval-after-load "rust-mode"
  '(setq-default rust-format-on-save t))

;;; rustのファイルを編集するときにracerを起動する
(add-hook 'rust-mode-hook #'racer-mode)

;;; rustのファイルを編集するときにflycheckを起動する
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook #'flycheck-mode))

;;; racerのeldocサポートを使う
(add-hook 'racer-mode-hook #'eldoc-mode)

;;; racerの補完サポートを使う
(with-eval-after-load 'company
  (add-hook 'racer-mode-hook #'company-mode))

(provide 'config-rust-mode)
