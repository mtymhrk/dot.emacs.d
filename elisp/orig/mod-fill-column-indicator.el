;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mod-fill-column-indicator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'fill-column-indicator
  (with-eval-after-load 'company

    ;; fci-mode を有効にしていると company の補完候補リストの表示が見だれる(カー
    ;; ソル位置から離れた場所に表示される)場合があるので company の補完中は
    ;; fci-mode を off にする設定

    (make-variable-buffer-local 'mod-fci:fci-active-p)
    (setq-default mod-fci:fci-active-p nil)

    (defun mod-fci:temporary-disable-fci-mode (&rest args)
      (setq mod-fci:fci-active-p
            (or mod-fci:fci-active-p
                (assoc-default 'fci-mode
                               (buffer-local-variables))))
      (when mod-fci:fci-active-p
        (turn-off-fci-mode)))

    (defun mod-fci:restore-temporary-disabled-fci-mode (&rest args)
      (when mod-fci:fci-active-p
        (turn-on-fci-mode)
        (setq mod-fci:fci-active-p nil)))

    (add-hook 'company-completion-started-hook
              #'mod-fci:temporary-disable-fci-mode)

    (add-hook 'company-completion-cancelled-hook
              #'mod-fci:restore-temporary-disabled-fci-mode)

    (add-hook 'company-completion-finished-hook
              #'mod-fci:restore-temporary-disabled-fci-mode)))


(provide 'mod-fill-column-indicator)
