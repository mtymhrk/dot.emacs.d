;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mod-yasnippet.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(with-eval-after-load 'yasnippet

  ;;
  ;; yasnippet 展開中は flymake、flycheck を無効にする
  ;;

  (make-variable-frame-local 'mod-yasnippet:flymake-active-p)
  (setq-default mod-yasnippet:flymake-active-p nil)

  (make-variable-frame-local 'mod-yasnippet:flycheck-active-p)
  (setq-default mod-yasnippet:flycheck-active-p nil)

  (defun yas-before-expand-snippet-hook--deactivate-flymake ()
    (setq mod-yasnippet:flymake-active-p
                   (or mod-yasnippet:flymake-active-p
                       (assoc-default 'flymake-mode
                                      (buffer-local-variables))))
    (when mod-yasnippet:flymake-active-p
      (flymake-mode-off))

    (setq mod-yasnippet:flycheck-active-p
          (or mod-yasnippet:flycheck-active-p
              (assoc-default 'flycheck-mode
                             (buffer-local-variables))))
    (when mod-yasnippet:flycheck-active-p
      (flycheck-mode -1)))

  (add-hook 'yas-before-expand-snippet-hook
            'yas-before-expand-snippet-hook--deactivate-flymake)

  (defun yas-after-exit-snippet-hook--activate-flymake ()
    (when mod-yasnippet:flymake-active-p
      (flymake-mode-on)
      (setq mod-yasnippet:flymake-active-p nil))

    (when mod-yasnippet:flycheck-active-p
      (flycheck-mode t)
      (setq mod-yasnippet:flycheck-active-p nil)))

  (add-hook 'yas-after-exit-snippet-hook
            'yas-after-exit-snippet-hook--activate-flymake))

(provide 'mod-yasnippet)
