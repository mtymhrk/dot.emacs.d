;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mod-switch-buffer.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'switch-window

  ;; switch-window と delete-other-window で表示する数字の face を変更する設定

  (defun mod-switch-window:setup-face-for-switch (orig-func &rest args)
    (set-face-foreground 'switch-window-label "light sky blue")
    (apply orig-func args))

  (defun mod-switch-window:setup-face-for-delete (orig-func &rest args)
    (set-face-foreground 'switch-window-label "DarkRed")
    (apply orig-func args))

  (advice-add 'switch-window :around 'mod-switch-window:setup-face-for-switch)
  (advice-add 'switch-window-then-delete :around 'mod-switch-window:setup-face-for-delete))


(provide 'mod-switch-window)
