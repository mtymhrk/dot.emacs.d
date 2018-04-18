;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mod-switch-buffer.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'switch-window

  ;; switch-window と delete-other-window で表示する数字の face を変更する設定

  (defface mod-switch-window:delete-label-face
    '((t (:foreground "DarkRed")))
    "")

  (defface mod-switch-window:switch-label-face
    '((t (:foreground "light sky blue")))
    "")

  (defvar mod-switch-window:display-number-face 'default)

  (defun mod-switch-window:setup-display-number-face (orig-func &rest args)
    (let ((buf (apply orig-func args)))
      (with-current-buffer buf
        (put-text-property (point-min) (point-max)
                           'face mod-switch-window:display-number-face))
      buf))

  (defun mod-switch-window:setup-face-for-switch (orig-func &rest args)
    (let ((mod-switch-window:display-number-face 'mod-switch-window:switch-label-face))
      (apply orig-func args)))

  (defun mod-switch-window:setup-face-for-delete (orig-func &rest args)
    (let ((mod-switch-window:display-number-face 'mod-switch-window:delete-label-face))
      (apply orig-func args)))

  (advice-add 'switch-window--display-number
              :around 'mod-switch-window:setup-display-number-face)
  (advice-add 'switch-window :around 'mod-switch-window:setup-face-for-switch)
  (advice-add 'switch-window-then-delete :around 'mod-switch-window:setup-face-for-delete))


(provide 'mod-switch-window)
