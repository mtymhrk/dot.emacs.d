;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; switch-buffer.el
;;;   http://www.emacswiki.org/emacs-en/download/switch-window.el
;;;   http://d.hatena.ne.jp/tomoya/20100807/1281150227
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (auto-install-from-url "https://raw.github.com/dimitri/switch-window/11ec2487889135d84eae45f25699d03b6fb91d14/switch-window.el")

(require 'switch-window)

(custom-set-variables
 '(switch-window-increase 14)
 '(switch-window-shortcut-style 'qwerty)
 '(switch-window-qwerty-shortcuts '("j" "k" "l" ";" "i" "o" "a" "s" "d" "f" "w" "e")))

;; (global-set-key (kbd "C-x C-o") 'switch-window)
(global-set-key (kbd "C-<tab>") 'switch-window)
(global-set-key (kbd "<C-S-iso-lefttab>") 'switch-window-then-delete)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; switch-window と delete-other-window で表示する数字の face を変更する設定

(defface my:delete-other-window-label-face
  '((t (:foreground "DarkRed")))
  "")

(defface my:switch-window-label-face
  '((t (:foreground "light sky blue")))
  "")

(defvar my:switch-window-display-number-face 'default)

(defun my:setup-switch-window-display-number-face (orig-func &rest args)
  (let ((buf (apply orig-func args)))
    (with-current-buffer buf
      (put-text-property (point-min) (point-max)
                         'face my:switch-window-display-number-face))
    buf))

(defun my:setup-face-for-switch-window (orig-func &rest args)
  (let ((my:switch-window-display-number-face 'my:switch-window-label-face))
    (apply orig-func args)))

(defun my:setup-face-for-delete-window (orig-func &rest args)
  (let ((my:switch-window-display-number-face 'my:delete-other-window-label-face))
    (apply orig-func args)))

(advice-add 'switch-window--display-number
            :around 'my:setup-switch-window-display-number-face)
(advice-add 'switch-window :around 'my:setup-face-for-switch-window)
(advice-add 'switch-window-then-delete :around 'my:setup-face-for-delete-window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-switch-window)
