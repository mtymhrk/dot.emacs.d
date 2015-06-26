;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; switch-buffer.el
;;;   http://www.emacswiki.org/emacs-en/download/switch-window.el
;;;   http://d.hatena.ne.jp/tomoya/20100807/1281150227
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (auto-install-from-url "https://raw.github.com/dimitri/switch-window/11ec2487889135d84eae45f25699d03b6fb91d14/switch-window.el")

(require 'switch-window)

(custom-set-variables
 '(switch-window-increase 10)
 '(switch-window-shortcut-style 'qwerty)
 '(switch-window-qwerty-shortcuts '("j" "k" "l" ";" "i" "o" "a" "s" "d" "f" "w" "e")))

;; (global-set-key (kbd "C-x C-o") 'switch-window)
(global-set-key (kbd "C-<tab>") 'switch-window)
(global-set-key (kbd "<C-S-iso-lefttab>") 'delete-other-window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; switch-window と delete-other-window で表示する数字の face を変更する設定

(defface my-delete-other-window-label-face
  '((t (:foreground "DarkRed")))
  "")

(defface my-switch-window-label-face
  '((t (:foreground "light sky blue")))
  "")

(defvar my-switch-window-display-number-face 'default)

(defadvice switch-window-display-number
  (after setup-switch-window-display-number-face)
  (with-current-buffer ad-return-value
    (put-text-property (point-min) (point-max)
                       'face my-switch-window-display-number-face)))

(defadvice switch-window (around setup-face-for-switch-window)
  (let ((my-switch-window-display-number-face 'my-switch-window-label-face))
    ad-do-it))

(defadvice delete-other-window (around setup-face-for-delete-other-window)
  (let ((my-switch-window-display-number-face 'my-delete-other-window-label-face))
    ad-do-it))

(ad-activate 'switch-window-display-number)
(ad-activate 'switch-window)
(ad-activate 'delete-other-window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-switch-window)
