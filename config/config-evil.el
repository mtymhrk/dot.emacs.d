;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'evil)

;;; insert ステートでは emacs のキーバインドを完全に emacs 互換に
(setq evil-insert-state-map nil)

;;; insert ステートから normal ステートへ戻るためと、evil-local-mode が無効の
;;; 状態から evil-loadl-mode を有効にするためのキーバインド設定
(global-set-key (kbd "M-ESC") 'evil-normal-state)

;;; insert ステートへの遷移時にカーソルの形状、色の変更を行わない
(setq evil-insert-state-cursor nil)

;;; C-z が emacs ステートへの遷移にバインドされているのを解除する (elscreen で
;;; 使用しているため)。emacs ステートへの遷移と emacs ステートからの脱出は M-x
;;; それぞれ evil-emacs-state、evil-exit-emacs-state を実行する
(custom-set-variables '(evil-toggle-key ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Util

(defvar my-evil-state-entry-hook-list
  '(evil-normal-state-entry-hook
    evil-insert-state-entry-hook
    evil-visual-state-entry-hook
    evil-replace-state-entry-hook
    evil-operator-state-entry-hook
    evil-motion-state-entry-hook
    evil-emacs-state-entry-hook))

(defvar my-evil-state-exit-hook-list
  '(evil-normal-state-exit-hook
    evil-insert-state-exit-hook
    evil-visual-state-exit-hook
    evil-replace-state-exit-hook
    evil-operator-state-exit-hook
    evil-motion-state-exit-hook
    evil-emacs-state-exit-hook))

(defun my-add-to-all-evil-state-entry-hook (func &optional append local)
  (dolist (hook my-evil-state-entry-hook-list)
    (add-hook hook func append local)))

(defun my-add-to-all-evil-state-exit-hook (func &optional append local)
  (dolist (hook my-evil-state-exit-hook-list)
    (add-hook hook func append local)))

(defun my-remove-from-all-evil-state-entry-hook (func)
  (dolist (hook my-evil-state-entry-hook-list)
    (remove-hook hook func)))

(defun my-remove-from-all-evil-state-exit-hook (func)
  (dolist (hook my-evil-state-exit-hook-list)
    (remove-hook hook func)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; normal ステート では背景色を変える
;;;   (evil-mode 1) で全メジャーモードで evil を有効にするとうまく動かない。

(defvar my-evil-overlay nil)
(make-variable-buffer-local 'my-evil-overlay)

(defvar my-evil-overlay-faces
  '((normal . ((:background "#15101a")))
    (operator . ((:background "#15101a")))
    (motion . ((:background "#15101a")))))

(defun my-evil-update-overlay ()
  (let ((elm (assoc evil-state my-evil-overlay-faces)))
    (if my-evil-overlay
        (if elm
            (move-overlay my-evil-overlay (point-min) (point-max))
          (delete-overlay my-evil-overlay)
          (setq my-evil-overlay nil))
      (setq my-evil-overlay (make-overlay (point-min) (point-max))))
    (when elm
      (overlay-put my-evil-overlay 'face (cdr elm)))))

(defun evil-state-entry-hook--make-overlay ()
  (my-evil-update-overlay))

(my-add-to-all-evil-state-entry-hook 'evil-state-entry-hook--make-overlay)

(defun evil-state-exit-hook--delete-overlay-if-needed ()
  (when (and (not evil-local-mode) my-evil-overlay)
    (delete-overlay my-evil-overlay)
    (setq my-evil-overlay nil)))

(my-add-to-all-evil-state-exit-hook
 'evil-state-exit-hook--delete-overlay-if-needed)

(add-hook 'post-command-hook 'my-evil-update-overlay)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Skk との共存設定

;;; skkによるカーソル色変更を, evil 無効時か, 挿入ステートかつ skk モードの場
;;; 合に限定
(defadvice update-buffer-local-cursor-color
  (around evil-update-buffer-local-cursor-color-in-insert-state activate)
  "Allow ccc to update cursor color only when we are in insert
state and in `skk-mode'."
  (when (or (not evil-local-mode)
            (and (eq evil-state 'insert) (boundp 'skk-mode) skk-mode))
    ad-do-it))

;;; Evilによるカーソルの変更を, 挿入ステートかつ skk モードではない場合に限定
(defadvice evil-refresh-cursor
  (around evil-refresh-cursor-unless-skk-mode activate)
  "Allow ccc to update cursor color only when we are in insert
state and in `skk-mode'."
  (unless (and (eq evil-state 'insert) (boundp 'skk-mode) skk-mode)
    ad-do-it))

;;; SKKの未確定状態(skk-henkan-mode)ではない場合だけ, 検索パターンをアップデート
(defadvice evil-ex-search-update-pattern
  (around evil-inhibit-ex-search-update-pattern-in-skk-henkan activate)
  "Inhibit search pattern update during `skk-henkan-mode'.
This is reasonable since inserted text during `skk-henkan-mode'
is a kind of temporary one which is not confirmed yet."
  (unless (and (boundp 'skk-henkan-mode) skk-henkan-mode)
    ad-do-it))

;;; insert ステートと emacs ステートから抜ける再に skk-mode を off にする
(defun evil-state-exit-hook--turn-off-skk-mode ()
  (skk-mode -1))

(add-hook 'evil-insert-state-exit-hook
          'evil-state-exit-hook--turn-off-skk-mode)
(add-hook 'evil-emacs-state-exit-hook
          'evil-state-exit-hook--turn-off-skk-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evil-mode の有効化
;;;   (evil-mode 1) を使って全てのメジャーモードで evil-mode を有効にするので
;;;   はなく、必要なもメージャモードだけ有効にする

;; (add-hook 'text-mode-hook 'turn-on-evil-mode)
;; (add-hook 'lisp-mode-hook 'turn-on-evil-mode)
;; (add-hook 'emacs-lisp-mode-hook 'turn-on-evil-mode)
;; (add-hook 'scheme-mode-hook 'turn-on-evil-mode)
;; (add-hook 'c-mode-hook 'turn-on-evil-mode)
;; (add-hook 'c++-mode-hook 'turn-on-evil-mode)
;; (add-hook 'ruby-mode-hook 'turn-on-evil-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar hli-evil-str-alist
  '((normal "N" . hli-face-header-line)
    (insert "I" . hli-face-header-line)
    (visual "V" . hli-face-header-line)
    (repalce "R" . hli-face-header-line)
    (operator "O" . hli-face-header-line)
    (motion "M" . hli-face-header-line)
    (emacs "E" . hli-face-header-line)))

(defun hli-evil-make-header-line ()
  (if (not evil-local-mode)
      (propertize (hli-format-info-item "-") 'face 'hli-face-header-line)
    (let ((elm (assoc evil-state hli-evil-str-alist)))
      (propertize (hli-format-info-item (if elm
                                            (propertize (cadr elm)
                                                        'face (cddr elm))
                                          "?"))
                  'face 'hli-face-header-line))))

(defvar hli-evil-header-line-format
  (hli-evil-make-header-line))

(put 'hli-evil-header-line-format 'risky-local-variable t)
(make-variable-buffer-local 'hli-evil-header-line-format)

(defvar hli-information-evil
  (hli-make-info 'hli-evil-enable 'hli-evil-disable))

(defun evil-state-entry-hook--hli ()
  (setq hli-evil-header-line-format
        (hli-evil-make-header-line)))

(defun evil-state-exit-hook--hli ()
  (setq hli-evil-header-line-format
        (hli-evil-make-header-line)))

(defun hli-evil-enable ()
  (my-add-to-all-evil-state-entry-hook 'evil-state-entry-hook--hli nil t)
  (my-add-to-all-evil-state-exit-hook 'evil-state-exit-hook--hli nil t)
  (evil-state-entry-hook--hli)
  'hli-evil-header-line-format)

(defun hli-evil-disable ()
  (my-remove-from-all-evil-state-entry-hook 'evil-state-entry-hook--hli)
  (my-remove-from-all-evil-state-exit-hook 'evil-state-exit-hook--hli)
  'hli-evil-header-line-format)

(eval-after-load 'header-line-info
  '(setq-default hli-information-list
                 (cons 'hli-information-evil hli-information-list)))

;; (defun evil-local-mode-hook--add-hli-info ()
;;   (unless (memq 'hli-information-evil hli-information-list)
;;     (make-local-variable 'hli-information-list)
;;     (push 'hli-information-evil hli-information-list)
;;     (when header-line-info-mode
;;       (hli-toggle-header-line-info-mode)
;;       (hli-toggle-header-line-info-mode))))

;; (add-hook 'evil-local-mode-hook 'evil-local-mode-hook--add-hli-info)


