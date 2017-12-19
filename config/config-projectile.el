;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'projectile)
(require 'helm-projectile)

(projectile-global-mode)

(setq  projectile-completion-system 'helm)

(helm-projectile-on)


(defun my-helm-add-keybind (source key cmd)
  (let* ((src (symbol-value source))
         (keymap-attr (assoc 'keymap src)))
    (unless keymap-attr
      (setq keymap-attr (cons 'keymap (make-sparse-keymap)))
      (set source (cons keymap-attr src)))
    (define-key (cdr keymap-attr) (kbd key) cmd))
  cmd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; グローバルキーマップの C-; にバインドされている helm の実行中に C-; を
;;; タイプすると helm-projectile を実行するようにし、helm-projectile 実行中
;;; に C-; をタイプするとグローバルキーマップの C-; にバインドされているコ
;;; マンドを実行するようにするための設定

(defvar my-helm-projectile-switch-to-projectile-p nil)

(defadvice helm (before my-helm-projectile-check-command activate)
  (setq my-helm-projectile-switch-to-projectile-p
        (or (eq this-command (global-key-binding (kbd "C-;")))
            (eq this-command 'my-helm-exec-C-Semicolon))))

(defun my-helm-exec-projectile ()
  (interactive)
  (when my-helm-projectile-switch-to-projectile-p
    (helm-run-after-quit
     '(lambda ()
        (call-interactively 'helm-projectile)))))

(defun my-helm-exec-C-Semicolon ()
  (interactive)
  (helm-run-after-quit
   '(lambda ()
      (call-interactively (global-key-binding (kbd "C-;"))))))

;;; helm-projectile の source の keymap にキーバインドを追加
(my-helm-add-keybind 'helm-source-projectile-buffers-list
                     "C-;" 'my-helm-exec-C-Semicolon)

(my-helm-add-keybind 'helm-source-projectile-files-list
                     "C-;" 'my-helm-exec-C-Semicolon)

(my-helm-add-keybind 'helm-source-projectile-recentf-list
                     "C-;" 'my-helm-exec-C-Semicolon)

(my-helm-add-keybind 'helm-source-projectile-projects
                     "C-;" 'my-helm-exec-C-Semicolon)

;;; 現在 C-; にバインドしている helm-filelist+ の source の keymap にキーバ
;;; インドを追加

(require 'helm-for-files)

(my-helm-add-keybind 'helm-source-buffers-list
                     "C-;" 'my-helm-exec-projectile)

(my-helm-add-keybind 'helm-source-recentf
                     "C-;" 'my-helm-exec-projectile)

(my-helm-add-keybind 'helm-source-file-cache
                     "C-;" 'my-helm-exec-projectile)

(my-helm-add-keybind 'helm-source-files-in-current-dir
                     "C-;" 'my-helm-exec-projectile)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-projectile)
