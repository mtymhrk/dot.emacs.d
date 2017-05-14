;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'projectile)
(require 'helm-projectile)

(projectile-global-mode)

;;; helm-projectile のソース定義
;;; 2014-06-02: helm-source-projectile-buffers-list が helm の変更に追いつい
;;;             ておらず、エラーで動かないため同ソースをコメントアウト
(setq helm-projectile-sources-list
      '(; helm-source-projectile-buffers-list
        helm-source-projectile-files-list
        helm-source-projectile-recentf-list))

(defun my-helm-add-keybind (source key cmd)
  (let* ((src (symbol-value source))
         (keymap-attr (assoc 'keymap src)))
    (unless keymap-attr
      (setq keymap-attr (cons 'keymap (make-sparse-keymap)))
      (set source (cons keymap-attr src)))
    (define-key (cdr keymap-attr) (kbd key) cmd))
  cmd)

;;; helm-projectile 実行中に C-c C-s で switch-project を実行する
(defun my-helm-quit-and-exec-helm-projectile-switch-project ()
  (interactive)
  (helm-run-after-quit
   '(lambda ()
      (call-interactively 'helm-projectile-switch-project))))

(dolist (src-sym helm-projectile-sources-list)
  (my-helm-add-keybind src-sym "C-c C-s"
                       'my-helm-quit-and-exec-helm-projectile-switch-project))

;;; helm-projectile-switch-project の action に不満があるので変更
;; (setcdr (assoc "Switch to project"
;;                (cdr (assoc 'action helm-source-projectile-projects)))
;;         (lambda (project)
;;           (let ((projectile-switch-project-action 'helm-projectile))
;;             (projectile-switch-project-by-name project))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; プロジェクトの切り替え時の入力補完に helm を使用するための設定

;;; 2014/09/14: helm-projectile.el で switch-project の helm インタフェースが
;;;             実装され、独自実装が不要になったためコメントアウト

;; (require 'helm)

;; (defvar my-helm-map-for-projectile-switch-project
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map helm-map)))

;; (defun my-completing-read-for-projectile-switch-project (prompt choices)
;;   (helm :sources `((name . "Projectile Switch Project")
;;                    (candidates . ,choices)
;;                    (keymap . ,my-helm-map-for-projectile-switch-project)
;;                    (action . (lambda (x) x)))
;;         :buffer "*helm projectile switch project*"
;;         :prompt prompt))

;; (defun my-helm-projectile-switch-project (&optional arg)
;;   (interactive "P")
;;   (let ((projectile-completion-system 'my-completing-read-for-projectile-switch-project)
;;         (projectile-switch-project-action 'helm-projectile))
;;     (projectile-switch-project arg)))

;; (global-set-key (kbd "C-' p") 'my-helm-projectile-switch-project)


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

(defun my-projectile-project-root (init)
  (condition-case err
      (projectile-project-root)
    (error init)))

(defun my-helm-exec-projectile ()
  (interactive)
  (when my-helm-projectile-switch-to-projectile-p
    (helm-run-after-quit
     '(lambda ()
        (call-interactively (if (my-projectile-project-root nil)
                                'helm-projectile
                              'helm-projectile-switch-project))))))

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
(my-helm-add-keybind 'helm-source-buffers-list
                     "C-;" 'my-helm-exec-projectile)

(my-helm-add-keybind 'helm-source-recentf
                     "C-;" 'my-helm-exec-projectile)

(my-helm-add-keybind 'helm-source-file-cache
                     "C-;" 'my-helm-exec-projectile)

(my-helm-add-keybind 'helm-source-files-in-current-dir
                     "C-;" 'my-helm-exec-projectile)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; カレントディレクトリがプロジェクト内ではない場合、エラーにせず空の候補
;;; を作る helm ソース。
;;; helm-fileslist+ に含める目的で作成してみたが、表示が一瞬遅れるため不採
;;; 用

(defvar my-helm-source-projectile-files-list
  `((name . "Projectile Files")
    (init . (lambda ()
              (let* ((root (my-projectile-project-root "NotInAProject"))
                     (files (if (equal root "NotInAProject")
                                '()
                                (projectile-current-project-files))))
                (helm-projectile-init-buffer-with-files root files))))
    (coerce . helm-projectile-coerce-file)
    (candidates-in-buffer)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-find-file-help-message)
    (mode-line . helm-ff-mode-line-string)
    (type . file)
    (action . (lambda (file) (find-file file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-projectile)
