;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm
;;;   https://github.com/emacs-helm/helm/wiki
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm-config)


(use-package helm ; helm-map を参照するには helm.el のロードが必要
  :defer t
  :config

  ;; helm-scroll-other-window のスクロール量を 1 行に設定
  (setq helm-scroll-amount 1)

  ;; キーバインド (helm-map は helm.el で定義されているので :bind は使えない)
  (bind-key "C-M-j" 'helm-scroll-other-window      helm-map)
  (bind-key "C-M-k" 'helm-scroll-other-window-down helm-map)
  (bind-key "C-w"   'backward-kill-word            helm-map)

  ;; helm バッファをフレーム下部にポップアップする設定
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4)
                 (side . bottom))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm-buffers
  :defer t
  :custom
  ;; buffer 名の表示領域を広くする
  (helm-buffer-max-length 50)

  :config
  ;; helm-souce-buffers-list が helm-buffers-list コマンドを実行するまで設定
  ;; されないようになったため、あらかじめ設定しておく
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))

  ;; buffer のリストが名前の長さでソートされるのを防ぐ
  (defadvice helm-buffers-sort-transformer (around ignore activate)
    (setq ad-return-value (ad-get-arg 0))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-filelist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm-filelist
  :defer t
  :custom
  (helm-filelist-file-name "/tmp/anything-filelist.all.filelist")
  (helm-filelist-async t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mod-helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package mod-helm)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; キーバインド
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO: delte
(defvar my:helm-command-keymap mod-helm:command-keymap)

;; (global-set-key (kbd "C-;") 'helm-for-files)
(global-set-key (kbd "C-;") 'mod-helm:filelist)
(global-set-key (kbd "C-M-;") 'mod-helm:move-in-buffer)
(global-set-key (kbd "C-'") my:helm-command-keymap)
(global-set-key (kbd "C-M-'") 'helm-resume)
;; (global-set-key (kbd "C-x C-f") 'my-helm-C-x-C-f)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-X") 'execute-extended-command)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-o") 'helm-occur)

(define-key my:helm-command-keymap (kbd "C-y") 'helm-show-kill-ring)
(define-key my:helm-command-keymap (kbd "C-M-s") 'helm-regexp)
(define-key my:helm-command-keymap (kbd "a") 'helm-apropos)
(define-key my:helm-command-keymap (kbd "m") 'mod-helm:manuals)
(define-key my:helm-command-keymap (kbd "g") 'helm-do-grep-ag)



;;; TODO: delete
(defun my:helm-override-keymap (source-sym keymap)
  (let* ((source (symbol-value source-sym))
         (k (assq 'keymap source)))
    (if (and k (not (eq k keymap)))
        (progn (set-keymap-parent keymap (cdr k))
               (setcdr k keymap))
      (set source-sym
           (cons (cons 'keymap keymap) source)))))




