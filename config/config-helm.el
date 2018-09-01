;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm
;;;   https://github.com/emacs-helm/helm/wiki
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm-config
  :config

  (use-package helm ; helm-map を参照するには helm.el のロードが必要
    :defer t
    :delight
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
;;; mod-helm & keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package mod-helm
    :bind
    ("M-x" . helm-M-x)
    ("M-X" . execute-extended-command)
    ("M-y" . helm-show-kill-ring)
    ;; ("C-;" . helm-for-files)
    (:map keymap-ctrl-meta-space
          ("C-;"   . mod-helm:filelist)
          ("C-'"   . mod-helm:move-in-buffer)
          ("C-M-;" . helm-resume)
          ("C-M-s" . helm-regexp)
          ("C-o"   . helm-occur)
          ("C-g"   . helm-do-grep-ag))
    (:map keymap-for-manuals
          ("a" . helm-apropos)
          ("m" . helm-man-woman)
          ("i" . helm-info)))
)
