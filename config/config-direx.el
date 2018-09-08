;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; direx.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package direx
  :commands direx-project:jump-to-project-root-other-window direx:jump-to-directory-other-window
  :init

  ;; direx-projectile を試してダメだったら direx を実行する
  (defun my-direx:jump-to-project-directory ()
    (interactive)
    (let ((result (ignore-errors
                    (direx-project:jump-to-project-root-other-window)
                    t)))
      (unless result
        (direx:jump-to-directory-other-window))))

  ;; font-lock-mode が有効だと face が有効にならないので、無効にする
  (defun my-direx:disable-font-lock-mode ()
    (font-lock-mode -1))

  ;; dired-x がロードされるとキーバインドを上書きさるので、再度キーバインドを設定
  (with-eval-after-load 'dired-x
    (bind-key "C-x C-j" #'my-direx:jump-to-project-directory))

  :custom
  (direx:open-icon "▾ ")
  (direx:closed-icon "▸ ")

  :config
  (use-package mod-popwin
    :config
    (mod-popwin:add-display-config
     '(direx:direx-mode :position left :width 40 :dedicated t)))

  :hook
  ((direx:direx-mode . my-direx:disable-font-lock-mode))

  :bind
  ("C-x C-j" . my-direx:jump-to-project-directory))
