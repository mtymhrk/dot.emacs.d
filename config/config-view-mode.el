;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup for view-mode
;;   ref http://d.hatena.ne.jp/rubikitch/20081104/1225745862
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package view
  :config
  (setq view-read-only t)

  ;; view-mode キーバインド設定
  (defvar pager-keybind
    `( ;; vi-like
      ("h" . backward-char)
      ("l" . forward-char)
      ("j" . next-line)
      ("k" . previous-line)
      (";" . gene-word)
      ("b" . scroll-down)
      (" " . scroll-up)
      ("i" . view-mode) ; view-mode から脱出
      ;; w3m-like
      ("m" . gene-word)
      ("w" . forward-word)
      ("e" . backward-word)
      ("(" . point-undo)
      (")" . point-redo)
      ("J" . ,(lambda () (interactive) (scroll-up 1)))
      ("K" . ,(lambda () (interactive) (scroll-down 1)))
      ;; bm-easy
      ("." . bm-toggle)
      ("[" . bm-previous)
      ("]" . bm-next)
      ;; langhelp-like
      ("c" . scroll-other-window-down)
      ("v" . scroll-other-window)
      ))

  (defun define-many-keys (keymap key-table &optional includes)
    (let (key cmd)
      (dolist (key-cmd key-table)
        (setq key (car key-cmd)
              cmd (cdr key-cmd))
        (if (or (not includes) (member key includes))
            (define-key keymap key cmd))))
    keymap)

  (defun my-hook-view-mode--keybind ()
    (define-many-keys view-mode-map pager-keybind))

  (add-hook 'view-mode-hook 'my-hook-view-mode--keybind)


  ;; view-mode 時は背景色を変える
  (make-face 'view-mode-overlay-face)
  (set-face-background 'view-mode-overlay-face "#15101a")

  (defvar view-mode-overlay nil)
  (make-variable-buffer-local 'view-mode-overlay)

  (defun my-hook-view-mode--overlay ()
    (unless view-mode-overlay ; 何故か hook が複数回呼ばれるので、作成する overlay
                                        ; を一だけにするためにチェック
      (setq view-mode-overlay (make-overlay (point-min) (point-max)))
      (overlay-put view-mode-overlay 'face 'view-mode-overlay-face)))

  (add-hook 'view-mode-hook 'my-hook-view-mode--overlay)

  (defun my-view-mode-delete-overlay (&rest args)
    (when (and (not view-mode) view-mode-overlay)
      (delete-overlay view-mode-overlay)
      (setq view-mode-overlay nil)))

  (advice-add 'view-mode
              :after 'my-view-mode-delete-overlay)

  ;; (defadvice view-mode (after delete-view-mode-overlay activate)
  ;;   (when (and (not view-mode) view-mode-overlay)
  ;;     (delete-overlay view-mode-overlay)
  ;;     (setq view-mode-overlay nil)))

  :bind
  (:map keymap-ctrl-meta-space
        ("C-v" . view-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

