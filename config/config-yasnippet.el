;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yasnippet.el
;;;   https://github.com/capitaomorte/yasnippet.git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'yasnippet)

;;; snippet のディレクトリを追加
(push  (concat user-emacs-directory "yasnippet-snippets") yas-snippet-dirs)

;;; スニペットの候補が複数あった場合、ドロップダウンのプロンプトを出さないよ
;;; うにする。
(setq yas-prompt-functions '(yas-completing-prompt yas-no-prompt))
;; (setq yas-prompt-functions '(yas-no-prompt))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yasnippet 展開中は flymake、flycheck を無効にする

(make-variable-frame-local 'my-yas-flymake-active-p)
(setq-default my-yas-flymake-active-p nil)

(make-variable-frame-local 'my-yas-flycheck-active-p)
(setq-default my-yas-flycheck-active-p nil)

(defun yas-before-expand-snippet-hook--deactivate-flymake ()
  (setq my-yas-flymake-active-p
                   (or my-yas-flymake-active-p
                       (assoc-default 'flymake-mode
                                      (buffer-local-variables))))
  (when my-yas-flymake-active-p
    (flymake-mode-off))

  (setq my-yas-flycheck-active-p
                   (or my-yas-flycheck-active-p
                       (assoc-default 'flycheck-mode
                                      (buffer-local-variables))))
  (when my-yas-flymake-active-p
    (flymake-mode -1))
  )

(add-hook 'yas-before-expand-snippet-hook
          'yas-before-expand-snippet-hook--deactivate-flymake)

(defun yas-after-exit-snippet-hook--activate-flymake ()
  (when my-yas-flymake-active-p
    (flymake-mode-on)
    (setq my-yas-flymake-active-p nil))

  (when my-yas-flycheck-active-p
    (flycheck-mode t)
    (setq my-yas-flycheck-active-p nil))
  )

(add-hook 'yas-after-exit-snippet-hook
          'yas-after-exit-snippet-hook--activate-flymake)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm for yasnippet

(eval-after-load 'config-helm
  '(progn
     (require 'helm-c-yasnippet)

     (setq helm-c-yas-space-match-any-greedy t)
     (setq helm-c-yas-display-key-on-candidate t)

     (define-key my:helm-command-keymap (kbd "s") 'helm-c-yas-complete)
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yasnippet 有効化

(yas/global-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-yasnippet)
