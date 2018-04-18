;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yasnippet.el
;;;   https://github.com/capitaomorte/yasnippet.git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package yasnippet
  :config
  ;; snippet のディレクトリを追加
  (push  (concat user-emacs-directory "yasnippet-snippets") yas-snippet-dirs)

  ;; スニペットの候補が複数あった場合、ドロップダウンのプロンプトを出さないよ
  ;; うにする。
  (setq yas-prompt-functions '(yas-completing-prompt yas-no-prompt)))

(use-package mod-yasnippet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm for yasnippet

(use-package helm-c-yasnippet
  :after (mod-helm)
  :config
  (setq helm-c-yas-space-match-any-greedy t)
  (setq helm-c-yas-display-key-on-candidate t)
  :bind
  (:map mod-helm:command-keymap
        ("s" . helm-c-yas-complete)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yasnippet 有効化

(yas/global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

