;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yasnippet.el
;;;   https://github.com/capitaomorte/yasnippet.git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package yasnippet
  :delight yas-minor-mode
  :config
  ;; snippet のディレクトリを追加
  (push  (concat user-emacs-directory "yasnippet-snippets") yas-snippet-dirs)

  ;; スニペットの候補が複数あった場合、ドロップダウンのプロンプトを出さないよ
  ;; うにする。
  (setq yas-prompt-functions '(yas-completing-prompt yas-no-prompt)))

(use-package mod-yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yasnippet 有効化

(yas/global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

