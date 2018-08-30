;; -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  ag.el
;;;    Silver Searcher Ag: very fast grep-like program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ag
  :config

  ;; popwin for ag
  (use-package mod-popwin
    :config
    (mod-popwin:add-display-config '(ag-mode :noselect t :stick t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  helm-ag
;;;    helm interface for ag
(use-package helm-ag
  :after (mod-helm)
  :config
  ;; デフォルトで検索ディレクトリを入力するよう変更
  (defalias 'orig-helm-ag (symbol-function 'helm-ag))
  (defalias 'orig-helm-do-ag (symbol-function 'helm-do-ag))

  (defun helm-ag ()
    (interactive)
    (orig-helm-ag (read-directory-name "Search Directory: ")))

  (defun helm-do-ag ()
    (interactive)
    (orig-helm-do-ag (read-directory-name "Search Directory: ")))

  ;; project を検索ディレクトリとするコマンドを定義
  (defun helm-ag-project ()
    (interactive)
    (orig-helm-ag (ag/project-root default-directory)))

  (defun helm-do-ag-project ()
    (interactive)
    (orig-helm-do-ag (ag/project-root default-directory)))

  ;; helm-do-ag から ag を起動するコマンド
  (defun helm-quit-and-exec-ag ()
    (interactive)
    (let ((pattern helm-pattern)
          (dir helm-ag--default-directory))
      (helm-run-after-quit
       #'(lambda () (ag pattern dir)))))

  (bind-key "C-c o" 'helm-quit-and-exec-ag helm-ag-map)

  :bind
  (:map keymap-ctrl-meta-space
        ("C-g" . helm-do-ag)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

