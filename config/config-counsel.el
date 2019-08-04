;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; counsel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy
  :commands ivy-mode
  :delight
  :init
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-format-functions-alist '((t . ivy-format-function-arrow)))
  (ivy-height-alist '((swiper . 25)))
  (enable-recursive-minibuffers t))

(use-package counsel
  :custom
  (counsel-yank-pop-separator "\n-------\n")
  :config

  ;; ag を起動するディレクトリを常に問うバージョンの counsel-ag
  (defun my-counsel-ag ()
    (interactive)
    (if current-prefix-arg
        (call-interactively #'counsel-ag)
      (setq current-prefix-arg 4)
      (counsel-ag nil nil "")))

  ;; find-file 中に C-w でディレクトリを 1 つ削除する
  (bind-key "C-w" 'counsel-up-directory counsel-find-file-map)

  ;; grep/agの検索結果のファイルを C-z で参照する (デフォルトで C-l にもバインドされている)
  (bind-key "C-z" 'ivy-call-and-recenter counsel-grep-map)
  (bind-key "C-z" 'ivy-call-and-recenter counsel-ag-map)

  :bind
  ("C-x C-f" . counsel-find-file)
  ("M-y" . counsel-yank-pop)
  (:map minibuffer-local-map
        ("C-r" . counsel-minibuffer-history))
  (:map keymap-for-grep
        ("g" . my-counsel-ag)))

(use-package swiper
  :bind
  (:map isearch-mode-map
        ("C-'" . swiper-from-isearch))
  (:map keymap-ctrl-meta-space
        ("C-'" . swiper)))
