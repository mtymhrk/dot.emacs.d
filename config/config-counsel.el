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
  (enable-recursive-minibuffers t)
  :bind
  (:map keymap-ctrl-meta-space
        ("C-;" . ivy-switch-buffer)
        ("C-M-;" . ivy-resume)))

(use-package counsel
  :custom
  (counsel-yank-pop-separator "\n-------\n")
  :config
  (use-package mod-counsel)

  ;; find-file 中に C-w でディレクトリを 1 つ削除する
  (bind-key "C-w" 'counsel-up-directory counsel-find-file-map)
  (bind-key "C-w" 'ivy-backward-delete-char ivy-minibuffer-map)

  ;; grep/agの検索結果のファイルを C-z で参照する (デフォルトで C-M-m や  C-l にもバインドされている)
  (bind-key "C-z" 'ivy-call-and-recenter counsel-grep-map)
  (bind-key "C-z" 'ivy-call-and-recenter counsel-ag-map)

  :bind
  ("C-x C-f" . counsel-find-file)
  ("M-y" . counsel-yank-pop)
  (:map minibuffer-local-map
        ("C-r" . counsel-minibuffer-history))
  (:map keymap-for-grep
        ("g" . mod-counsel:counsel-ag))
  (:map keymap-ctrl-meta-space
        ("C-'" . counsel-imenu)
        ("o g" . mod-counsel:counsel-grep-my-memo)
        ("o r" . mod-counsel:counsel-open-my-memo))
  (:map keymap-for-manuals
        ("a" . counsel-apropos)))

(use-package swiper
  :bind
  (:map isearch-mode-map
        ("C-'" . swiper-from-isearch))
  (:map keymap-ctrl-meta-space
        ("C-o" . swiper)))
