;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ruby-mode
;;;   ELPA: Marmalade
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package ruby-mode
  :commands ruby-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

  :config
  (use-package flycheck
    :hook
    ((ruby-mode . flycheck-mode)))

  (use-package fill-column-indicator
    :init
    (defun my-hook-ruby-mode--fci ()
      (setq fill-column 80)
      (fci-mode 1))
    :hook
    ((ruby-mode . my-hook-ruby-mode--fci)))

  ;; Emacs 24.4 用設定
  ;; ruby-mode-set-encoding の実装と auto-save-buffers の相性が悪いらしいので
  ;; その対処。ruby-insert-encoding-magic-comment を nil にしただけでは解決し
  ;; ない
  (when (version<= "24.4" emacs-version)
    (defun ruby-mode-set-encoding ()))

  ;; inf-ruby
  (use-package inf-ruby
    :hook
    ((ruby-mode . inf-ruby-minor-mode)))

  ;; ruby-block
  ;;  キーワード end に対応する行をハイライトする
  (use-package ruby-block
    :config
    ;; これを行っていないと ruby-mode 以外でも ruby-block minor-mode が有効に
    ;; なってしまう
    (setq-default ruby-block-mode nil)
    ;; end に対応する行をミニバッファに表示し、かつオーバレイする
    (setq ruby-block-highlight-toggle t))


  (use-package mod-popwin
    :config
    (add-popwin-special-display-config
     '(inf-ruby-mode :height 0.45 :position bottom :stick t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-ruby-mode)
