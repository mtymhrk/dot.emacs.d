;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ruby-mode
;;;   ELPA: Marmalade
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ruby-mode)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;;; Emacs 24.4 用設定
;;; ruby-mode-set-encoding の実装と auto-save-buffers の相性が悪いらしいので
;;; その対処。ruby-insert-encoding-magic-comment を nil にしただけでは解決し
;;; ない
(when (version<= "24.4" emacs-version)
  (defun ruby-mode-set-encoding ()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; inf-ruby
;;;   ELPA: Marmalade
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'inf-ruby)

(defun ruby-mode-hook--inf-ruby ()
  (inf-ruby-minor-mode))

(add-hook 'ruby-mode-hook 'ruby-mode-hook--inf-ruby)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ruby-electric
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'ruby-electric)

;; ;;; ruby-electric-mode 時に " や ( を自動で閉じないよう設定
;; (setq ruby-electric-expand-delimiters-list nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ruby-end
;;;   ELPA: Marmalade
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 対応する end を自動挿入する elisp

;;; 2015/10/18: SmartParens 導入で不要になったのでマイナーモードを無効化
(require 'ruby-end)
(remove-hook 'ruby-mode-hook 'ruby-end-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  ruby-block
;;;    http://d.hatena.ne.jp/khiker/20071130/emacs_ruby_block
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    キーワード end に対応する行をハイライトする
;;;    (auto-install-from-emacswiki "ruby-block.el")

(require 'ruby-block)

;;; これを行っていないと ruby-mode 以外でも ruby-block minor-mode が有効に
;;; なってしまう
(setq-default ruby-block-mode nil)

;;; end に対応する行をミニバッファに表示し、かつオーバレイする
(setq ruby-block-highlight-toggle t)

(defun ruby-mode-hook--ruby-block ()
  (ruby-block-mode t))

(add-hook 'ruby-mode-hook 'ruby-mode-hook--ruby-block)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; electric-*-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'ruby-mode-hook
;;           '(lambda ()
;;              (electric-pair-mode t)
;;              (electric-indent-mode t)
;;              (electric-layout-mode t)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;  ri-emacs
;; ;;    http://rubyforge.org/projects/ri-emacs/
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq ri-ruby-script "/home/hiroki/lib/emacs/site-lisp/ri-emacs.rb")
;; (autoload 'ri "/home/hiroki/lib/emacs/site-lisp/ri-ruby.elc" nil t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake for Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 2014/09/17: flycheck へ移行

;; (eval-after-load 'config-flymake
;;   '(progn
;;      (defun flymake-ruby-init ()
;;        (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;                             'flymake-create-temp-inplace))
;;               (local-file  (file-relative-name
;;                             temp-file
;;                             (file-name-directory buffer-file-name))))
;;          (list "ruby" (list "-c" local-file))))

;;      (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
;;      (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
;;      (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)
;;            flymake-err-line-patterns)


;;      (defun ruby-mode-hook--flymake ()
;;        ;; Don't want flymake mode for ruby regions in rhtml files
;;        (if (not (null buffer-file-name)) (flymake-mode)))

;;      (add-hook 'ruby-mode-hook 'ruby-mode-hook--flymake)
;;      ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake for Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load 'config-flycheck
  '(progn
     (defun ruby-mode-hook--flycheck ()
       (flycheck-mode t))

     (add-hook 'ruby-mode-hook 'ruby-mode-hook--flycheck)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-complete for Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load 'config-auto-complete
  '(progn
     ;;; ruby-mode 時の補完候補に yasnippet を加える
     (defun ruby-mode-hook--auto-complete ()
       (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))

     (add-hook 'ruby-mode-hook 'ruby-mode-hook--auto-complete)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; popwin for Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load 'config-popwin
  '(progn
     ;;; run-ruby で起動した irb のバッファをポップアップで表示させる
     (add-popwin-special-display-config
      '(inf-ruby-mode :height 0.45 :position bottom :stick t))
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1 行の文字数上限を越えた場合にハイライトする
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'highlight-column-limit)

(hcl:add-hook ruby-mode-hook 80)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-ruby-mode)
