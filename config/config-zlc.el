;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  zlc.el
;;;    http://d.hatena.ne.jp/mooz/20101003/p1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Emacs のミニバッファ補完を zsh ライクにする

;;; (auto-install-from-url
;;;  "https://raw.github.com/mooz/emacs-zlc/master/zlc.el")

(require 'zlc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 「/」 入力でディレクトリの補完の確定を行うコマンドを定義
(defun zlc--my-extract-path (str)
  (cond
   ((string-match "Find file\\( or URL\\)?: \\(.*\\)" str)
    (substring str (match-beginning 2) (match-end 2)))
   (t nil)))

(defun zlc--my-slash-terminated-p (path)
  (string-match-p "/$" path))

(defun zlc-my-fix-selection-* ()
  (interactive)
  (let ((path (zlc--my-extract-path (buffer-string)))
        (selecting-p (or (eq last-command 'minibuffer-complete)
                         (eq last-command 'zlc-select-previous)
                         (eq last-command 'zlc-select-previous-vertical)
                         (eq last-command 'zlc-select-next)
                         (eq last-command 'zlc-select-next-vertical))))
    (unless (and selecting-p
                 path
                 (zlc--my-slash-terminated-p path)
                 (file-directory-p path))
      (insert (this-command-keys)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; キーバインド
(let ((map minibuffer-local-map))
  ;;; like menu select
  (define-key map (kbd "<down>")  'zlc-select-next-vertical)
  (define-key map (kbd "<up>")    'zlc-select-previous-vertical)
  (define-key map (kbd "<right>") 'zlc-select-next)
  (define-key map (kbd "<left>")  'zlc-select-previous)

  (define-key map (kbd "M-J")  'zlc-select-next-vertical)
  (define-key map (kbd "M-K")    'zlc-select-previous-vertical)
  (define-key map (kbd "M-L") 'zlc-select-next)
  (define-key map (kbd "M-H")  'zlc-select-previous)

  ;;; reset selection
  (define-key map (kbd "M-c") 'zlc-reset)

  ;;; / でディレクトリの補完を確定
  (define-key map (kbd "/") 'zlc-my-fix-selection-*)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zlc を有効化
(zlc-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-zlc)
