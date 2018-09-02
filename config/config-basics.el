;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 基本設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ロードパスの設定

(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(add-to-list 'load-path (concat user-emacs-directory "elisp/orig"))
(add-to-list 'load-path (concat user-emacs-directory "elisp/apel"))
(add-to-list 'load-path (concat user-emacs-directory "elisp/flim"))
(add-to-list 'load-path (concat user-emacs-directory "elisp/semi"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 基本情報設定

;; (set-language-environment "Japanese")
(set-locale-environment)

;; (setq user-name "hiroki")
(setq user-full-name "MOTOYAMA Hiroki")
(setq user-mail-address "mtymhrk@zf7.so-net.ne.jp")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 基本的なキーバインドの変更

;;; C-h を一文字削除、Backspace をヘルプのプレフィクスにする
(keyboard-translate ?\C-h ?\C-?)
(keyboard-translate ?\C-? ?\C-h)
;;; 上の記述は terminal 内の emacs では効くが X 上の emacs では効かない。そ
;;; のため下の設定も行う
(global-set-key (kbd "<backspace>") 'help-command)

;;; M-h を一単語削除にする
;; (global-set-key (kbd "M-h") 'backward-kill-word)

;;; インクリメンタルサーチでバックスペースを有効にする
;; (define-key isearch-mode-map "\C-h" 'isearch-delete-char)

;;; C-x C-o、Ctrl+TAB でウィンドウを移動
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-<tab>") 'other-window)

;;; C-c u でカーソルにある URL をブラウザで開く
(global-set-key "\C-cu" 'browse-url-at-point)

;;; ミニバッファ内で C-w で単語削除
(define-key minibuffer-local-completion-map "\C-w" 'backward-kill-word)

;;; バッファリストをカレントバッファに表示
(global-set-key "\C-x\C-b" 'buffer-menu)

;;; keyboard-escape-quit をより押しやすい位置に
;; (global-set-key (kbd "C-M-g") 'keyboard-escape-quit)

;;; M-: (eval-expression) にて tab で補完を行う
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

;;; just-on-space の代りに cycle-spacing を使う
(global-set-key (kbd "M-SPC") 'cycle-spacing)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 基本設定

;;; 色付けを有効化
(global-font-lock-mode t)

;;; *scratch* バッファを emacs-lisp-mode にする
(setq initial-major-mode 'emacs-lisp-mode)

;;; スタートアップメッセージ表示しない
(setq inhibit-startup-message t)

;;; y or n
(fset 'yes-or-no-p 'y-or-n-p)

;;; enamcs -nw ではメニューバーを非表示
(if window-system (menu-bar-mode 1) (menu-bar-mode 0))

;;; ツールバーを非表示
(tool-bar-mode -1)

;;; スクロールバーを非表示
(scroll-bar-mode -1)

;;; バッファの境界を fringe に表示
(setq-default   indicate-buffer-boundaries   t)
(modify-frame-parameters   nil   '((left-fringe)))

;;; 音を鳴らさない
(setq ring-bell-function (lambda ()))

;;; マウスホイールによるスクロールを有効化
(mouse-wheel-mode t)
(setq mouse-wheel-follow-mouse t)

;;; auto-fill を有効にする。デフォルトは 76 文字で折り返し
(auto-fill-mode t)
(setq-default fill-column 76)

;;; バッファ末尾に改行コードを追加しない
(setq next-line-add-newlines nil)

;;; ffap を有効にする
;; (ffap-bindings)

;;; マーク範囲に色をつける
(transient-mark-mode t)

;;; 対応するカッコを強調表示
(show-paren-mode t)

;;; don't create backup file
(setq backup-inhibited t)

;;; iswitchb を有効にする
(when (version< emacs-version "24.4")
  (iswitchb-mode 1))

;;; カーソルを点滅させない
(blink-cursor-mode 0)

;;; カーソルの行と列の位置を表示
(line-number-mode t)
(column-number-mode t)

;;; 画像ファイルを表示する
(auto-image-file-mode t)

;;; インデントにタブを使わない
(setq-default indent-tabs-mode nil)

;;; yank 時にリージョンを x window の clipboard に送る
(setq x-select-enable-clipboard t)

;;; GC を減らして軽くする(デフォルトの 10 倍)
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;;; ログの記録行数を増やす
(setq message-log-max 10000)

;;; ミニバッファを再帰的に呼び出せるようにする
(setq enable-recursive-minibuffers t)

;;; ダイアログボックスを使わないようにする
(setq use-dialog-box nil)
(defalias 'message-box 'message)

;;; 履歴をたくさん保存する
(setq history-length 1000)

;;; キーストロークをエコーエリアに早く表示する
(setq echo-keystrokes 0.1)

;;; 現在位置の関数名を表示する
(which-func-mode 1)
;;; 全てのメジャーモードに対して which-func-mode を適用する
(setq which-func-modes t)
;; ;;; 以下 2 行は画面上部に表示する設定
;; (delete (assoc 'which-func-mode mode-line-format) mode-line-format)
;; (setq-default header-line-format '(which-func-mode ("" which-func-format)))

;;; ミニバッファで入力を取り消しても履歴に残す
;;; 誤って取り消して入力が失われるのを防ぐため
(defadvice abort-recursive-edit (before minibuffer-save activate)
  (when (eq (selected-window) (active-minibuffer-window))
    (add-to-history minibuffer-history-variable (minibuffer-contents))))

;;; uniquify の設定
;;;   同じ名前のファイルを開いたときにバッファ名にディレクトリ名を追加する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; version 24.4 の場合、cua-mode を使用せず、ractangle-mark-mode を使用する
(cond ((version<= "24.4" emacs-version)
       (cua-mode -1)
       (global-set-key (kbd "<C-return>") 'rectangle-mark-mode))
      (t
       (cua-mode 1)
       (setq cua-enable-cua-keys nil))) ;; 変なキーバインド禁止

;;; 圧縮されたファイルを直接編集可能にする
(auto-compression-mode t)

;;; grep に lgrep を使用する
;; (when (file-exists-p "/usr/bin/lgrep")
;;   (setq grep-command "lgrep -n "))

;;; windmove の設定
;; (windmove-default-keybindings)
;; (setq windmove-wrap-around t)
;; (global-set-key (kbd "M-K") 'windmove-up)
;; (global-set-key (kbd "M-J") 'windmove-down)
;; (global-set-key (kbd "M-L") 'windmove-right)
;; (global-set-key (kbd "M-H") 'windmove-left)

;;; インクリメンタルサーチの一致部分をすぐにハイライトする
(setq lazy-highlight-initial-delay 0)

;;; 設定ファイルに色付けする
(require 'generic-x)

;;; 開いているファイルが Emacs 外部から変更された場合に自動的にファイルを読み
;;; 込む
(global-auto-revert-mode)

;;; electric-indent-mode を無効にする
(electric-indent-mode -1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 

(defvar my-orig-C-M-SPC-command (global-key-binding (kbd "C-M-SPC")))

(defvar keymap-ctrl-meta-space (make-sparse-keymap))
(global-set-key (kbd "C-M-SPC") keymap-ctrl-meta-space)
(global-set-key (kbd "C-;") keymap-ctrl-meta-space)

(defvar keymap-for-manuals (make-sparse-keymap))
(define-key keymap-ctrl-meta-space (kbd "d") keymap-for-manuals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 雑多なコマンド定義

;;; カーソル上の文字のフェイスを表示するコマンド
(defun describe-face-at-point ()
 (interactive)
 (message "%s" (get-char-property (point) 'face)))

;;; ~/.emacs.d/elisp 配下の elisp をバイトコンパイルするコマンド
(defun my-byte-recompile-elisp-directory ()
  (interactive)
  (byte-recompile-directory (concat user-emacs-directory "elisp") 0 t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; etc

;; kill-region と kill-ring-save の拡張
(require 'ex-kill-region)

;; *scratch* バッファを永続化
(require 'permanent-scratch)

;; isearch の挙動変更
(require 'mod-isearch)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


