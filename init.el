;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs -q -lした時に、user-emacs-directoryが変わるように
(prog1 "user-emacs-directory"
  (when load-file-name
    (setq user-emacs-directory (file-name-directory load-file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package 設定

(prog1 "package"
  ;; elisp のインストール場所
  (setq package-user-dir (concat user-emacs-directory "elpa"))

  ;; package 初期化
  (package-initialize)

  ;; リポジトリ MELPA を追加
  (add-to-list 'package-archives
               '("melpa stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t)

  ;; melpa stable を優先
  (setq package-archive-priorities
        '(("melpa stable" . 10)
          ("gnu"          . 5)
          ("melpa"        . 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; leaf のインストールと設定、bind-key のインストール

(prog1 "leaf"
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf)))

(leaf leaf-keywords
  :ensure t
  :config
  ;; leaf の hydra サポートを有効にする
  (leaf hydra :ensure t)
  (leaf-keywords-init))

(leaf bind-key
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 基礎設定
(leaf *basics
  :config

  ;; ロードパス
  (leaf *load-path
    :config
    (add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
    (add-to-list 'load-path (expand-file-name "elisp/orig" user-emacs-directory)))

  ;; ロケール
  (leaf *locale
    :config
    (set-locale-environment))

  ;; 個人設定
  (leaf *personal
    :config
    (setq user-full-name "MOTOYAMA Hiroki")
    (setq user-mail-address "mtymhrk@zf7.so-net.ne.jp"))

  ;; キーバイド
  (leaf *keybinding
    :config
    ;; C-h を一文字削除、Backspace をヘルプのプレフィクスにする
    (keyboard-translate ?\C-h ?\C-?)
    (keyboard-translate ?\C-? ?\C-h)

    ;; 上の記述は terminal 内の emacs では効くが X 上の emacs では効かない。そ
    ;; のため下の設定も行う
    (bind-key "<backspace>" 'help-command)

    ;; C-x C-o、Ctrl+TAB でウィンドウを移動
    (bind-key "C-x C-o" 'other-window)
    (bind-key "C-<tab>" 'other-window)

    ;; C-c u でカーソルにある URL をブラウザで開く
    (bind-key "C-c u" 'browse-url-at-point)

    ;; ミニバッファ内で C-w で単語削除
    (bind-key "C-w" 'backward-kill-word minibuffer-local-completion-map)

    ;; バッファリストをカレントバッファに表示
    (bind-key "C-x C-b" 'buffer-menu)

    ;; keyboard-escape-quit をより押しやすい位置に
    ;; (bind-key "C-M-g" 'keyboard-escape-quit)

    ;; M-: (eval-expression) にて tab で補完を行う
    (bind-key "TAB" 'lisp-complete-symbol read-expression-map)

    ;; just-on-space の代りに cycle-spacing を使う
    (bind-key "M-SPC" 'cycle-spacing)

    ;; C-Return で rectanble-mark-mode
    (bind-key "<C-return>" 'rectangle-mark-mode))

  ;; 見た目
  (leaf *appearance
    :config
    ;; 色付けを有効化
    (global-font-lock-mode t)

    ;; enamcs -nw ではメニューバーを非表示
    (if window-system
        (menu-bar-mode 1)
      (menu-bar-mode 0))

    ;; ツールバーを非表示
    (tool-bar-mode -1)

    ;; スクロールバーを非表示
    (scroll-bar-mode -1)

    ;; バッファの境界を fringe に表示
    (setq-default indicate-buffer-boundaries t)
    (modify-frame-parameters nil '((left-fringe)))

    ;; カーソルを点滅させない
    (blink-cursor-mode 0)

    ;; カーソルの行と列の位置を表示
    (line-number-mode t)
    (column-number-mode t))

  ;; その他設定
  (leaf *etc
    :config
    ;; *scratch* バッファを emacs-lisp-mode にする
    (setq initial-major-mode 'emacs-lisp-mode)

    ;; スタートアップメッセージ表示しない
    (setq inhibit-startup-message t)

    ;; yes or no を y or n に
    (fset 'yes-or-no-p 'y-or-n-p)

    ;; マウスホイールによるスクロールを有効化
    (mouse-wheel-mode t)
    (setq mouse-wheel-follow-mouse t)

    ;; auto-fill を有効にする。デフォルトは 76 文字で折り返し
    (auto-fill-mode t)
    (setq-default fill-column 76)

    ;; バッファ末尾に改行コードを追加しない
    (setq next-line-add-newlines nil)

    ;; ffap を有効にする
    ;; (ffap-bindings)

    ;; マーク範囲に色をつける
    (transient-mark-mode t)

    ;; 対応するカッコを強調表示
    (show-paren-mode t)

    ;; don't create backup file
    (setq backup-inhibited t)

    ;; 画像ファイルを表示する
    (auto-image-file-mode t)

    ;; インデントにタブを使わない
    (setq-default indent-tabs-mode nil)

    ;; yank 時にリージョンを x window の clipboard に送る
    (setq x-select-enable-clipboard t)

    ;; GC を減らして軽くする(デフォルトの 10 倍)
    (setq gc-cons-threshold (* 10 gc-cons-threshold))

    ;; ログの記録行数を増やす
    (setq message-log-max 10000)

    ;; ミニバッファを再帰的に呼び出せるようにする
    (setq enable-recursive-minibuffers t)

    ;; ダイアログボックスを使わないようにする
    (setq use-dialog-box nil)
    (defalias 'message-box 'message)

    ;; 履歴をたくさん保存する
    (setq history-length 1000)

    ;; キーストロークをエコーエリアに早く表示する
    (setq echo-keystrokes 0.1)

    ;; 現在位置の関数名を表示する
    (which-func-mode 1)
    ;; 全てのメジャーモードに対して which-func-mode を適用する
    (setq which-func-modes t)

    ;; ミニバッファで入力を取り消しても履歴に残す
    ;; 誤って取り消して入力が失われるのを防ぐため
    (defun my:minibuffer-save-history (&rest args)
        (when (eq (selected-window) (active-minibuffer-window))
          (add-to-history minibuffer-history-variable (minibuffer-contents))))
    (advice-add 'abort-recursive-edit :before #'my:minibuffer-save-history)

    ;; 圧縮されたファイルを直接編集可能にする
    (auto-compression-mode t)

    ;; インクリメンタルサーチの一致部分をすぐにハイライトする
    (setq lazy-highlight-initial-delay 0)

    ;; 設定ファイルに色付けする
    (require 'generic-x)

    ;; 開いているファイルが Emacs 外部から変更された場合に自動的にファイルを読み
    ;; 込む
    (global-auto-revert-mode)

    ;; electric-indent-mode を無効にする
    (electric-indent-mode -1)

    ;; bell を無効化 (visible-bel も使わない)
    (setq ring-bell-function #'(lambda ())))

  (leaf uniquify
    :require t
    :config
    ;; 同じ名前のファイルを開いたときにバッファ名にディレクトリ名を追加する
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 独自キーマップ
(leaf *my:keymaps
  :config
  (defvar my:orig-C-M-SPC-command (global-key-binding (kbd "C-M-SPC")))

  (defvar keymap-ctrl-meta-space (make-sparse-keymap))
  (global-set-key (kbd "C-M-SPC") keymap-ctrl-meta-space)
  (global-set-key (kbd "C-;") keymap-ctrl-meta-space)

  (defvar keymap-for-manuals (make-sparse-keymap))
  (define-key keymap-ctrl-meta-space (kbd "d") keymap-for-manuals)

  (defvar keymap-for-code-navigation (make-sparse-keymap))
  (define-key keymap-ctrl-meta-space (kbd "c") keymap-for-code-navigation)

  (defvar keymap-for-grep (make-sparse-keymap))
  (define-key keymap-ctrl-meta-space (kbd "g") keymap-for-grep)

  (provide 'my:keymaps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 雑多なコマンド定義
(leaf *my:commands
  :config
  ;; カーソル上の文字のフェイスを表示するコマンド
  (defun describe-face-at-point ()
    (interactive)
    (message "%s" (get-char-property (point) 'face)))

  ;; ~/.emacs.d/elisp 配下の elisp をバイトコンパイルするコマンド
  (defun my:byte-recompile-elisp-directory ()
    (interactive)
    (byte-recompile-directory (concat user-emacs-directory "elisp") 0 t))

  ;; package の elisp を再コンパイルするコマンド
  (defun my:package-recompile (pkg)
    (unless (package-installed-p pkg)
      (error "failed to recompile a package: uninstalled package: %s" pkg))
    (when (package-built-in-p pkg)
      (error "failed to recompile a package: built-in pacakge: %s" pkg))
    (let ((desc (cadr (assq pkg package-alist))))
      ;; async パッケージが導入されていると (helm をインストールすると自動的に
      ;; インストールされる) デフォルトで helm が非同期にコンパイルされるよう
      ;; になるが、うまく動かないので async byte compile を抑制してコンパイル
      ;; を実行する
      (let ((async-bytecomp-allowed-packages nil))
        (package--compile desc))
      ))

  ;; 全ての package の elisp を再コンパイルするコマンド
  (defun my:package-recompile-all ()
    (dolist (pkg package-alist)
      (let* ((name (package-desc-name (cadr pkg))))
        (unless (package-built-in-p name)
          (my:package-recompile name)))))

  ;; リージョンが有効でない場合に kill-region を実行した場合は後ろの単語をkillす
  ;; る
  (defun my:advice-kill-region (orig-fun &rest args)
    (if (and (called-interactively-p 'any)
             (not (region-active-p)))
        (backward-kill-word 1)
      (apply orig-fun args)))
  (advice-add 'kill-region :around 'my:advice-kill-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; その他基本機能の挙動変更

;; kill-region と kill-ring-save の拡張
;; (leaf ex-kill-region
;;   :require t)

;; *scratch* バッファを永続化
(leaf permanent-scratch
  :require t)

;; isearch の挙動変更
(leaf mod-isearch
  :require t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(leaf *frame-and-font
  :when window-system
  :config
  (defvar my:default-frame-font nil)
  (defvar my:default-frame-height nil)
  (defvar my:default-frame-width nil)
  (cond
   ((string-match "spinel" (system-name))
    (create-fontset-from-ascii-font "MyricaM M-10.5" nil "my:fontset")
    (set-fontset-font "fontset-my:fontset" 'unicode "MyricaM M-10.5" nil 'append)
    (setq my:default-frame-font "fontset-my:fontset")
    (setq my:default-frame-height 62)
    (setq my:default-frame-width 246))
   ((string-match "amber" (system-name))
    (create-fontset-from-ascii-font "MyricaM M-10.5" nil "my:fontset")
    (set-fontset-font "fontset-my:fontset" 'unicode "MyricaM M-10.5" nil 'append)
    (setq my:default-frame-font "fontset-my:fontset")
    (setq my:default-frame-height 43)
    (setq my:default-frame-width 163))
   (t
    (create-fontset-from-ascii-font "MyricaM M-10.5" nil "my:fontset")
    (set-fontset-font "fontset-my:fontset" 'unicode "MyricaM M-10.5" nil 'append)
    (setq my:default-frame-font "fontset-my:fontset")
    (setq my:default-frame-height 65)
    (setq my:default-frame-width 80))) ;; 1 Window 設定

  (set-frame-font my:default-frame-font)
  (push `(font . ,my:default-frame-font) initial-frame-alist)
  (push `(height . ,my:default-frame-height) initial-frame-alist)
  (push `(width . ,my:default-frame-width) initial-frame-alist)
  (setq default-frame-alist initial-frame-alist)

  (leaf all-the-icons
    :init
    ;; font-lock+ を require しないと all-the-icons が使えない
    (leaf font-lock+
      :require t)
    :require t)

  (leaf *maximize-frame-and-split-window-at-start
    :config
    ;; ウィンドウを等分割するコマンド
    (defun my:split-window-horizontally-n (n)
      (interactive "nNumber of Windows: ")
      (dotimes (i (- n 1))
        (split-window-horizontally))
      (balance-windows))

    (defun my:default-window-split ()
      (when (and window-system (one-window-p))
        (cond
         ((>= (frame-width) 184)
          (my:split-window-horizontally-n 2))
         (t
          ))))

    ;; 起動時点でフレームを最大化し、ウィンドウを分割する
    (defun my:after-init-hook--setup-for-frame-and-window ()
      (modify-frame-parameters (selected-frame) initial-frame-alist)
      (toggle-frame-maximized)
      ;; フレームが最大化されるのを待つ
      ;; (toggle-frame-maximaized 直後にフレームを分割すると等分割されないため)
      (sleep-for 0.5)
      (my:default-window-split))

    (add-hook 'after-init-hook 'my:after-init-hook--setup-for-frame-and-window)))

(leaf *theme
  :config
  (leaf doom-themes
    :ensure t
    :require t
    :config
    (load-theme 'doom-dracula t))

  (leaf doom-modeline
    :ensure t
    :custom
    ((doom-modeline-buffer-file-name-style . 'truncate-with-project))
    :config
    ;; doom-modeline が eldoc-in-minibuffer-mode を有効にするが、不要なのと、これ
    ;; を有効にすると eval-expression で C-h が効かなくなるで無効にする
    (when eldoc-in-minibuffer-mode
      (eldoc-in-minibuffer-mode -1))
    :hook
    ((after-init-hook . doom-modeline-init))))

(leaf delight
  :ensure t
  :config
  ;; delight で major-mode のモード名表示変更が有効にならない対処
  (setq inhibit-mode-name-delight nil))

(leaf exec-path-from-shell
  :ensure t
  :custom
  ((exec-path-from-shell-variables . '("PATH" "MANPATH" "INFOPATH")))
  :config
  ;; NTEmacs 用設定
  (when (eq system-type 'windows-nt)
    (defvar my:cygwin-bin-path
      "D:/lib/gnupack_devel-13.01-2015.05.10/app/cygwin/cygwin/bin/")
    (setq shell-file-name (concat my:cygwin-bin-path "bash"))
    (setenv "SHELL" shell-file-name)

    ;; 環境変数 PATH のときのみパスの変換を実行
    (defun ad-exec-path-from-shell-setenv (orig-fun &rest args)
      (when (string=  (car args) "PATH")
        (setf (nth 1 args)
              (with-temp-buffer
                (call-process (concat my:cygwin-bin-path "cygpath")
                              nil '(t nil) nil "-amp" (nth 1 args))
                (unless (bobp)
                  (goto-char (point-min))
                  (buffer-substring-no-properties (point)
                                                  (line-end-position))))))
      (apply orig-fun args))
    (advice-add 'exec-path-from-shell-setenv
                :around 'ad-exec-path-from-shell-setenv))

  (exec-path-from-shell-initialize))

(leaf eldoc
  :require t
  :delight)

(leaf whitespace
  :delight whitespace
  :delight global-whitespace-mode
  :require t
  :config
    ;; whitespace-mode の対象を trailing blank とタブとスペースに設定。
  ;; face による可視化機能を有効化
  (setq whitespace-style '(face trailing tabs tab-mark spaces space-mark))

  ;;; 対象となるスペースを全角スペースに限定
  (setq whitespace-space-regexp "\\(　+\\)")

  ;;; 全角スペースとタブを他も文字で表示
  (setq whitespace-display-mappings
        '((space-mark ?　 [?□])
          ;; WARNING: the mapping below has a problem.
          ;; When a TAB occupies exactly one column, it will display the
          ;; character ?\xBB at that column followed by a TAB which goes to
          ;; the next TAB column.
          ;; If this is a problem for you, please, comment the line below.
          ;; (tab-mark ?\t [?\xBB ?\t] [?\\ ?\t])
          ))

  (set-face-attribute 'whitespace-trailing nil
                      :foreground "purple"
                      :background 'unspecified
                      :strike-through nil
                      :underline t)

  (set-face-attribute 'whitespace-space nil
                      :foreground "purple"
                      :background 'unspecified
                      :strike-through nil
                      :underline nil)

  (set-face-attribute 'whitespace-tab nil
                      :foreground "purple"
                      :background 'unspecified
                      :strike-through nil
                      :underline t)

  (setq whitespace-global-modes '(not dired-mode))
  (global-whitespace-mode 1))

(leaf *move-error
  :hydra (hydra-move-error
          (global-map "M-g")
          ("n" next-error "next")
          ("p" previous-error "previous")
          ("C-n" next-error "next")
          ("C-p" previous-error "previous")))

(leaf *window
  :config
  (leaf switch-window
    :ensure t
    :custom
    (switch-window-shortcut-style . 'qwerty)
    (switch-window-qwerty-shortcuts . '("j" "k" "l" ";" "i" "o" "a" "s" "d" "f" "w" "e"))
    :bind
    ("C-<tab>" . switch-window)
    ("<C-S-iso-lefttab>" . switch-window-then-delete)
    (:switch-window-extra-map
     ("C-k" . switch-window-mvborder-up)
     ("C-j" . switch-window-mvborder-down)
     ("C-h" . switch-window-mvborder-left)
     ("C-l" . switch-window-mvborder-right)
     ("C-b" . balance-windows))
    :config
    (leaf mod-switch-window
      :require t)

    (set-face-attribute 'switch-window-label nil :height 10.0)
    (cl-loop for key in switch-window-qwerty-shortcuts
             do (bind-key key nil switch-window-extra-map)))

  (leaf eyebrowse
    :ensure t
    :require t
    :custom
    ;; eyebrowse-mode-map にキーをバインドしない
    (eyebrowse-keymap-prefix . "")
    (eyebrowse-new-workspace . #'my-eyebrowse-new-workspace-func)
    ;; doom-mode-line では別途 Window Config 番号が表示されるので eyebrowse での表示は削除
    (eyebrowse-mode-line-style . 'hide)

    :config
    (defun my:eyebrowse-new-workspace-func ()
      (switch-to-buffer "*scratch*")
      (my:default-window-split))
    (eyebrowse-mode 1))

  (leaf popwin
    :ensure t
    :require t
    :config
    (leaf mod-popwin :require t)
    ;; ポップアップウィンドウの高さ (フレームに対する割合で指定)
    (setq popwin:popup-window-height 0.4)

    ;; 補完バッファをポップアップで表示
    (mod-popwin:add-display-config '(compilation-mode :noselect t :stick t))

    ;; occur バッファをポップアップで表示
    ;; (:noselect t とすると、検索元のバッファが read-only になってしまうので
    ;;  noselect は有効にはせず、advice 設定で noselect 化する)
    (mod-popwin:add-display-config '(occur-mode :stick t))

    (defun my:popwin-noselect (&rest args)
      (when (and popwin-mode popwin:selected-window)
        (select-window popwin:selected-window)))
    (advice-add 'occur :after #'my:popwin-noselect)

    ;; grep の結果をポップアップで表示
    (mod-popwin:add-display-config '(grep-mode :noselect t :stick t))

    ;; backtrace をポップアップで表示
    (mod-popwin:add-display-config '("*Backtrace*" :noselect t :stick t))

    ;; メッセージバッファをポップアップで表示
    (mod-popwin:add-display-config '("*Messages*" :noselect t :stick t))

    ;; shell コマンドの output をポップアップで表示
    (mod-popwin:add-display-config '("*Shell Command Output*" :noselect t :stick t))

    ;; apropos をポップアップで表示
    (mod-popwin:add-display-config '(apropos-mode :noselect nil :stick t))

    (popwin-mode 1))

  (leaf winner
    :require t
    :config
    (winner-mode))

  (leaf *keybindings
    :hydra
    (hydra-popwin
     (:hint nil)
"
^Window^           ^Scroll^          ^isearch^            ^Quit^
^^^^^^^^-----------------------------------------------------------------
_p_: repopup       _j_  : up         _C-s_: forward       _ESC_: Window
_c_: close         _k_  : down       _C-r_: backward      _q_  : quit
_o_: select        _h_  : right
^ ^                _l_  : left
^ ^                _C-a_: home
^ ^                _C-e_: end
"
      ("p"          mod-popwin:repopup-window)
      ("c"          popwin:close-popup-window)
      ("o"          popwin:select-popup-window)
      ("j"          mod-popwin:scroll-up)
      ("k"          mod-popwin:scroll-down)
      ("h"          mod-popwin:scroll-right)
      ("l"          mod-popwin:scroll-left)
      ("C-v"        mod-popwin:scroll-up-command)
      ("M-v"        mod-popwin:scroll-down-command)
      ("M-<"        mod-popwin:beginning-of-buffer)
      ("M->"        mod-popwin:end-of-buffer)
      ("C-s"        mod-popwin:isearch-forward)
      ("C-r"        mod-popwin:isearch-backward)
      ("C-a"        mod-popwin:seq-home)
      ("M-m"        mod-popwin:seq-home2)
      ("C-e"        mod-popwin:seq-end)
      ("ESC"        hydra-window/body :exit t)
      ("q"          nil))
    :hydra
    (hydra-eyebrowse
     (:hint nil)
"
^Create^           ^Swit^           ^Quit^
^^^^^^^^-------------------------------------------------------
_c_  : create      _p_  : prev       _ESC_: Window
_k_  : close       _n_  : next       _q_  : quit
^ ^                _'_  : last
^ ^                _._  : select
^ ^                0~9  : switch
"
      ("c" eyebrowse-create-window-config :exit t)
      ("k" eyebrowse-close-window-config)
      ("p" eyebrowse-prev-window-config)
      ("n" eyebrowse-next-window-config)
      ("'" eyebrowse-last-window-config)
      ("." eyebrowse-switch-to-window-config)
      ("0" eyebrowse-switch-to-window-config-0)
      ("1" eyebrowse-switch-to-window-config-1)
      ("2" eyebrowse-switch-to-window-config-2)
      ("3" eyebrowse-switch-to-window-config-3)
      ("4" eyebrowse-switch-to-window-config-4)
      ("5" eyebrowse-switch-to-window-config-5)
      ("6" eyebrowse-switch-to-window-config-6)
      ("7" eyebrowse-switch-to-window-config-7)
      ("8" eyebrowse-switch-to-window-config-8)
      ("9" eyebrowse-switch-to-window-config-9)
      ("ESC" hydra-window/body :exit t)
      ("q" nil :exit t))
    :hydra
    (hydra-window
     (:hint nil)
"
^Division^         ^eyebrowse^          ^Popwin^          ^Undo^                   ^Quit^
^^^^^^^^-----------------------------------------------------------------------------------------
_h_: horizon       _c_: create          _p_: popwin       _z_  : winner-undo       _q_: quit
_v_: vertical      _k_: close           ^ ^               _C-z_: winner-redo
_d_: delete        _._: switch
_s_: switch        _e_: eyebrowse
"
      ("h" split-window-below)
      ("v" split-window-right)
      ("d" switch-window-then-delete)
      ("s" switch-window)
      ("c" eyebrowse-create-window-config :exit t)
      ("k" hydra-eyebrowse/eyebrowse-close-window-config :exit t)
      ("." hydra-eyebrowse/eyebrowse-switch-to-window-config :exit t)
      ("e" hydra-eyebrowse/body :exit t)
      ("p" hydra-popwin/body :exit t)
      ("z" winner-undo)
      ("C-z" winner-redo)
      ("q" nil :exit t))
    :config
    (bind-key "w" 'hydra-window/body keymap-ctrl-meta-space)))

(leaf recentf
  :require t
  :config
  (setq recentf-max-saved-items 1000)
  (setq recentf-exclude '("/TAGS$" "/var/tmp/"))

  (leaf recentf-ext
    :ensure t
    :require t))

(leaf compile
  :leaf-defer nil
  :config
  (setq compile-command "make")
  (setq compile-history (list "make" "make clean"))

  ;; compilation-mode で ansi color が化けてしまうことへの対処
  (add-hook 'compilation-mode-hook #'ansi-color-for-comint-mode-on)
  (add-hook 'compilation-filter
            #'(lambda ()
                (let ((start-marker (make-marker))
                      (end-marker (process-mark (get-buffer-process
                                                 (current-buffer)))))
                  (set-marker start-marker (point-min))
                  (ansi-color-apply-on-region start-marker end-marker))))

  ;; コンパイルプロセスの出力を追ってコンパイルバッファをスクロースする
  (setq compilation-scroll-output t)

  :bind
  (mode-specific-map
   ;; C-c c で compile コマンドを呼び出す
   ("c" . compile)))

(leaf flymake
  :delight
  :require t
  :config
  ;; flymake の行表示の色を設定
  (set-face-attribute 'flymake-errline nil
                      :background nil
                      :foreground nil
                      :underline '(:color "Red1" :style wave)
                      :inherit nil)

  (set-face-attribute 'flymake-warnline nil
                      :background nil
                      :foreground nil
                      :underline '(:color "DarkOrange" :style wave)
                      :inherit nil)

  ;; gcc の日本語警告メッセージを警告と判別できるようにする
  (setq flymake-warning-re "^\\([wW]arning\\|警告\\)")

  ;; C ヘッダファイルにエラーがあった場合にエラー行がうまくパースできず、
  ;; flymake がエラーになる問題への対処
  (push '("^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 4)
        flymake-err-line-patterns))

(leaf flycheck
  :commands flycheck-mode
  :ensure t
  :custom
  ;; mode-line での flycheck の情報表示を無効に
  ;; (flycheck-mode-line . "")
  ;; カーソル位置のエラー情報をエコーエリアに表示しない
  ;; (flycheck-display-errors-function . nil)
  ;; カーソル位置のエラーを表示するまでの時間
  ;; 手動表示のみしたいが、できないので大きな時間を設定して自動表示させないよう
  ;; にする
  (flycheck-display-errors-delay . 10000.0)

  :hydra
  (hydra-flycheck ()
    "
Flycheck
"
    ("n" flycheck-next-error             "next")
    ("p" flycheck-previous-error         "previous")
    ("e" flycheck-explain-error-at-point "explain")
    ("l" flycheck-list-errors            "list")
    ("q" nil                             "quit"))

  :config
  ;; 無効化する checker の設定
  ;; emacs-lisp-checkdoc を無効化する
  (setq-default flycheck-disabled-checkers
                (cons 'emacs-lisp-checkdoc
                      (default-value 'flycheck-disabled-checkers)))


  (leaf flycheck-popup-tip
    :hook
    ((flycheck-mode-hook . flycheck-popup-tip-mode)))

  (leaf *popwin
    :after mod-popwin
    :config
    ;; エラーリストをポップアップで表示
    (mod-popwin:add-display-config '(flycheck-error-list-mode :noselect t :stick t)))

  (bind-keys :map keymap-for-code-navigation
             ("c" . flycheck-buffer)
             ("n" . hydra-flycheck/flycheck-next-error)
             ("p" . hydra-flycheck/flycheck-previous-error)
             ("e" . hydra-flycheck/flycheck-explain-error-at-point)
             ("l" . hydra-flycheck/flycheck-list-errors)))

(leaf super-save
  :ensure t
  :require t
  :custom
  (super-save-auto-save-when-idle . t)
  (super-save-idle-duration . 5)
  :config
  (add-to-list 'super-save-triggers 'find-file)
  (super-save-mode +1))

(leaf dmacro
  :leaf-defer t
  :init
  (defconst *dmacro-key* "\C-t" "繰返し指定キー")
  :require t
  :bind
  ("C-t" . dmacro-exec))

(leaf migemo
  :ensure t
  :require t
  :config
  ;; setup for cmigemo
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  ;; migemo-dict のパスを指定
  (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)

  ;; デフォルトでは migemo の機能を off に
  ;; isearch 中に M-m で migemo の on/off 切り替えが可能
  (setq migemo-isearch-enable-p nil)

  (migemo-init))

(leaf expand-region
  :ensure t
  :custom
  (expand-region-contract-fast-key . "R")
  :config
  (bind-key "r" 'er/expand-region keymap-ctrl-meta-space))

(leaf yasnippet
  :ensure t
  :delight yas-minor-mode
  :require t
  :config
  ;; snippet のディレクトリを追加
  (push  (concat user-emacs-directory "yasnippet-snippets") yas-snippet-dirs)

  ;; スニペットの候補が複数あった場合、ドロップダウンのプロンプトを出さないよ
  ;; うにする。
  (setq yas-prompt-functions '(yas-completing-prompt yas-no-prompt))

  (leaf mod-yasnippet :require t)

  (yas/global-mode 1))

(leaf open-junk-file
  :ensure t
  :config
  (defvar my:open-junk-file-base-dir "~/memo/junk/")
  ;; junk ファイル名フーマット
  (setq open-junk-file-format (concat my:open-junk-file-base-dir
                                      "%Y.%m.%d-%H.%M.%S."))

  (bind-key "C-j" 'open-junk-file keymap-ctrl-meta-space))

(leaf ivy
  :delight
  :ensure t
  :leaf-defer nil
  :require t
  :custom
  (ivy-use-virtual-buffers . t)
  (ivy-format-functions-alist . '((t . ivy-format-function-arrow)))
  (ivy-height . 15)
  (ivy-height-alist . '((swiper . 20)))
  (enable-recursive-minibuffers . t)

  :config
  (ivy-mode 1)

  :bind
  (:keymap-ctrl-meta-space
   :package my:keymaps
   ("C-;" . ivy-switch-buffer)
   ("C-M-;" . ivy-resume)))

(leaf counsel
  :ensure t
  :custom
  (counsel-yank-pop-separator . "\n-------\n")
  :config
  (leaf mod-counsel :require t)

  ;; find-file 中に C-w でディレクトリを 1 つ削除する
  (bind-key "C-w" 'counsel-up-directory counsel-find-file-map)
  (bind-key "C-w" 'ivy-backward-delete-char ivy-minibuffer-map)

  ;; grep/agの検索結果のファイルを C-z で参照する (デフォルトで C-M-m や  C-l にもバインドされている)
  (bind-key "C-z" 'ivy-call-and-recenter counsel-grep-map)
  (bind-key "C-z" 'ivy-call-and-recenter counsel-ag-map)

  :bind
  ("C-x C-f" . counsel-find-file)
  ("M-y" . counsel-yank-pop)
  (:minibuffer-local-map
   ("C-r" . counsel-minibuffer-history))
  (:keymap-for-grep
   :package my:keymaps
   ("g" . mod-counsel:counsel-ag))
  (:keymap-ctrl-meta-space
   :package my:keymaps
   ("C-'" . counsel-imenu)
   ("o g" . mod-counsel:counsel-grep-my-memo)
   ("o r" . mod-counsel:counsel-open-my-memo))
  (:keymap-for-manuals
   :package my:keymaps
     ("a" . counsel-apropos)))

(leaf swiper
  :ensure t
  :bind
  (:isearch-mode-map
   :package isearch
   ("C-'" . swiper-from-isearch))
  (:keymap-ctrl-meta-space
   :package my:keymaps
   ("C-o" . swiper)))

(leaf amx
  :ensure t
  :bind
  ("M-x" . amx)
  ("M-X" . execute-extended-command))

(leaf company
  :delight
  :ensure t
  :leaf-defer nil
  :require t
  :custom
  (company-idle-delay . nil)
  (company-minimum-prefix-length . 2)
  (company-selection-wrap-around . t)
  (company-require-match . nil) ; 候補にマッチしない入力をした場合、補完せず終了する
  (company-dabbrev-code-everywhere . t) ; コメントや文字列でも dabbrev-code の補完を行う
  :bind
  (:company-mode-map
   ("M-i" . company-complete)
   ("<tab>" . company-indent-or-complete-common)
   ("TAB" . company-indent-or-complete-common))
  (:company-active-map
   ("M-n" . company-select-next)
   ("M-p" . company-select-previous)
   ("M-i" . company-complete-common)
   ("M-m" . company-complete-selection)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("<tab>" . company-complete-common-or-cycle)
   ("TAB" . company-complete-common-or-cycle)
   ("C-h" . nil))
  :config
  ;; (setq-default company-backends
  ;;               '((company-capf company-files company-keywords company-dabbrev-code company-dabbrev)))

  (global-company-mode)

  (leaf company-irony
    :ensure t
    :config
    (add-to-list 'company-backends 'company-irony))

  (leaf company-quickhelp
    :ensure t
    :config
    (company-quickhelp-mode)))

(leaf sequential-command
  :ensure t
  :config
  (require 'sequential-command-config)

  (defun my:seq-aux-upcase-backword-word ()
    (interactive)
    (upcase-word (- 1)))

  (defun my:seq-aux-capitalize-backword-word ()
    (interactive)
    (capitalize-word (- 1)))

  (defun my:seq-aux-downcase-backword-word ()
    (interactive)
    (downcase-word (- 1)))

  ;; 直前の word を upper case -> capitalize -> lower case に変換する
  (define-sequential-command my:seq-upcase-capitalize-downcase-backword-word
    my:seq-aux-upcase-backword-word
    my:seq-aux-capitalize-backword-word
    my:seq-aux-downcase-backword-word)

  ;; sequential-command-config に含まれる seq-home に back-to-indentiation
  ;; を加えたバージョンを定義
  (define-sequential-command my:seq-home2
    back-to-indentation beginning-of-line beginning-of-buffer seq-return)

  :bind
  ("C-a" . seq-home)
  ("M-m" . my:seq-home2)
  ("C-e" . seq-end)
  ("M-U" . my:seq-upcase-capitalize-downcase-backword-word))

(leaf avy
  :ensure t
  :bind
  (:isearch-mode-map
   :package isearch
   (";" . avy-isearch)
   ("C-;" . avy-isearch)))

(leaf projectile
  :delight
  :leaf-defer nil
  :ensure t
  :config
  (setq  projectile-completion-system 'ivy)
  (projectile-mode +1)
  :bind
  (:projectile-mode-map
   ("C-c p" . projectile-command-map))
  (:keymap-ctrl-meta-space
   :package my:keymaps
   ("p" . projectile-command-map)
   ("C-p" . projectile-find-file)))

(leaf anzu
  :leaf-defer nil
  :ensure t
  :require t
  :custom
  (anzu-mode-lighter . "")
  (anzu-deactivate-region . t)
  (anzu-search-threshold . 1000)
  (anzu-use-mnigemo . t)
  (anzu-replace-to-string-separator . "")
  :config
  (global-anzu-mode +1)
  :bind
  ("M-%" . anzu-query-replace)
  ("C-M-%" . anzu-query-replace-regexp))

(leaf which-key
  :delight
  :leaf-defer nil
  :ensure t
  :require t
  :custom
  (which-key-idle-delay . 0.2)
  (which-key-show-transient-maps . nil)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode 1))

(leaf volatile-highlights
  :delight
  :leaf-defer nil
  :ensure t
  :require t
  :config
  ;; ハイライト時の face 設定
  (set-face-background 'vhl/default-face "dark slate blue")
  (volatile-highlights-mode))

(leaf highlight-symbol
  :delight
  :ensure t
  :custom
  (highlight-symbol-idle-delay . 1.0)
  :bind
  (:keymap-for-code-navigation
   :package my:keymaps
   ("h" . highlight-symbol)))

(leaf beacon
  :leaf-defer nil
  :ensure t
  :require t
  :config
  (beacon-mode 1))

(leaf revbufs
  :leaf-defer nil
  :require t)

(leaf smartparens
  :leaf-defer nil
  :ensure t
  :require t
  :delight
  :custom
  ;; カッコが自動挿入された際に、ハイライトを行わない
  (sp-highlight-pair-overlay . nil)
  :config
  (leaf smartparens-config :require t)
  (sp-use-paredit-bindings)
  (smartparens-global-mode)

  ;; リージョンが有効でない場合に sp-kill-region を実行した場合は後ろの単語を
  ;; killする
  (defun my:advice-sp-kill-region (orig-fun &rest args)
    (if (and (called-interactively-p 'any)
             (not (region-active-p)))
        (sp-backward-kill-word 1)
      (apply orig-fun args)))
  (advice-add 'sp-kill-region :around 'my:advice-sp-kill-region)

  :bind
  (:smartparens-mode-map
   ("C-<" . sp-splice-sexp-killing-backward)
   ("C->" . sp-splice-sexp-killing-forward)))

(leaf yascroll
  :leaf-defer nil
  :ensure t
  :require t
  :config
  (global-yascroll-bar-mode 1))

(leaf wgrep
  :ensure t
  :custom
  ;; wgrep-mode 有効化キーの変更
  (wgrep-enable-key . "e")
  ;; 編集終了後にバッファーを自動的に保存しない
  (wgrep-auto-save-buffer . nil)
  ;; readonly ファイルは変更を反映しない
  (wgrep-change-readonly-file . nil)
  :bind
  (:wgrep-mode-map
   ;; C-c C-c で変更を反映
   ("C-c C-c" . wgrep-finish-edit)
   ;; wgrep-finish-edit をバインドしているキーバインド
   ;; を解除(上記キーバインドをミニバッファに表示させるため)。
   ("C-c C-e" . nil)
   ("C-x C-s" . nil))
  :config
  (leaf wgrep-ag
    :after ag
    :require t))

(leaf multiple-cursors
  :ensure t
  :hydra
  (hydra-multiple-cursors (:hint nil)
    "
^Mark^         ^Unmark^       ^Skip^         ^All^           ^Edit                      ^Quit^
^^^^^^^^-----------------------------------------------------------------------------------------
_n_: next      _u_: next      _s_: next      _*_: all        _i_: insert numbers         _q_: done
_p_: prev      _U_: prev      _S_: prev      _d_: dwim       _o_: sort regions
_m_: more      ^ ^            ^ ^            ^ ^             _O_: reverse regions
"
    ("n" mc/mark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("m" mc/mark-more-like-this-extended)
    ("u" mc/unmark-next-like-this)
    ("U" mc/unmark-previous-like-this)
    ("s" mc/skip-to-next-like-this)
    ("S" mc/skip-to-previous-like-this)
    ("*" mc/mark-all-like-this)
    ("d" mc/mark-all-like-this-dwim)
    ("i" mc/insert-numbers)
    ("o" mc/sort-regions)
    ("O" mc/reverse-regions)
    ("q" nil :eixt t))
  :bind
  (:keymap-ctrl-meta-space
   :package my:keymaps
   ("m" . hydra-multiple-cursors/body)))

(leaf bm
  :ensure t
  :custom
  (bm-in-lifo-order . t)
  :hydra
  (hydra-bm (:hint nil)
    "
^Move^        ^Mark              ^Quit^
^^^^^^^^-----------------------------------
_n_: next     _b_: toggle        _q_: quit
_p_: prev     _D_: remove all
"
    ("n" bm-next)
    ("p" bm-previous)
    ("b" bm-toggle)
    ("D" bm-remove-all-current-buffer)
    ("q" nil :exit t))
  :bind
  (:keymap-ctrl-meta-space
   :package my:keymaps
   ("b" . hydra-bm/body)))

(leaf fill-column-indicator
  :ensure t
  :config
  (leaf mod-fill-column-indicator :require t))

(leaf selected
  :ensure t
  :leaf-defer nil
  :delight selected-minor-mode
  :require t
  :init
  (setq my:selected-keymap (make-sparse-keymap))
  :bind
  (:my:selected-keymap
   ("q" . selected-off)
   ("u" . upcase-region)
   ("d" . downcase-region))
  :config
  (bind-key "s" my:selected-keymap selected-keymap)
  (selected-global-mode))

(leaf direx
  :ensure t
  :init
  ;; dired-x がロードされるとキーバインドを上書きさるので、再度キーバインドを設定
  (with-eval-after-load 'dired-x
    (bind-key "C-x C-j" #'my:direx:jump-to-project-directory))
  :custom
  (direx:open-icon . "▾ ")
  (direx:closed-icon . "▸ ")
  :hook
  (direx:direx-mode-hook . my:direx:disable-font-lock-mode)
  :bind
  ("C-x C-j" . my:direx:jump-to-project-directory)
  :config
  ;; direx-projectile を試してダメだったら direx を実行する
  (defun my:direx:jump-to-project-directory ()
    (interactive)
    (let ((result (ignore-errors
                    (direx-project:jump-to-project-root-other-window)
                    t)))
      (unless result
        (direx:jump-to-directory-other-window))))

  ;; font-lock-mode が有効だと face が有効にならないので、無効にする
  (defun my:direx:disable-font-lock-mode ()
    (font-lock-mode -1))

  (leaf *popwin
    :after mod-popwin
    :config
    (mod-popwin:add-display-config
     '(direx:direx-mode :position left :width 40 :dedicated t))))

(leaf dumb-jump :ensure t)

(leaf *undo
  :config
  (leaf winner
    :config
    (winner-mode))

  (leaf undo-tree
    :ensure t
    :leaf-defer nil
    :require t
    :delight
    :config
    (global-undo-tree-mode)

    (leaf *popwin
      :after mod-popwin
      :config
      (mod-popwin:add-display-config
       `(,undo-tree-visualizer-buffer-name :height 0.45 :position bottom :stick t))))

  (leaf *hydra
    :hydra
    (hydra-undo ()
                "Undo
>"
                ("C-z" undo-tree-undo "undo")
                ("SPC" undo-tree-undo "undo")
                ("C-SPC" undo-tree-redo "redo")
                ("t" undo-tree-visualize "tree" :exit t)
                ("w" winner-undo "window undo")
                ("C-w" winner-redo "window redo"))
    :config
    (bind-key "C-z" 'hydra-undo/body)))

(leaf easy-kill
  :ensure t
  :bind
  ("M-w" . easy-kill))

(leaf lsp-mode
  :ensure t
  :custom
  ;; flyamke ではなく flycheck を使用する
  (lsp-prefer-flymake . nil)

  :config
  (leaf lsp-ui
    :ensure t
    :custom
    (lsp-ui-peek-always-show . t)
    (lsp-ui-doc-enable . nil))

  (leaf *popwin
    :after mod-popwin
    :config
    ;; lsp-describe-thing-at-point の表示を popup で出す
    (mod-popwin:add-display-config '("*lsp-help*" :noselect t :stick t)))

  :hydra
  (hydra-xref (:hint nil)
              "
^Find^                     ^Pop^               ^Quit^
^^^^^^^^-----------------------------------------------------------------
_M-._: find definitions    _,_: pop            _q_: quit
_M-/_: find references
"
              (","   xref-pop-marker-stack)
              ("M-." xref-find-definitions)
              ("M-/" xref-find-references)
              ("q"   nil))

  :bind
  (:keymap-for-code-navigation
   :package my:keymaps
   ("C-i" . completion-at-point)
   ("i"   . lsp-ui-imenu)
   ("."   . lsp-ui-peek-find-definitions)
   ("/"   . lsp-ui-peek-find-references)
   (","   . hydra-xref/xref-pop-marker-stack)
   ("x"   . lsp-rename)
   ("d"   . lsp-describe-thing-at-point)
   ("D"   . lsp-ui-doc-show)
   ("M-D" . lsp-ui-doc-hide)
   ("M-." . hydra-xref/xref-find-definitions)
   ("M-/" . hydra-xref/xref-find-references)))

(leaf text-mode
  :hook
  ((text-mode-hook . turn-on-auto-fill)))

(leaf *org
  :config
  (leaf org-install
    :config
    (setq org-startup-truncated nil)
    (setq org-return-follows-link t)
    (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
    (setq org-directory "~/memo/org/")
    (setq org-default-notes-file (concat org-directory "agenda.org"))

    ;; org-mode で書き換えられたくないキーバインドの書き換えを回避
    (defun my:hook-func-org-mode ()
      (define-key org-mode-map (kbd "C-<tab>") nil)
      (define-key org-mode-map (kbd "C-a") nil)
      (define-key org-mode-map (kbd "C-e") nil)
      (define-key org-mode-map (kbd "C-'") nil))

    ;; ファイルを開いたとき見出しを折り畳まない
    (setq org-startup-folded 'nofold)
    :hook
    ((org-mode-hook . my:hook-func-org-mode)))

  (leaf org-capture
    :config
    (leaf org-install :require t)

    ;; メモ用のファイル名を作成する関数
    (setq my:org-capture-journal-dir (concat org-directory "journal/"))
    (setq my:org-capture-note-dir (concat org-directory "note/"))
    (setq my:org-capture-cheatsheet-dir (concat org-directory "cheatsheet/"))

    (defun my:create-journal-file-name ()
      (unless (file-exists-p my:org-capture-journal-dir)
        (make-directory my:org-capture-journal-dir))
      (concat my:org-capture-journal-dir
              (format-time-string "%Y.%m.%d-%H.%M.%S.org")))

    (defun my:create-note-file-name ()
      (unless (file-exists-p my:org-capture-note-dir)
        (make-directory my:org-capture-note-dir))
      (concat my:org-capture-note-dir
              (format-time-string "%Y.%m.%d-%H.%M.%S.org")))

    (defun my:create-cheatsheet-file-name ()
      (unless (file-exists-p my:org-capture-cheatsheet-dir)
        (make-directory my:org-capture-cheatsheet-dir))
      (concat my:org-capture-cheatsheet-dir
              (format-time-string "%Y.%m.%d-%H.%M.%S.org")))

    (setq org-capture-templates
          '(("t" "Todo" entry
             (file+headline "todo.org" "Inbox")
             "** TODO %?\n   %i\n   %a\n   %t")
            ("j" "Journal" entry
             (file my:create-journal-file-name)
             "* %?\n[%T]\n")
            ("n" "Note" entry
             (file my:create-note-file-name)
             "* %?\n[%T]\n")
            ("c" "Cheatsheet" entry
             (file my:create-cheatsheet-file-name)
             "* %?\n[%T]\n")
            ("i" "Idea" entry
             (file+headline "ideas.org" "New Ideas")
             "** %?\n   %i\n   %a\n   %t")
            ("b" "Bookmark" entry
             (file+headline "bookmarks.org" "Bookmarks")
             "** %?\n   %i\n   %a\n   %t")))

;;;  %[pathname] insert the contents of the file given by `pathname'.
;;;  %(sexp)     evaluate elisp `(sexp)' and replace with the result.
;;;  %<...>      the result of format-time-string on the ... format specification.
;;;  %t          time stamp, date only.
;;;  %T          time stamp with date and time.
;;;  %u, %U      like the above, but inactive time stamps.
;;;  %a          annotation, normally the link created with `org-store-link'.
;;;  %i          initial content, copied from the active region.  If %i is
;;;              indented, the entire inserted text will be indented as well.
;;;  %A          like %a, but prompt for the description part.
;;;  %c          current kill ring head.
;;;  %x          content of the X clipboard.
;;;  %k          title of currently clocked task.
;;;  %K          link to currently clocked task.
;;;  %n          user name (taken from `user-full-name').
;;;  %f          file visited by current buffer when org-capture was called.
;;;  %F          full path of the file or directory visited by current buffer.
;;;  %:keyword   specific information for certain link types, see below.
;;;  %^g         prompt for tags, with completion on tags in target file.
;;;  %^G         prompt for tags, with completion on all tags in all agenda files.
;;;  %^t         like %t, but prompt for date.  Similarly %^T, %^u, %^U.
;;;              You may define a prompt like %^{Please specify birthday.
;;;  %^C         interactive selection of which kill or clip to use.
;;;  %^L         like %^C, but insert as link.
;;;  %^{prop}p   prompt the user for a value for property `prop'.
;;;  %^{prompt}  prompt the user for a string and replace this sequence with it.
;;;              A default value and a completion table ca be specified like this:
;;;              %^{prompt|default|completion2|completion3|...}.
;;;  %?          After completing the template, position cursor here.

    ;; org-agenda
    (setq org-agenda-files
          (list org-directory my:org-capture-journal-dir my:org-capture-note-dir))

    ;; todo
    (setq org-todo-keywords
          '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

    :bind
    ("C-c o c" . org-capture)
    ("C-c o a" . org-agenda)
    (:keymap-ctrl-meta-space
     :package my:keymaps
     ("o c" . org-capture)
     ("o a" . org-agenda))))

(leaf cc-mode
  :hook
  (c-mode-common-hook . my:hook-c-mode-common--0)
  (c-mode-common-hook . my:hook-c-mode-common--ff-find-other-file)
  :config
  (defun my:hook-c-mode-common--0 ()
    (show-paren-mode t)
    (setq indent-tabs-mode nil))

  (defun my:hook-c-mode-common--ff-find-other-file ()
    (cond ((eq major-mode 'c-mode)
           (define-key c-mode-map (kbd "C-c .") 'ff-find-other-file))
          ((eq major-mode 'c++-mode)
           (define-key c++-mode-map (kbd "C-c .") 'ff-find-other-file))))

  (leaf cquery
    :config
    (setq cquery-executable "cquery")

    (defun my:cquery-enable ()
      (condition-case nil
          (lsp)
        (user-error nil)))

    :hook
    (c-mode-hook . my:cquery-enable)
    (c++-mode-hook . my:cquery-enable))

  (leaf flycheck
    :hook
    (c-mode-common-hook . flycheck-mode))

  (leaf fill-column-indicator
    :config
    (defun my:hook-c-mode-common--fci ()
      (setq fill-column 80)
      (fci-mode 1))
    :hook
    (c-mode-common-hook . my:hook-c-mode-common--fci)))

(leaf sh-script
  :hook
  (sh-mode-hook . my:hook-sh-mode--0)
  :config
  (defun my:hook-sh-mode--0 ()
    (setq sh-basic-offset 2) ; インデント幅の設定
    (setq sh-indentation 2)) ; インデント幅の設定

  (leaf fill-column-indicator
    :config
    (defun my:hook-sh-mode--fci ()
      (setq fill-column 80)
      (fci-mode 1))
    :hook
    (sh-mode-hook . my:hook-sh-mode--fci)))

(use-package scheme
  :hook
  (scheme-mode-hook . my:hook-scheme-mode--0)
  :config
  (defun my:hook-scheme-mode--0 ()
    (show-paren-mode t)
    (setq indent-tabs-mode nil))

  (leaf smartparens
    :hook
    (scheme-mode-hook . smartparens-strict-mode))

  (leaf fill-column-indicator
    :config
    (defun my:hook-scheme-mode--fci ()
      (setq fill-column 80)
      (fci-mode 1))
    :hook
    (scheme-mode-hook . my:hook-scheme-mode--fci))

  ;; popwin for inferior-scheme-mode
  (leaf *popwin
    :after mod-popwin
    :config
    (mod-popwin:add-display-config
     '(inferior-scheme-mode :noselect t :stick t)))

  ;; Gauche
  (setq scheme-program-name "gosh")

  (defun my:scheme-other-window ()
    "Run scheme on other window"
    (interactive)
    (switch-to-buffer-other-window (get-buffer-create "*scheme*"))
    (run-scheme scheme-program-name))

  (defun my:gauche-info ()
    (interactive)
    (info "/usr/share/info/gauche-refe.info.gz"))


  ;; from Gauche:EditingWithEmacs 
  ;;      (http://www.shiro.dreamhost.com/scheme/wiliki/wiliki.cgi?Gauche%3aEditingWithEmacs)
  (put 'and-let* 'scheme-indent-function 1)
  (put 'begin0 'scheme-indent-function 0)
  (put 'call-with-client-socket 'scheme-indent-function 1)
  (put 'call-with-input-conversion 'scheme-indent-function 1)

  (put 'call-with-input-file 'scheme-indent-function 1)
  (put 'call-with-input-process 'scheme-indent-function 1)
  (put 'call-with-input-string 'scheme-indent-function 1)
  (put 'call-with-iterator 'scheme-indent-function 1)
  (put 'call-with-output-conversion 'scheme-indent-function 1)
  (put 'call-with-output-file 'scheme-indent-function 1)
  (put 'call-with-output-string 'scheme-indent-function 0)
  (put 'call-with-temporary-file 'scheme-indent-function 1)
  (put 'call-with-values 'scheme-indent-function 1)
  (put 'dolist 'scheme-indent-function 1)
  (put 'dotimes 'scheme-indent-function 1)
  (put 'if-match 'scheme-indent-function 2)
  (put 'let*-values 'scheme-indent-function 1)
  (put 'let-args 'scheme-indent-function 2)
  (put 'let-keywords* 'scheme-indent-function 2)
  (put 'let-match 'scheme-indent-function 2)
  (put 'let-optionals* 'scheme-indent-function 2)
  (put 'let-syntax 'scheme-indent-function 1)
  (put 'let-values 'scheme-indent-function 1)
  (put 'let/cc 'scheme-indent-function 1)
  (put 'let1 'scheme-indent-function 2)
  (put 'letrec-syntax 'scheme-indent-function 1)
  (put 'make 'scheme-indent-function 1)
  (put 'multiple-value-bind 'scheme-indent-function 2)
  (put 'parameterize 'scheme-indent-function 1)
  (put 'parse-options 'scheme-indent-function 1)
  (put 'receive 'scheme-indent-function 2)
  (put 'rxmatch-case 'scheme-indent-function 1)
  (put 'rxmatch-cond 'scheme-indent-function 0)
  (put 'rxmatch-if  'scheme-indent-function 2)
  (put 'rxmatch-let 'scheme-indent-function 2)
  (put 'syntax-rules 'scheme-indent-function 1)
  (put 'unless 'scheme-indent-function 1)
  (put 'until 'scheme-indent-function 1)
  (put 'when 'scheme-indent-function 1)
  (put 'while 'scheme-indent-function 1)
  (put 'with-builder 'scheme-indent-function 1)
  (put 'with-error-handler 'scheme-indent-function 0)
  (put 'with-error-to-port 'scheme-indent-function 1)
  (put 'with-input-conversion 'scheme-indent-function 1)
  (put 'with-input-from-port 'scheme-indent-function 1)
  (put 'with-input-from-process 'scheme-indent-function 1)
  (put 'with-input-from-string 'scheme-indent-function 1)
  (put 'with-iterator 'scheme-indent-function 1)
  (put 'with-module 'scheme-indent-function 1)
  (put 'with-output-conversion 'scheme-indent-function 1)
  (put 'with-output-to-port 'scheme-indent-function 1)
  (put 'with-output-to-process 'scheme-indent-function 1)
  (put 'with-output-to-string 'scheme-indent-function 1)
  (put 'with-port-locking 'scheme-indent-function 1)
  (put 'with-string-io 'scheme-indent-function 1)
  (put 'with-time-counter 'scheme-indent-function 1)
  (put 'with-signal-handlers 'scheme-indent-function 1)

  (put 'match 'scheme-indent-function 1))

(leaf ruby-mode
  :config
  (leaf flycheck
    :hook
    (ruby-mode-hook . flycheck-mode))

  (leaf fill-column-indicator
    :config
    (defun my:hook-ruby-mode--fci ()
      (setq fill-column 80)
      (fci-mode 1))
    :hook
    (ruby-mode-hook . my:hook-ruby-mode--fci))

  ;; Emacs 24.4 用設定
  ;; ruby-mode-set-encoding の実装と auto-save-buffers の相性が悪いらしいので
  ;; その対処。ruby-insert-encoding-magic-comment を nil にしただけでは解決し
  ;; ない
  ;; (when (version<= "24.4" emacs-version)
  ;;   (defun ruby-mode-set-encoding ()))

  ;; inf-ruby
  (leaf inf-ruby
    :ensure t
    :hook
    (ruby-mode-hook . inf-ruby-minor-mode))

  ;; ruby-block
  ;;  キーワード end に対応する行をハイライトする
  (leaf ruby-block
    :ensure t
    :delight
    :config
    ;; これを行っていないと ruby-mode 以外でも ruby-block minor-mode が有効に
    ;; なってしまう
    (setq-default ruby-block-mode nil)
    ;; end に対応する行をミニバッファに表示し、かつオーバレイする
    (setq ruby-block-highlight-toggle t))


  (leaf *popwin
    :after mod-popwin
    :config
    (mod-popwin:add-display-config
     '(inf-ruby-mode :height 0.45 :position bottom :stick t))))

(leaf elisp-mode
  :delight (emacs-lisp-mode "ELisp" :major)
  :hook
  (emacs-lisp-mode-hook . my:hook-emacs-lisp-mode--0)
  :config
  (defun my:hook-emacs-lisp-mode--0 ()
    (setq indent-tabs-mode nil))

  (leaf smartparens
    :hook
    (emacs-lisp-mode-hook . smartparens-strict-mode))

  (leaf flycheck
    :hook
    (emacs-lisp-mode-hook . flycheck-mode))

  (leaf fill-column-indicator
    :config
    (defun my:hook-emacs-lisp-mode--fci ()
      (setq fill-column 80)
      (fci-mode 1))
    :hook
    (emacs-lisp-mode-hook . my:hook-emacs-lisp-mode--fci)))

(leaf rust-mode
  :ensure t
  :config
  ;; rust-modeでrust-format-on-saveをtにすると自動でrustfmtが走る
  (setq-default rust-format-on-save t)

  ;; rustのファイルを編集するときにflycheckを起動する
  (leaf flycheck
    :hook
    ;; lsp-mode の flycheck 設定を使用するため、flycheck-rust-setup は hook に追
    ;; 加しない
    ;; (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    (rust-mode-hook . flycheck-mode))

  ;; ;; Racer - code completion for Rust
  ;; (use-package racer
  ;;   :commands racer-mode
  ;;   :delight
  ;;   :hook
  ;;   ;; rustのファイルを編集するときにracerを起動する
  ;;   ((rust-mode . racer-mode)
  ;;    ;; racerのeldocサポートを使う
  ;;    (racer-mode . eldoc-mode)
  ;;    ;; ;; racerの補完サポートを使う
  ;;    (racer-mode . company-mode)))

  (leaf lsp-mode
    :custom
    ;; clippy によるチェックを常に行うよう設定
    (lsp-rust-clippy-preference . "on")
    :hook
    (rust-mode-hook . lsp))

  (leaf fill-column-indicator
    :config
    (defun my:hook-rust-mode-common--fci ()
      (setq fill-column 80)
      (fci-mode 1))
    :hook
    (rust-mode-hook . my:hook-rust-mode-common--fci)))

(leaf gud
  :config
  ;; 有用なバッファを開くモード
  (setq gdb-many-windows t)
  ;; I/O バッファを表示
  (setq gdb-use-separate-io-buffer t)
  ;; t にすると mini buffer に値が表示される
  (setq gud-tooltip-echo-area nil)

  :hook
  ;; 変数の上にマウスカーソルを置くと値を表示
  (gdb-mode-hook . gud-tooltip-mode))

(leaf view
  :config
  (leaf mod-view :require t)

  (setq view-read-only t)

  ;; view-mode キーバインド設定
  (defvar pager-keybind
    `( ;; vi-like
      ("h" . backward-char)
      ("l" . forward-char)
      ("j" . next-line)
      ("k" . previous-line)
      (";" . gene-word)
      ("b" . scroll-down)
      (" " . scroll-up)
      ("i" . view-mode) ; view-mode から脱出
      ;; w3m-like
      ("m" . gene-word)
      ("w" . forward-word)
      ("e" . backward-word)
      ("(" . point-undo)
      (")" . point-redo)
      ("J" . ,(lambda () (interactive) (scroll-up 1)))
      ("K" . ,(lambda () (interactive) (scroll-down 1)))
      ;; bm-easy
      ("." . bm-toggle)
      ("[" . bm-previous)
      ("]" . bm-next)
      ;; langhelp-like
      ("c" . scroll-other-window-down)
      ("v" . scroll-other-window)
      ))

  (defun my:define-many-keys (keymap key-table &optional includes)
    (let (key cmd)
      (dolist (key-cmd key-table)
        (setq key (car key-cmd)
              cmd (cdr key-cmd))
        (if (or (not includes) (member key includes))
            (define-key keymap key cmd))))
    keymap)

  (defun my:hook-view-mode--keybind ()
    (my:define-many-keys view-mode-map pager-keybind))

  :hook
  (view-mode-hook . my:hook-view-mode--keybind)

  :bind
  (:keymap-ctrl-meta-space
   :package my:keymaps
   ("C-v" . view-mode)))

(leaf info
  :config
  ;; Info-directory-list の初期設定は環境変数 INFOPATH を元に行われるが、
  ;; exec-path-from-shell による INFOPATH の設定よりも前に Info-directory-list
  ;; が 初期設定されるため、Info-directory-list を未初期化 (nil を設定) するこ
  ;; とで、もう一度初期設定を行なわせる
  (setq Info-directory-list nil))

(leaf woman
  :config
  (setq woman-use-own-frame nil)

  (leaf *popwin
    :after mod-popwin
    :config
    ;; popwin for Woman-mode
    (mod-popwin:add-display-config
     '("^\\*WoMan" :regexp t :noselect t :stick t))
    ;; popwin for Man-mode
    (mod-popwin:add-display-config
     '(Man-mode :noselect t :stick t)))
  :bind
  (:keymap-for-manuals
   :package my:keymaps
   ("m" . woman)))

(leaf dired
  :config
  (leaf dired-x :require t)

  (leaf wdired
    :bind
    (:dired-mode-map
     :package dired
     ("e" . wdired-change-to-wdired-mode)))

  (load "sorter")

  ;; dired-find-alternate-file コマンドを有効化
  (put 'dired-find-alternate-file 'disabled nil)
  :bind
  (:dired-mode-map
   ;; C-m で新規バッファを作成せずにディレクトリ/ファイルを開く
   ("C-m" . dired-find-alternate-file)
   ;; a で新規バッファを作成してディレクトリ/ファイルを開く
   ("a" . dired-find-file)))

(leaf ag
  :ensure t
  :config

  ;; popwin for ag
  (leaf *popwin
    :after mod-popwin
    :config
    (mod-popwin:add-display-config '(ag-mode :noselect t :stick t))))

(leaf eww
  :config
  (leaf mod-eww :require t)
  (setq eww-search-prefix "https://www.google.co.jp/search?q=")
  :bind
  (:eww-mode-map
   ("h" . backword-char)
   ("j" . next-line)
   ("k" . previous-line)
   ("l" . forward-char)
   ("n" . scroll-up)
   ("p" . scroll-down)
   ("r" . eww-reload)
   ("<" . eww-back-url)
   (">" . eww-forward-url)))

(leaf ahg
  :ensure t)

(leaf ddskk
  :ensure t
  :init
  ;; Emacs の input method を ddskk にする
  ;; "C-\" で skk-mode を on にできる
  (setq default-input-method "japanese-skk")

  (setq skk-user-directory (concat user-emacs-directory "ddskk/"))

  :config
  ;; チュートリアルの場所設定
  (setq skk-tut-file (concat user-emacs-directory "elisp/ddskk/etc/SKK.tut"))

  (defun my:hook-skk-load--0 ()
    ;; コメント行を抜けたら ascii にする
    ;; (require 'context-skk)
    ;; C-M-j でアンドゥ確定
    (bind-key "C-M-j" 'skk-undo-kakutei skk-j-mode-map))

  (add-hook 'skk-load-hook 'my:hook-skk-load--0)

  (leaf *isearch
    :config
    ;; Isearch setting.
    (defun my:skk-isearch-setup-maybe ()
      (require 'skk-vars)
      (when (or (eq skk-isearch-mode-enable 'always)
                (and (boundp 'skk-mode)
                     skk-mode
                     skk-isearch-mode-enable))
        (skk-isearch-mode-setup)))

    (defun my:skk-isearch-cleanup-maybe ()
      (require 'skk-vars)
      (when (and (featurep 'skk-isearch)
                 skk-isearch-mode-enable)
        (skk-isearch-mode-cleanup)))

    (add-hook 'isearch-mode-hook #'my:skk-isearch-setup-maybe)
    (add-hook 'isearch-mode-end-hook #'my:skk-isearch-cleanup-maybe)))

(leaf *custom-file
  :config
  ;; 25.1 から custom-set-variables が init.el に書き出されるようになってい
  ;; て邪魔なので ~/.emacs.d/custom.el に書き出すよう変更する
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file))

(leaf server
  :require t
  :config
  (unless (server-running-p) (server-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs の挙動がおかしい場合は調査のため有効にする
;; (toggle-debug-on-error)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
