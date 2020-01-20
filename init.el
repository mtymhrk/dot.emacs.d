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
               '("melpa" . "http://melpa.org/packages/") t))

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
    (electric-indent-mode -1))

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
  (define-key keymap-ctrl-meta-space (kbd "g") keymap-for-grep))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 雑多なコマンド定義
(leaf *my:commands
  :config
  ;; カーソル上の文字のフェイスを表示するコマンド
  (defun describe-face-at-point ()
    (interactive)
    (message "%s" (get-char-property (point) 'face)))

  ;; ~/.emacs.d/elisp 配下の elisp をバイトコンパイルするコマンド
  (defun my-byte-recompile-elisp-directory ()
    (interactive)
    (byte-recompile-directory (concat user-emacs-directory "elisp") 0 t))

  ;; package の elisp を再コンパイルするコマンド
  (defun my-package-recompile (pkg)
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
  (defun my-package-recompile-all ()
    (dolist (pkg package-alist)
      (let* ((name (package-desc-name (cadr pkg))))
        (unless (package-built-in-p name)
          (my-package-recompile name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; その他基本機能の挙動変更

;; kill-region と kill-ring-save の拡張
(leaf ex-kill-region
  :require t)

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
    (setq my:default-frame-font "MyricaM M-10.5")
    (setq my:default-frame-height 62)
    (setq my:default-frame-width 246))
   ((string-match "amber" (system-name))
    (setq my:default-frame-font "MyricaM M-10.5")
    (setq my:default-frame-height 43)
    (setq my:default-frame-width 163))
   (t
    (setq my:default-frame-font "DejaVu Sans Mono-9")
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
    :init
    (eyebrowse-mode 1)
    :custom
    ;; eyebrowse-mode-map にキーをバインドしない
    (eyebrowse-keymap-prefix . "")
    (eyebrowse-new-workspace . #'my-eyebrowse-new-workspace-func)
    ;; doom-mode-line では別途 Window Config 番号が表示されるので eyebrowse での表示は削除
    (eyebrowse-mode-line-style . 'hide)
    :config
    (defun my:eyebrowse-new-workspace-func ()
      (switch-to-buffer "*scratch*")
      (my:default-window-split)))

  (leaf popwin
    :ensure t
    :commands popwin-mode
    :init
    (popwin-mode 1)
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
    (mod-popwin:add-display-config '(apropos-mode :noselect nil :stick t)))

  (leaf winner
    :ensure t
    :init
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
    ("h" flycheck-display-error-at-point "display")
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
    ((flycheck-mode . flycheck-popup-tip-mode)))

  (leaf *popwin
    :after mod-popwin
    :config
    ;; エラーリストをポップアップで表示
    (mod-popwin:add-display-config '(flycheck-error-list-mode :noselect t :stick t)))

  (bind-keys :map keymap-for-code-navigation
             ("c" . flycheck-buffer)
             ("n" . hydra-flycheck/flycheck-next-error)
             ("p" . hydra-flycheck/flycheck-previous-error)
             ("h" . hydra-flycheck/flycheck-display-error-at-point)
             ("e" . hydra-flycheck/flycheck-explain-error-at-point)
             ("l" . hydra-flycheck/flycheck-list-errors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 個別設定ファイルのロード

(defvar my-init-config-dir (concat user-emacs-directory "config/"))
(defvar my-init-config-file-list
  '(;; Basics
    ;; "config-basics"

    ;; Package System
    ;; "config-package"

    ;; Appearance
    ;; "config-frame-and-font"
    ;; "config-theme"

    ;; Minor-modes and Utilities
    ;; "config-exec-path-from-shell"
    ;; "config-eldoc"
    ;; "config-whitespace"
    ;; "config-hydra"
    ;; "config-window"
    ;; "config-recentf"
    ;; "config-compile"
    ;; "config-irony"
    ;; "config-flymake"
    ;; "config-flycheck"
    ; "config-auto-save-buffers"
    "config-super-save"
    "config-dmacro"
    "config-migemo"
    "config-gtags"
    "config-expand-region"
    "config-yasnippet"
    "config-open-junk-file"
    "config-counsel"
    "config-amx"
    "config-company"
    "config-smart-tab"
    "config-sequential-command"
    "config-avy"
    "config-projectile"
    "config-quickrun"
    "config-anzu"
    "config-which-key"
    "config-volatile-highlights"
    "config-highlight-symbol"
    "config-beacon"
    "config-revbufs"
    "config-smartparens"
    "config-yascroll"
    "config-wgrep"
    "config-multiple-cursors"
    "config-bm"
    "config-fill-column-indicator"
    "config-selected"
    "config-direx"
    "config-dumb-jump"
    "config-undo"
    "config-easy-kill"
    "config-display-line-info"

    "config-lsp-mode"

    ;; Major-modes
    "config-text-mode"
    "config-org-mode"
    "config-c-mode"
    "config-sh-mode"
    "config-scheme-mode"
    "config-ruby-mode"
    "config-emacs-lisp-mode"
    "config-rust-mode"
    "config-gdb"
    "config-view-mode"
    "config-info"
    "config-woman"
    "config-dired"
    "config-ag"
    "config-eww"
    "config-hg"

    ;; Input Method
    "config-ddskk"
))

(defun my-load-init-config-files (file-list)
  (let ((load-errors '()))
    (dolist (file file-list)
      (condition-case err
          (load-file (concat my-init-config-dir file ".el"))
        (error
         (push (cons file err) load-errors)
         (message "Error has occurred while loading `%s': %s"
                  file (error-message-string err)))))
    (unless (null load-errors)
      (with-output-to-temp-buffer "*Config File Load Error*"
        (princ "Configuration File Load Error\n\n")
        (dolist (err load-errors)
          (princ (format "  %s: %s\n"
                         (car err) (error-message-string (cdr err)))))))))

(my-load-init-config-files my-init-config-file-list)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 25.1 から custom-set-variables が init.el に書き出されるようになってい
;;; て邪魔なので ~/.emacs.d/custom.el に書き出すよう変更する
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs server の起動

(require 'server)
(unless (server-running-p) (server-start))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs の挙動がおかしい場合は調査のため有効にする
;; (toggle-debug-on-error)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
