;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package switch-window
  :custom
  (switch-window-shortcut-style 'qwerty)
  (switch-window-qwerty-shortcuts '("j" "k" "l" ";" "i" "o" "a" "s" "d" "f" "w" "e"))
  :bind
  ("C-<tab>" . switch-window)
  ("<C-S-iso-lefttab>" . switch-window-then-delete)
  (:map switch-window-extra-map
        ("C-k" . switch-window-mvborder-up)
        ("C-j" . switch-window-mvborder-down)
        ("C-h" . switch-window-mvborder-left)
        ("C-l" . switch-window-mvborder-right)
        ("C-b" . balance-windows))
  :config
  (use-package mod-switch-window)
  (set-face-attribute 'switch-window-label nil :height 10.0)
  (cl-loop for key in switch-window-qwerty-shortcuts
           do (bind-key key nil switch-window-extra-map)))

(use-package eyebrowse
  :commands eyebrowse-mode
  :init
  (eyebrowse-mode 1)
  :custom
  ;; eyebrowse-mode-map にキーをバインドしない
  (eyebrowse-keymap-prefix "")
  (eyebrowse-new-workspace #'my-eyebrowse-new-workspace-func)
  ;; doom-mode-line では別途 Window Config 番号が表示されるので eyebrowse での表示は削除
  (eyebrowse-mode-line-style 'hide)
  :config
  (defun my-eyebrowse-new-workspace-func ()
    (switch-to-buffer "*scratch*")
    (my-default-window-split)))

(use-package popwin
  :commands popwin-mode
  :init
  (popwin-mode 1)
  :config
  (use-package mod-popwin)
  ;; ポップアップウィンドウの高さ (フレームに対する割合で指定)
  (setq popwin:popup-window-height 0.4)

  ;; 補完バッファをポップアップで表示
  (mod-popwin:add-display-config '(compilation-mode :noselect t :stick t))

  ;; occur バッファをポップアップで表示
  ;; (:noselect t とすると、検索元のバッファが read-only になってしまうので
  ;;  noselect は有効にはせず、advice 設定で noselect 化する)
  (mod-popwin:add-display-config '(occur-mode :stick t))

  (defadvice occur (after popwin-noselect activate)
    (when (and popwin-mode popwin:selected-window)
      (select-window popwin:selected-window)))

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

  ;; TODO: delte
  (defalias 'add-popwin-special-display-config
    'mod-popwin:add-display-config)
  (defalias 'delete-popwin-special-display-config
    'delte-popwin:add-display-config))

(use-package winner
  :config
  (winner-mode))

(use-package hydra
  :config
  (defhydra hydra-popwin (:hint nil)
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

  (defhydra hydra-eyebrowse (:hint nil)
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


  (defhydra hydra-window (:hint nil)
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
      ("q" nil :exit t)))

(bind-key "w" 'hydra-window/body keymap-ctrl-meta-space)
