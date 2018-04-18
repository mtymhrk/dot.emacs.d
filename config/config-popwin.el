;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  popwin
;;;    http://d.hatena.ne.jp/m2ym/20110120/1295524932
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ヘルプバッファや補完バッファをポップアップで表示する elisp

;;; (auto-install-from-url "https://raw.github.com/m2ym/popwin-el/v0.3/popwin.el")

(eval-when-compile (require 'use-package))

(use-package popwin
  :commands popwin-mode
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

  ;; TODO: delte
  (defalias 'add-popwin-special-display-config
    'mod-popwin:add-display-config)
  (defalias 'delete-popwin-special-display-config
    'delte-popwin:add-display-config)

  (use-package hydra
    :config
    (defhydra hydra-popwin ()
      ("p"          mod-popwin:repopup-window       "cycle"        )
      ("q"          popwin:close-popup-window       "close"        )
      ("j"          mod-popwin:scroll-up            "scroll-up"    )
      ("k"          mod-popwin:scroll-down          "scroll-down"  )
      ("h"          mod-popwin:scroll-right         "scroll-right" )
      ("l"          mod-popwin:scroll-left          "scroll-left"  )
      ("C-v"        mod-popwin:scroll-up-command    "up-cmd"       )
      ("M-v"        mod-popwin:scroll-down-command  "down-cmd"     )
      ("M-<"        mod-popwin:beginning-of-buffer  "bob"          )
      ("M->"        mod-popwin:end-of-buffer        "eob"          )
      ("C-s"        mod-popwin:isearch-forward      "isearch"      )
      ("C-r"        mod-popwin:isearch-backward     "isearch"      )
      ("C-a"        mod-popwin:seq-home             "home"         )
      ("M-m"        mod-popwin:seq-home2            "home2"        )
      ("C-e"        mod-popwin:seq-end              "end"          )
      ("<C-return>" popwin:select-popup-window      "select"       ))
    (bind-key "p" 'hydra-popwin/body keymap-ctrl-meta-space))
  )



(popwin-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-popwin)
