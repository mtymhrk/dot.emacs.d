;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  タブと空白の可視化設定
;;;    タブと全角スペース、行末の空白を表示する設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package whitespace
  :delight whitespace
  :delight global-whitespace-mode
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
          (tab-mark ?\t [?\xBB ?\t] [?\\ ?\t])))

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
                      :foreground "dim gray"
                      :background 'unspecified
                      :strike-through nil
                      :underline t)

  (setq whitespace-global-modes '(not dired-mode))
  (global-whitespace-mode 1))



;;;   http://openlab.dino.co.jp/2008/08/29/230500336.html
;;;   http://homepage3.nifty.com/satomii/software/elisp.ja.html
;;;   http://homepage3.nifty.com/satomii/software/jaspace.el
(use-package jaspace
  :disabled
  :delight
  :config
  (setq jaspace-modes (append jaspace-modes
                              (list 'php-mode
                                    'yaml-mode
                                    'javascript-mode
                                    'ruby-mode
                                    'scheme-mode
                                    'makefile-gmake-mode
                                    'text-mode
                                    'fundamental-mode
                                    'org-mode)))
  (setq jaspace-alternate-jaspace-string "□")
  (setq jaspace-highlight-tabs ?\xBB)

  (add-hook 'jaspace-mode-off-hook
            #'(lambda()
                (setq show-trailing-whitespace nil)))

  (add-hook 'jaspace-mode-hook
            #'(lambda()
                (setq show-trailing-whitespace t)
                (face-spec-set 'jaspace-highlight-jaspace-face
                               '((((class color) (background light))
                                  (:foreground "blue"))
                                 (t (:foreground "purple"))))
                (face-spec-set 'jaspace-highlight-tab-face
                               '((((class color) (background light))
                                  (:foreground "red"
                                               :background "unspecified"
                                               :strike-through nil
                                               :underline t))
                                 (t (:foreground "purple"
                                                 :background "unspecified"
                                                 :strike-through nil
                                                 :underline t))))
                (face-spec-set 'trailing-whitespace
                               '((((class color) (background light))
                                  (:foreground "red"
                                               :background "unspecified"
                                               :strike-through nil
                                               :underline t))
                                 (t (:foreground "purple"
                                                 :background "unspecified"
                                                 :strike-through nil
                                                 :underline t)))))))



