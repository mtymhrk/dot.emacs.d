;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  popwin
;;;    http://d.hatena.ne.jp/m2ym/20110120/1295524932
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ヘルプバッファや補完バッファをポップアップで表示する elisp

;;; (auto-install-from-url "https://raw.github.com/m2ym/popwin-el/v0.3/popwin.el")

(require 'popwin)
(popwin-mode 1)

;;; ポップアップウィンドウの高さ (フレームに対する割合で指定)
(setq popwin:popup-window-height 0.4)

(defun delete-popwin-special-display-config (pattern)
  (let ((x (assoc pattern popwin:special-display-config)))
  (when x
    (setq popwin:special-display-config
          (delq x popwin:special-display-config)))))

(defun add-popwin-special-display-config (config)
  (delete-popwin-special-display-config (car config))
  (push config popwin:special-display-config))

;;; 補完バッファをポップアップで表示
(add-popwin-special-display-config '(compilation-mode :noselect t :stick t))

;;; occur バッファをポップアップで表示
;;; (:noselect t とすると、検索元のバッファが read-only になってしまうので
;;;  noselect は有効にはせず、advice 設定で noselect 化する)
(add-popwin-special-display-config '(occur-mode :stick t))
(defadvice occur (after popwin-noselect activate)
  (when (and popwin-mode popwin:selected-window)
    (select-window popwin:selected-window)))

;;; grep の結果をポップアップで表示
(add-popwin-special-display-config '(grep-mode :noselect t :stick t))

;;; backtrace をポップアップで表示
(add-popwin-special-display-config '("*Backtrace*" :noselect t :stick t))

;;; メッセージバッファをポップアップで表示
(add-popwin-special-display-config '("*Messages*" :noselect t :stick t))

;;; shell コマンドの output をポップアップで表示
(add-popwin-special-display-config '("*Shell Command Output*" :noselect t :stick t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elscreen で screen を変更する前に popup window をクローズする設定

(eval-after-load 'elscreen
  '(defadvice elscreen-goto
     (before close-popwin-window-before-exec-elscreen-goto activate)
     (popwin:close-popup-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; popup させたバッファを再度 popup 表示させる

(defvar my-popwin:seq-store-count 0)
(defvar my-popwin:buffer-list '())

(defun my-popwin:repopup-target-buffer-list ()
  (loop for buf in (buffer-list)
        if (popwin:match-config buf) collect buf))

(defun my-popwin:next-buffer ()
  (if (eq last-command this-command)
      (incf my-popwin:seq-store-count)
    (setq my-popwin:seq-store-count 0)
    (setq my-popwin:buffer-list (my-popwin:repopup-target-buffer-list)))
  (if (null my-popwin:buffer-list)
      '()
    (nth (mod my-popwin:seq-store-count (length my-popwin:buffer-list))
         my-popwin:buffer-list)))

(defun my-popwin:repopup-window ()
  (interactive)
  (let ((buffer (my-popwin:next-buffer)))
    (cond
     ((null buffer)
      (message "popup target do not exist"))
     (t
      (popwin:close-popup-window)
      (popwin:display-buffer buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; バッファを popup するための helm インタフェース

(require 'helm)
(require 'helm-source)
(require 'helm-buffers)

(defvar helm-popwin-buffers-list-cache nil)

(defvar helm-source-popwin-configured-buffers-list
  (helm-make-source "Configured Buffers" 'helm-source-buffers))

(defvar my-helm-buffer-orig-init-func
  (cdr (assoc 'init helm-source-popwin-configured-buffers-list)))

(setcdr (assoc 'action helm-source-popwin-configured-buffers-list)
        '(("Popup Window" . popwin:display-buffer)))

(setcdr (assoc 'init helm-source-popwin-configured-buffers-list)
        '(lambda ()
           (funcall my-helm-buffer-orig-init-func)
           (setq helm-popwin-buffers-list-cache
                 (my-popwin:repopup-target-buffer-list))))

(setcdr (assoc 'candidates helm-source-popwin-configured-buffers-list)
        'helm-popwin-buffers-list-cache)


(defvar helm-source-popwin-buffers-list
  (helm-make-source "Buffers" 'helm-source-buffers))

(setcdr (assoc 'action helm-source-popwin-buffers-list)
        '(("Popup Window" . popwin:popup-buffer)))

(defun my-helm-popwin-buffer ()
  (interactive)
  (helm :sources '(helm-source-popwin-configured-buffers-list
                   helm-source-popwin-buffers-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; popup window 操作用コマンド

(defmacro my-popwin:with-popup-window (&rest body)
  `(if (popwin:popup-window-live-p)
       (with-selected-window popwin:popup-window
         ,@body)
     (error "No popup window displayed")))

(defmacro my-popwin:defun-command (name &rest body)
  `(defun ,name ()
     (interactive)
     (my-popwin:with-popup-window
      ,@body)))

(my-popwin:defun-command my-popwin:scroll-up
                         (scroll-up 1))
(my-popwin:defun-command my-popwin:scroll-down
                         (scroll-down 1))
(my-popwin:defun-command my-popwin:scroll-right
                         (scroll-right 1))
(my-popwin:defun-command my-popwin:scroll-left
                         (scroll-left 1))
(my-popwin:defun-command my-popwin:scroll-up-command
                         (scroll-up-command))
(my-popwin:defun-command my-popwin:scroll-down-command
                         (scroll-down-command))
(my-popwin:defun-command my-popwin:beginning-of-buffer
                         (beginning-of-buffer))
(my-popwin:defun-command my-popwin:end-of-buffer
                         (end-of-buffer))
(my-popwin:defun-command my-popwin:isearch-forward
                         (isearch-forward))
(my-popwin:defun-command my-popwin:isearch-backward
                         (isearch-backward))
(my-popwin:defun-command my-popwin:seq-home
                         (if (commandp 'seq-home)
                             (seq-home)
                           (beginning-of-line)))
(my-popwin:defun-command my-popwin:seq-home2
                         (if (commandp 'seq-home2)
                             (seq-home2)
                           (back-to-indentation)))
(my-popwin:defun-command my-popwin:seq-end
                         (if (commandp 'seq-end)
                             (seq-end)
                           (end-of-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; キーバインド

(defvar my-popwin:keymap-alist
  '(("M-P" . my-helm-popwin-buffer)
    ("p" . my-popwin:repopup-window)
    ("0" . popwin:close-popup-window)
    ("q" . popwin:close-popup-window)
    ("j" . my-popwin:scroll-up)
    ("k" . my-popwin:scroll-down)
    ("h" . my-popwin:scroll-right)
    ("l" . my-popwin:scroll-left)
    ("C-v" . my-popwin:scroll-up-command)
    ("M-v" . my-popwin:scroll-down-command)
    ("M-<" . my-popwin:beginning-of-buffer)
    ("M->" . my-popwin:end-of-buffer)
    ("C-s" . my-popwin:isearch-forward)
    ("C-r" . my-popwin:isearch-backward)
    ("C-a" . my-popwin:seq-home)
    ("M-m" . my-popwin:seq-home2)
    ("C-e" . my-popwin:seq-end)
    ("<C-return>" . popwin:select-popup-window)))

(dolist (key-cmd my-popwin:keymap-alist)
  (define-key popwin:keymap (kbd (car key-cmd)) (cdr key-cmd)))

(global-set-key (kbd "M-P") popwin:keymap)

(require 'smartrep)

(smartrep-define-key global-map "M-P"
  (cons '("C-q" . keyboard-quit)
        my-popwin:keymap-alist))

(provide 'config-popwin)
