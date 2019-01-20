;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  mod-popwin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'cl-lib))

(require 'popwin)

(defun mod-popwin:delete-display-config (pattern)
  (let ((x (assoc pattern popwin:special-display-config)))
    (when x
      (setq popwin:special-display-config
            (delq x popwin:special-display-config)))))

(defun mod-popwin:add-display-config (config)
  (mod-popwin:delete-display-config (car config))
  (push config popwin:special-display-config))


;;
;; elscreen で screen を変更する前に popup window をクローズする設定
;;
(with-eval-after-load 'elscreen
  (defun mod-popwin:close-popwin-advice (&rest args)
    (popwin:close-popup-window))
  (advice-add 'elscreen-goto
              :before 'mod-popwin:close-popwin-advice))

;;
;; popup させたバッファを再度 popup 表示させる
;;
(defvar mod-popwin:seq-store-count 0)
(defvar mod-popwin:buffer-list '())

(defun mod-popwin:repopup-target-buffer-list ()
  (cl-loop for buf in (buffer-list)
           if (popwin:match-config buf) collect buf))

(defun mod-popwin:next-buffer ()
  (if (eq last-command this-command)
      (cl-incf mod-popwin:seq-store-count)
    (setq mod-popwin:seq-store-count 0)
    (setq mod-popwin:buffer-list (mod-popwin:repopup-target-buffer-list)))
  (if (null mod-popwin:buffer-list)
      '()
    (nth (mod mod-popwin:seq-store-count (length mod-popwin:buffer-list))
         mod-popwin:buffer-list)))

(defun mod-popwin:repopup-window ()
  (interactive)
  (let ((buffer (mod-popwin:next-buffer)))
    (cond
     ((null buffer)
      (message "popup target do not exist"))
     (t
      (popwin:close-popup-window)
      (popwin:display-buffer buffer)))))

;;
;; popup window 操作用コマンド
;;
(defmacro mod-popwin:with-popup-window (&rest body)
  `(if (popwin:popup-window-live-p)
       (with-selected-window popwin:popup-window
         ,@body)
     (error "No popup window displayed")))

(defmacro mod-popwin:defun-command (name &rest body)
  `(defun ,name ()
     (interactive)
     (mod-popwin:with-popup-window
      ,@body)))

(mod-popwin:defun-command mod-popwin:scroll-up
                          (scroll-up 1))
(mod-popwin:defun-command mod-popwin:scroll-down
                          (scroll-down 1))
(mod-popwin:defun-command mod-popwin:scroll-right
                          (scroll-right 1))
(mod-popwin:defun-command mod-popwin:scroll-left
                          (scroll-left 1))
(mod-popwin:defun-command mod-popwin:scroll-up-command
                          (scroll-up-command))
(mod-popwin:defun-command mod-popwin:scroll-down-command
                          (scroll-down-command))
(mod-popwin:defun-command mod-popwin:beginning-of-buffer
                          (beginning-of-buffer))
(mod-popwin:defun-command mod-popwin:end-of-buffer
                          (end-of-buffer))
(mod-popwin:defun-command mod-popwin:isearch-forward
                          (isearch-forward))
(mod-popwin:defun-command mod-popwin:isearch-backward
                          (isearch-backward))
(mod-popwin:defun-command mod-popwin:seq-home
                          (if (commandp 'seq-home)
                              (seq-home)
                            (beginning-of-line)))
(mod-popwin:defun-command mod-popwin:seq-home2
                          (if (commandp 'seq-home2)
                              (seq-home2)
                            (back-to-indentation)))
(mod-popwin:defun-command mod-popwin:seq-end
                          (if (commandp 'seq-end)
                              (seq-end)
                            (end-of-line)))

;; ウィンドウがポップアップされていれば何もせず、ポップアップされていれば、
;; repopup-window を実行するコマンド
(defun mod-popwin:popwin-begin ()
  (interactive)
  (unless (popwin:popup-window-live-p)
    (mod-popwin:repopup-window)))

(provide 'mod-popwin)
