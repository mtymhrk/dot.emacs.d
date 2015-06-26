;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compilation-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq compile-command "make")
(setq compile-history (list "make" "make clean"))

;;; コンパイルプロセスの出力を追ってコンパイルバッファをスクロースする
(setq compilation-scroll-output t)

;;; C-c c で compile コマンドを呼び出す
(define-key mode-specific-map "c" 'compile)

;;; compilation-mode で ansi color が化けてしまうことへの対処
(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'compilation-filter-hook
          '(lambda ()
             (let ((start-marker (make-marker))
                   (end-marker (process-mark (get-buffer-process
                                              (current-buffer)))))
               (set-marker start-marker (point-min))
               (ansi-color-apply-on-region start-marker end-marker))))


;;; popwin による表示方法に移行したためコメントアウト
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; to pop up compilation buffers at the bottom (OPTIONAL)
;; ;;   ref: http://d.hatena.ne.jp/grandVin/20090825/1251180023
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (when (require 'split-root nil t)
;;   (require 'compile)

;;   (defvar compilation-window nil
;;     "The window opened for displaying a compilation buffer.")

;;   (setq compilation-window-height 24)

;;   ;;; for emacs23
;;   (defun my-split-window-by-split-root (window)
;;     (if (or (compilation-buffer-p buffer)
;;             (equal name-of-buffer "*Shell Command Output*"))
;;         (split-root-window compilation-window-height)
;;       (split-window-sensibly window)))
;;   ;; (setq split-window-preferred-function 'my-split-window-by-split-root)

;;   ;;; for emacs21,22
;;   ;; (defun my-display-buffer (buffer &optional not-this-window)
;;   ;;   (if (or (compilation-buffer-p buffer)
;;   ;;           (equal (buffer-name buffer) "*Shell Command Output*"))
;;   ;;       (progn
;;   ;;         (unless (and my-compilation-window (window-live-p my-compilation-window))
;;   ;;           (setq my-compilation-window (split-root-window compilation-window-height))
;;   ;;           (set-window-buffer my-compilation-window buffer))
;;   ;;         my-compilation-window)
;;   ;;     (let ((display-buffer-function nil))
;;   ;;       (display-buffer buffer not-this-window))))
;;   ;; (setq display-buffer-function 'my-display-buffer)

;;   )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; to close compilation window
;; ;;   Ref: Http://Murakan.Cocolog-Nifty.com/blog/emacs/index.html
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun compilation-buffer-close ()
;;   "*compilation*バッファを表示しているwindowをクローズする"
;;   (interactive)
;;   (let ((com-buffer (get-buffer "*compilation*")))
;;     (if com-buffer
;;         (let ((com-window (get-buffer-window com-buffer)))
;;           (if com-window
;;               (delete-window com-window))))))

;; (define-key mode-specific-map "\M-c" 'compilation-buffer-close)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-compile)
