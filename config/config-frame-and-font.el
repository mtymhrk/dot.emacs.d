;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup for frame and font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-default-frame-height nil)
(defvar my-default-frame-width nil)

(when window-system
  (cond
   ((string-match "spinel" (system-name))
    (set-frame-font "MyricaM M-10.5")
    (set-fontset-font (frame-parameter nil 'font)
                      'japanese-jisx0208
                      '("MyricaM M" . "unicode-bmp"))
    (setq my-default-frame-height 62)
    (setq my-default-frame-width 246))  ;; 3 Window を横にならべる場合
   ((string-match "amber" (system-name))
    (set-frame-font "MyricaM M-10.5")
    (set-fontset-font (frame-parameter nil 'font)
                      'japanese-jisx0208
                      '("MyricaM M" . "unicode-bmp"))
    (setq my-default-frame-height 43)
    (setq my-default-frame-width 163)) ;; 2 Window を横にならべる場合
   (t
    (set-frame-font "DejaVu Sans Mono-9")
    (set-fontset-font (frame-parameter nil 'font)
                      'japanese-jisx0208
                      '("Takaoゴシック" . "unicode-bmp"))
    (setq my-default-frame-height 65)
    (setq my-default-frame-width 80))) ;; 1 Window 設定

  (push `(height . ,my-default-frame-height) initial-frame-alist)
  (push `(width . ,my-default-frame-width) initial-frame-alist)
  (setq default-frame-alist initial-frame-alist)

  (defun resize-frame-to-default-size ()
    (interactive)
    (let ((frame (selected-frame)))
      (set-frame-height frame my-default-frame-height)
      (set-frame-width frame my-default-frame-width))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ウィンドウを等分割するコマンド

(defun my-split-window-horizontally-n (n)
  (interactive "nNumber of Windows: ")
  (dotimes (i (- n 1))
    (split-window-horizontally))
  (balance-windows))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 自宅デスクトップ PC の場合はウィンドウを 3 分割、Note PC の場合はウィン
;;; ドウを 2 分割する

(defun my-default-window-split ()
  (when (and window-system (one-window-p))
    (cond
     ((>= (frame-width) 184)
      (my-split-window-horizontally-n 2))
     (t
      ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 起動時点でフレームを最大化し、ウィンドウを分割する

(defun after-init-hook--setup-for-frame-and-window ()
  (modify-frame-parameters (selected-frame) initial-frame-alist)
  (toggle-frame-maximized)
  ;; フレームが最大化されるのを待つ
  ;; (toggle-frame-maximaized 直後にフレームを分割すると等分割されないため)
  (sleep-for 0.5)
  (my-default-window-split))

(when window-system
  (add-hook 'after-init-hook 'after-init-hook--setup-for-frame-and-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


