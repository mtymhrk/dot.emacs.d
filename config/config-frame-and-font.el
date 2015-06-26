;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup for frame and font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-default-frame-height nil)
(defvar my-default-frame-width nil)

(when window-system
  (cond
   ((string-match "^2[34]\." emacs-version)  ;; setup for Emacs23 and
    (cond
     ((string-match "spinel" (system-name))
      (set-frame-font "ricty-10.5")
      (set-fontset-font (frame-parameter nil 'font)
                        'japanese-jisx0208
                        '("ricty" . "unicode-bmp"))
      (setq my-default-frame-height 62)
      (setq my-default-frame-width 246))  ;; 3 Window を横にならべる場合
     ((string-match "amber" (system-name))
      (set-frame-font "ricty-10.5")
      (set-fontset-font (frame-parameter nil 'font)
                        'japanese-jisx0208
                        '("ricty" . "unicode-bmp"))
      (setq my-default-frame-height 43)
      (setq my-default-frame-width 163)) ;; 2 Window を横にならべる場合
     ((string-match "helblindi" (system-name))
      (set-frame-font "Inconsolata-9")
      (set-fontset-font (frame-parameter nil 'font)
                        'japanese-jisx0208
                        '("Takaoゴシック" . "unicode-bmp"))
      (setq my-default-frame-height 65)
      (setq my-default-frame-width 163)) ;; 2 Window を横にならべる場合
     (t
      (set-frame-font "DejaVu Sans Mono-9")
      (set-fontset-font (frame-parameter nil 'font)
                        'japanese-jisx0208
                        '("Takaoゴシック" . "unicode-bmp"))
      (setq my-default-frame-height 65)
      (setq my-default-frame-width 80))) ;; 1 Window 設定
    )
   (t  ;; setup for Emacs22
    (create-fontset-from-fontset-spec
     "-misc-fixed-medium-r-normal--12-*-*-*-*-*-fontset-12,
        ascii:-mplus-fxd-medium-r-*-*-12-*-*-*-*-*-iso8859-*,
        japanese-jisx0208:-mplus-gothic-medium-r-normal-*-12-*-*-*-*-*-jisx0208.1990-*,
        korean-ksc5601:-*-mincho-medium-r-normal--16-*-*-*-*-*-ksc*-*,
        chinese-gb2312:-*-fang*-medium-r-normal--16-*-*-*-*-*-gb2312*-*")
    (set-frame-font "fontset-12")
    (setq my-default-frame-height 70)
    (setq my-default-frame-width 80)
    (push '(font . "-misc-fixed-medium-r-normal--12-*-*-*-*-*-fontset-12")
          initial-frame-alist)
    ))
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

(defun split-window-horizontally-n (n)
  (interactive "nNumber of Windows: ")
  ;; ww 分割後の個々のウィンドウの幅
  ;;   (* 3 (- n 1)) を引いているのは Window 境界線を作る毎に finge 等で
  ;;   幅を 3 使うため。(例えば、3 分割する場合、境界線は 2 つできるので
  ;;    2 * 3 の 6 を引く)
  (let ((ww (/ (- (window-width) (* 3 (- n 1))) n)))
    (dotimes (i (- n 1))
      (split-window-horizontally (- (window-width) ww)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 自宅デスクトップ PC の場合はウィンドウを 3 分割、Note PC の場合はウィン
;;; ドウを 2 分割する

(defun my-default-window-split ()
  (when (and window-system (one-window-p))
    (cond
     ((string-match "spinel" (system-name))
      (split-window-horizontally-n 3))
     ((string-match "helblindi" (system-name))
      (split-window-horizontally-n 2))
     ((string-match "amber" (system-name))
      (split-window-horizontally-n 2)))))

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

(provide 'config-frame-and-font)
