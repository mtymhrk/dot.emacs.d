;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm
;;;   https://github.com/emacs-helm/helm/wiki
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm-config)


(require 'helm) ; helm-map を参照するには helm.el のロードが必要

;;; minibuffer の入力にて C-h を一文字削除に割り当てる
;; (define-key helm-map (kbd "C-h") 'delete-backward-char)

;;; 選択されている要素の色がドフォルトでは見づらかったので変更する
(set-face-background 'helm-selection "DeepSkyBlue4")
;; (set-face-background 'helm-selection "firebrick4")
;; (set-face-background 'helm-selection "DarkGreen")
;; (set-face-background 'helm-selection "cyan4")

;;; helm-scroll-other-window のスクロール量を 1 行に設定
(setq helm-scroll-amount 1)

(define-key helm-map (kbd "C-M-j") 'helm-scroll-other-window)
(define-key helm-map (kbd "C-M-k") 'helm-scroll-other-window-down)
(define-key helm-map (kbd "C-w") 'backward-kill-word)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-find-files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; helm-find-files のキーバインドを変更
;;;   C-h: 一文字削除
;;;   C-w: ディレクトリ一つ分の文字を削除
(require 'helm-files) ; helm-find-files-map を参照するため

;;; 自動補完を行わない
(custom-set-variables '(helm-ff-auto-update-initial-value nil))

(define-key helm-generic-files-map (kbd "C-w") 'backward-kill-word)

;;; C-h で 1 文字削除
;; (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
;;; C-w で直前のディレクトを削除
(define-key helm-find-files-map (kbd "C-w") 'helm-find-files-down-one-level)
;;; tab、C-i で補完
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 2015/01/12:
;;;   ido-ubiquitous を試用、helm-mode の使用を中止

(require 'helm-mode)

;;; helm-mode が有効でも、ファイルの選択では helm を起動させない設定
(defun my-read-file-name (&rest _)
  (helm-mode -1)
  (unwind-protect
      (apply 'read-file-name-default _)
    (helm-mode 1)))

(defadvice helm-mode (after reset-read-file-name activate)
  (when helm-mode
    (setq read-file-name-function 'my-read-file-name)))

;;; 上記の設定でも ffap は helm が起動してしまうので、find-file-at-point を
;;; ブラックリストへ追加
(push '(find-file-at-point . nil) helm-completing-read-handlers-alist)

;; ;;; helm-read-file-name を決め打ちで呼び出している helm コマンドがあるので
;; ;;; helm-read-file-name の挙動を変える
;; (defadvice helm-read-file-name (around disable-helm-read-file-name activate)
;;   (read-file-name (ad-get-arg 0)))

;; (helm-mode 1)

;;; C-h で 1 文字削除
;; (define-key helm-read-file-map (kbd "C-h") 'delete-backward-char)
;;; C-w で直前のディレクトを削除
(define-key helm-read-file-map (kbd "C-w") 'helm-find-files-down-one-level)
;;; tab、C-i で補完
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

;;; helm-mode を有効な場合、find-file を実行し、無効な場合は
;;; find-file-at-point を実行するコマンド
;; (defun my-helm-C-x-C-f ()
;;   "This command executes `find-file' when helm-mode is enabled, otherwise this execute `find-file-at-point'"
;;   (interactive)
;;   (call-interactively (if helm-mode 'find-file 'find-file-at-point)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm バッファを split-root で表示する設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar helm-compilation-window-height-percent 50.0)

;; (require 'split-root nil t) ;; http://nschum.de/src/emacs/split-root/

;; (defun helm-compilation-window-root (buf)
;;   (if helm-full-frame (switch-to-buffer buf)
;;     (progn
;;       (setq helm-compilation-window
;;             (split-root-window
;;              (truncate (* (window-height)
;;                           (/ helm-compilation-window-height-percent
;;                              100.0)))))
;;       (set-window-buffer helm-compilation-window buf))))

;; (defadvice helm (before close-popwin-window-before-exec-helm activate)
;;   (when (featurep 'popwin) (popwin:close-popup-window)))

;; (setq helm-display-function 'helm-compilation-window-root)

(require 'popwin)

(defun helm-display-function--popwin (buf)
  (if helm-full-frame
      (switch-to-buffer buf)
    (popwin:popup-buffer buf
                         :height (/ helm-compilation-window-height-percent
                                    100.0))))

(setq helm-display-function 'helm-display-function--popwin)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm-buffers)

;;; helm-souce-buffers-list が helm-buffers-list コマンドを実行するまで設定
;;; されないようになったため、あらかじめ設定しておく
(unless helm-source-buffers-list
  (setq helm-source-buffers-list
        (helm-make-source "Buffers" 'helm-source-buffers)))

;;; buffer のリストが名前の長さでソートされるのを防ぐ
(defadvice helm-buffers-sort-transformer (around ignore activate)
  (setq ad-return-value (ad-get-arg 0)))

;;; buffer 名の表示領域を広くする
(setq helm-buffer-max-length 50)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; register 登録用ソース
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar helm-source-my-register-set
  '((name . "Set Register")
    (dummy)
    (no-delay-on-input)
    (action . point-to-register)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-occur 設定

;;; helm-occur から occur を起動するコマンド
(defun helm-quit-and-exec-occur ()
  (interactive)
  (lexical-let ((helm-pattern helm-pattern))
    (helm-run-after-quit
     (lambda () (occur helm-pattern)))))

(defun my-helm-override-keymap (source-sym keymap)
  (let* ((source (symbol-value source-sym))
         (k (assq 'keymap source)))
    (if (and k (not (eq k keymap)))
        (progn (set-keymap-parent keymap (cdr k))
               (setcdr k keymap))
      (set source-sym
           (cons (cons 'keymap keymap) source)))))

(defvar my-helm-occur-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") 'helm-quit-and-exec-occur)
    map))

(helm-occur-init-source)
(my-helm-override-keymap 'helm-source-occur my-helm-occur-keymap)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffer 内移動用
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm-imenu)
(require 'helm-bookmark)
(require 'helm-ring)
(require 'helm-regexp)

(defvar helm-source-occur-for-move-in-buf
  (helm-make-source "Occur" 'helm-source-multi-occur))

(defvar my-helm-occur-for-move-in-buf-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-w") 'backward-kill-word)
    (define-key map (kbd "C-c o") 'helm-quit-and-exec-occur)
    map))

(let ((attr (assq 'candidate-number-limit helm-source-occur-for-move-in-buf)))
  (if attr
      (setcdr attr 100)
    (push '(candidate-number-limit . 100) helm-source-occur-for-move-in-buf)))

(my-helm-override-keymap 'helm-source-occur-for-move-in-buf
                         my-helm-occur-for-move-in-buf-keymap)


(defvar helm-for-movement-in-buffer-sources
  '(helm-source-imenu
    helm-source-occur-for-move-in-buf
    helm-source-bookmarks
    helm-source-register
    helm-source-bookmark-set
    helm-source-my-register-set))

(defun preproc-helm-occur (source)
 (let ((bufs (list (buffer-name (current-buffer)))))
    (helm-attrset 'moccur-buffers bufs source)
    (helm-set-local-variable 'helm-multi-occur-buffer-list bufs)
    (helm-set-local-variable
     'helm-multi-occur-buffer-tick
     (cl-loop for b in bufs
              collect (buffer-chars-modified-tick (get-buffer b))))))

(defun helm-for-movement-in-buffer ()
  (interactive)
  (preproc-helm-occur helm-source-occur-for-move-in-buf)
  (helm-other-buffer helm-for-movement-in-buffer-sources
                     "*helm for movement in buffer*"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; マニュアル参照
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm-man)
(require 'helm-info)

(defvar helm-for-manuals-sources
  '(helm-source-man-pages
    helm-source-info-pages))

(dolist (src helm-for-manuals-sources)
  (add-to-list 'helm-sources-using-default-as-input src))

(defun helm-for-manuals ()
  (interactive)
  (helm-other-buffer helm-for-manuals-sources "*helm for manuals"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-filelist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (concat user-emacs-directory "elisp/helm-third-party"))

(require 'helm-filelist)

(custom-set-variables
 '(helm-filelist-file-name "/tmp/anything-filelist.all.filelist")
 '(helm-filelist-async t))

;; ;;; helm-for-files の source に含まれる locate を filelist に置き換える
;; (setq helm-for-files-preferred-list
;;       (loop for source in helm-for-files-preferred-list
;;             collect
;;             (if (eq 'helm-source-locate source)
;;                 (helm-source-filelist)
;;               source)))

;; ;;; helm-for-files の source に fielist を追加する
;; (setq helm-for-files-preferred-list
;;       (append helm-for-files-preferred-list (list (helm-source-filelist))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; キーバインド
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar helm-my-command-keymap (make-keymap))

;; (global-set-key (kbd "C-;") 'helm-for-files)
(global-set-key (kbd "C-;") 'helm-filelist+)
(global-set-key (kbd "C-M-;") 'helm-for-movement-in-buffer)
(global-set-key (kbd "C-'") helm-my-command-keymap)
(global-set-key (kbd "C-M-'") 'helm-resume)
;; (global-set-key (kbd "C-x C-f") 'my-helm-C-x-C-f)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-X") 'execute-extended-command)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-o") 'helm-occur)

(define-key helm-my-command-keymap (kbd "C-y") 'helm-show-kill-ring)
(define-key helm-my-command-keymap (kbd "C-M-s") 'helm-regexp)
(define-key helm-my-command-keymap (kbd "a") 'helm-apropos)
(define-key helm-my-command-keymap (kbd "z") 'helm-elscreen)
(define-key helm-my-command-keymap (kbd "m") 'helm-for-manuals)
(define-key helm-my-command-keymap (kbd "g") 'helm-do-grep)


(eval-after-load 'config-elscreen
  '(progn
     (global-set-key (kbd "C-z C-z") 'helm-elscreen)
     ))

(provide 'config-helm)
