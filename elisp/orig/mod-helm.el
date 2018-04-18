;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mod-helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mod-helm:command-keymap (make-sparse-keymap))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-occur のキーバインド追加

(with-eval-after-load 'helm-regexp

  ;; helm を終了して occur を起動するコマンド
  (defun helm-quit-and-exec-occur ()
    (interactive)
    (lexical-let ((helm-pattern helm-pattern))
      (helm-run-after-quit
       #'(lambda () (occur helm-pattern)))))

  (define-key helm-moccur-map (kbd "C-c o") 'helm-quit-and-exec-occur))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer 内移動用 helm

(require 'helm-imenu)
(require 'helm-bookmark)
(require 'helm-ring)
(require 'helm-regexp)

;;;
(defvar mod-helm:source-register-set
  '((name . "Set Register")
    (dummy)
    (no-delay-on-input)
    (action . point-to-register)))

;;; occur
(defvar mod-helm:occur-for-move-in-buf-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-moccur-map)
    (define-key map (kbd "C-w") 'backword-kill-word)
    map))

(defclass mod-helm:source-occur-for-move-in-buf (helm-source-multi-occur)
  ((keymap :initform  mod-helm:occur-for-move-in-buf-map)
   (candidate-number-limit :initform 100)))

(defvar mod-helm:source-occur-fmib
  (helm-make-source "Occur" 'mod-helm:source-occur-for-move-in-buf))


(defvar mod-helm:move-in-buffer-sources
  '(helm-source-imenu
    mod-helm:source-occur-fmib
    helm-source-bookmarks
    helm-source-register
    helm-source-bookmark-set
    mod-helm:source-register-set))

(defun mod-helm:preproc-occur (source)
 (let ((bufs (list (buffer-name (current-buffer)))))
    (helm-attrset 'moccur-buffers bufs source)
    (helm-set-local-variable 'helm-multi-occur-buffer-list bufs)
    (helm-set-local-variable
     'helm-multi-occur-buffer-tick
     (cl-loop for b in bufs
              collect (buffer-chars-modified-tick (get-buffer b))))
    (helm-set-local-variable 'helm-occur--invisible
                           (null helm-occur-show-buffer-name))))

(defun mod-helm:move-in-buffer ()
  (interactive)
  (mod-helm:preproc-occur mod-helm:source-occur-fmib)
  (helm-other-buffer mod-helm:move-in-buffer-sources
                     "*helm for movement in buffer*"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; マニュアル参照

(require 'helm-man)
(require 'helm-info)

(defvar mod-helm:manuals-sources
  '(helm-source-man-pages
    helm-source-info-pages))

(dolist (src mod-helm:manuals-sources)
  (add-to-list 'helm-sources-using-default-as-input src))

(defun mod-helm:manuals ()
  (interactive)
  (helm-other-buffer mod-helm:manuals-sources "*helm for manuals*"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-filelist

(require 'helm-filelist)
(require 'helm-files)
(require 'helm-for-files)

(defun mod-helm:filelist ()
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer  `(helm-source-buffers-list
                          helm-source-recentf
                          helm-source-file-cache
                          helm-source-files-in-current-dir
                          ,(helm-source-filelist))
                        "*helm filelist+*")))

(provide 'mod-helm)
