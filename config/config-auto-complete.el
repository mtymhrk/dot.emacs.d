;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-complete.el
;;;   http://www.emacswiki.org/emacs/AutoComplete
;;;   http://github.com/m2ym/auto-complete
;;;   git://github.com/m2ym/auto-complete.git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'auto-complete nil)
(require 'auto-complete-config)

(ac-config-default)

(global-auto-complete-mode t)

;;; コメントや文字列の中でも auto-complete を有効にする
(setq ac-disable-faces nil)

;;; 補完機能起動コマンドをバインド
;;;   ac-auto-start の設定による自動起動では fuzzy.el を使用した曖昧マッチを利用
;;;   できない。曖昧マッチを利用したい時にこのキーバインドを叩く
(let ((map ac-mode-map))
  (define-key map (kbd "M-TAB") 'auto-complete)
  (define-key map (kbd "ESC <tab>") 'auto-complete))

;;; 補完機能起動時のキーマップ設定
(let ((map ac-completing-map))
  (define-key map "\t" 'ac-expand)
  (define-key map [tab] 'ac-expand)
  (define-key map "\C-j" 'ac-complete) ; C-j で compelte
  (define-key map "\r" nil)      ; Enter キーでは通常の改行
  (define-key map "\C-m" nil)    ; Enter キーでは通常の改行
  (define-key map [return] nil)  ; Enter キーでは通常の改行
  )

;;; 補完メニュー表示時のみ使用するキーマップの有効化
(setq ac-use-menu-map t)

;;; 補完メニュー表示時のキーマップ設定
(let ((map ac-menu-map))
  (define-key map (kbd "C-n") nil)  ; C-n でのメニュー選択を無効化
  (define-key map (kbd "C-p") nil)  ; C-p でのメニュー選択を無効化
  (define-key map (kbd "C-M-i") 'ac-previous)  ; TAB, C-i でメニューを下に移動
                                               ; できるので、C-M-i で上に移動
                                               ; できるよう設定
  )

;;; 2 文字入力すると自動的に補完機能を起動する
(setq ac-auto-start 2)

;;; 補完機能の起動と同時に補完候補のメニューを表示する
;; (setq ac-auto-show-menu t)

;;; Do Waht I Mean 機能を off に
(setq ac-dwim nil)

;;; auto-complete-mode を有効にする対象に scheme-mode を追加
(add-to-list 'ac-modes 'scheme-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ac-use-comphist が有効な場合、最初の TAB(C-i) 入力で先頭の補完候補で補完さ
;;; れ、補完が完了してしまう。最初の TAB(C-i) 入力では補完候補の共通部分だけを
;;; 補完するようにするための advice 設定
(defadvice ac-candidates (after overwrite-ac-common-part activate)
  (when ac-use-comphist
    (setq ac-common-part ac-whole-common-part)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gtags による補完候補の生成で生成の度に global コマンドを実行しないように
;;; source を変更

(defvar ac-gtags-candidate-limit 30)
(defvar ac-gtags-cache-update-interval 600.0)

(defvar ac-gtags-cache-table nil)

(defvar-local ac-gtags-symbols nil)
(defvar-local ac-gtags-dbpath nil)

(defun ac-gtags-get-dbpath ()
  (car (split-string (shell-command-to-string "global -pq") "\n")))

(defun ac-gtags-get-symbols ()
  (split-string (shell-command-to-string "global -cq")))

(defun ac-gtags-update-data (data)
  (setcar data (float-time))
  (setcdr data (ac-gtags-get-symbols)))

(defun ac-gtags-new-data ()
  (let ((data (cons nil nil)))
    (ac-gtags-update-data data)
    data))

(defun ac-gtags-add-cache (dbpath data)
  (let ((cache (cons dbpath data)))
    (push cache ac-gtags-cache-table)
    cache))

(defun ac-gtags-init-internal (dbpath)
  (setq ac-gtags-dbpath dbpath)
  (setq ac-gtags-symbols
          (if (string= ac-gtags-dbpath "")
              'no-gtags-found
            (let ((cache (assoc ac-gtags-dbpath ac-gtags-cache-table)))
              (unless cache
                (setq cache
                      (ac-gtags-add-cache ac-gtags-dbpath
                                          (ac-gtags-new-data))))
              (cdr cache)))))

(defun ac-gtags-init ()
  (unless ac-gtags-symbols
    (ac-gtags-init-internal (ac-gtags-get-dbpath))
    (message "ac-gtags: initialized")))

(defun ac-gtags-update ()
  (interactive)
  (let ((dbpath (ac-gtags-get-dbpath)))
    (cond
     ((string= dbpath "")
      (ac-gtags-init-internal dbpath))
     ((string= ac-gtags-dbpath dbpath)
      (setcar ac-gtags-symbols (float-time))
      (setcdr ac-gtags-symbols (ac-gtags-get-symbols)))
     (t
      (setq ac-gtags-symbols
            (let ((data (cdr (assoc dbpath ac-gtags-cache-table))))
              (if (not data)
                  (cdr (ac-gtags-add-cache dbpath (ac-gtags-new-data)))
                (ac-gtags-update-data data)
                data)))
      (setq ac-gtags-dbpath dbpath)))
    (message "ac-gtags: updated")))

(defun ac-gtags-match-func (prefix candidates)
  (let ((x (nthcdr (1- ac-gtags-candidate-limit) candidates)))
    (when x
      (setcdr x nil)))
  candidates)

(push (cons 'match ac-match-function) ac-source-gtags)

(defadvice ac-gtags-candidate (around use-cache disable)
  (ac-gtags-init)
  (setq ad-return-value
        (if (eq ac-gtags-symbols 'no-gtags-found)
            nil
          (all-completions ac-prefix (cdr ac-gtags-symbols)))))

(defun ac-gtags-handle-save ()
  (when (and (consp ac-gtags-symbols)
             (> (- (float-time) (car ac-gtags-symbols))
                ac-gtags-cache-update-interval))
    (ac-gtags-update)))

(defun ac-gtags-add-after-save-hook ()
  (if auto-complete-mode
      (add-hook 'after-save-hook 'ac-gtags-handle-save nil t)
    (remove-hook 'after-save-hook 'ac-gtags-handle-save t)))

(defun ac-gtags-enable-to-use-cache ()
  (interactive)
  (ad-enable-advice 'ac-gtags-candidate 'around 'use-cache)
  (ad-activate 'ac-gtags-candidate)
  (setcdr (assq 'match ac-source-gtags) 'ac-gtags-match-func)
  (add-hook 'auto-complete-mode-hook 'ac-gtags-add-after-save-hook))

(defun ac-gtags-disable-to-use-cache ()
  (interactive)
  (ad-disable-advice 'ac-gtags-candidate 'around 'use-cache)
  (ad-activate 'ac-gtags-candidate)
  (setcdr (assq 'match ac-source-gtags) ac-match-function)
  (remove-hook 'auto-complete-mode-hook 'ac-gtags-add-after-save-hook))

(ac-gtags-enable-to-use-cache)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-auto-complete)
