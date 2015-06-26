;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything.el
;;;   http://www.emacswiki.org/cgi-bin/wiki/download/anything.el
;;;   http://www.emacswiki.org/cgi-bin/wiki/download/anything-config.el
;;;   http://www.emacswiki.org/cgi-bin/wiki/download/anything-migemo.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'anything)
(require 'anything-config)
(require 'anything-migemo)

(define-key anything-map (kbd "C-p") 'anything-previous-line)
(define-key anything-map (kbd "C-n") 'anything-next-line)
(define-key anything-map (kbd "C-v") 'anything-next-source)
(define-key anything-map (kbd "M-v") 'anything-previous-source)
(define-key anything-map (kbd "C-z") 'anything-execute-persistent-action)
(define-key anything-map (kbd "C-r") 'anything-select-source)

;; (anything-iswitchb-setup)  ; うまく動かない

(defvar anything-my-command-keymap (make-keymap))

(global-set-key (kbd "C-;")   'anything-filelist+)
(global-set-key (kbd "C-x b") 'anything-for-buffers)
(global-set-key (kbd "M-y")   'anything-show-kill-ring)
(global-set-key (kbd "C-'")   anything-my-command-keymap)
(global-set-key (kbd "C-M-'") 'anything-resume)

(define-key anything-my-command-keymap (kbd "C-y")   'anything-show-kill-ring)
(define-key anything-my-command-keymap (kbd "C-M-s") 'anything-regexp)
(define-key anything-my-command-keymap (kbd "a") 'anything-apropos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 候補の最大表示数
(setq anything-candidate-number-limit 100)
;;; 候補が多いときに体感速度を早くする
(setq anything-quick-update t)
;;; 候補選択のショートカットをアルファベットにする
(setq anything-enable-shortcuts 'alphabet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-show-kill-ring で選択対象となる文字列の長さの閾値
(setq anything-kill-ring-threshold 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup anything-sources
;; (setq anything-sources '(anything-c-source-buffers
;;                          anything-c-source-imenu
;;                          anything-c-source-semantic
;;                          anything-c-source-register
;;                          anything-c-source-bookmarks
;;                          anything-c-source-recentf
;;                          anything-c-source-files-in-current-dir
;; ;;                         anything-c-source-file-cache
;;                          anything-c-source-man-pages
;;                          anything-c-source-info-pages
;; ;;                         anything-c-source-rurima
;; ;;                         anything-c-source-emacs-commands
;;                          anything-c-source-calculation-result
;; ;;                       anything-c-source-private-locate
;;                          anything-c-source-locate
;;                          ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; anything-for-files の source をカスタマイズ
;; (setq anything-for-files-prefered-list
;;       '(anything-c-source-ffap-line
;;         anything-c-source-ffap-guesser
;;         anything-c-source-buffers+
;;         anything-c-source-imenu
;;         anything-c-source-semantic
;;         anything-c-source-register
;;         anything-c-source-recentf
;;         anything-c-source-bookmarks
;;         anything-c-source-file-cache
;;         anything-c-source-files-in-current-dir+
;;         anything-c-source-locate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-files-in-current-dir+ のファイルの face が見づらいので変更
(defface my-anything-file-name
  '((t (:foreground "light sky blue")))
  "*Face used for file names (without suffixes) in dired buffers."
  :group 'anything)
(setq anything-c-files-face2 'my-anything-file-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-filelist(anything-fileslit+) の設定
(setq anything-c-filelist-file-name
      "/tmp/anything-filelist.all.filelist")
;;; ↓を設定すると何故か重くなる(候補表示時に固まる)のでコメントアウト
;;;   → insert-LANG=C-into-agp-command-line の advice を有効にすると改善され
;;;      ので設定を有効化
(setq anything-grep-candidates-fast-directory-regexp "^/tmp")

;;; anything-filelist 速度改善用 advice
;;; 日本語のロケールよりも英語のロケールの方が grep が速いので filelist のソー
;;; スで使用している grep-candidates が grep 検索時に英語ロケールで実行するよ
;;; う、grep-candidates が生成する grep コマンドラインに "LANG=C" を付ける
;;; advice を定義
(defadvice agp-command-line (after insert-LANG=C-into-agp-command-line)
  (while (string-match "\\(^\\|| \\)\\(grep -ih\\)" ad-return-value)
    (setq ad-return-value (replace-match "LANG=C \\2" t nil ad-return-value 2))))
;;; anything-filelist 速度改善用 advice
;;; grep の結果が candidate-number-limit の数 (200) を越える場合、200 個の一
;;; 致行を出力した後、Broken pipe エラーを出力しつづけるため、それを抑制する
;;; ために標準エラー出力を null デバイスへリダイレクトする advice を定義
(defadvice agp-command-line (after insert-Redirection-of-STDERR-into-agp-command-line)
  (when (string-match "| head -n" ad-return-value)
    (setq ad-return-value (replace-match "2>/dev/null \\&" t nil ad-return-value nil))))

(ad-activate 'agp-command-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'filecache)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for anything-c-source-recentf
(setq recentf-max-saved-items 1000)
(setq recentf-exclude '("/TAGS$" "/var/tmp/"))
;;; (install-elisp-from-emacswiki "recentf-ext.el")
(require 'recentf-ext)
;; (recentf-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything バッファを split-root で表示する設定

;;; split-window
(defun anything-split-window (buf)
  (split-window)
  (other-window 1)
  (switch-to-buffer buf))

;;; split-root
(require 'split-root) ;; http://nschum.de/src/emacs/split-root/
(defvar anything-compilation-window-height-percent 50.0)
(defun anything-compilation-window-root (buf)
  (if anything-samewindow (switch-to-buffer buf)
    (progn
      (setq anything-compilation-window
            (split-root-window
             (truncate (* (window-height)
                          (/ anything-compilation-window-height-percent
                             100.0)))))
      (set-window-buffer anything-compilation-window buf))))

;; (setq anything-display-function 'anything-split-window)
(setq anything-display-function 'anything-compilation-window-root)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffer 内移動用 anything
(defvar anything-for-movement-in-buffer-sources
  '(anything-c-source-imenu
    anything-c-source-semantic
    anything-c-source-bookmarks
    anything-c-source-register
    anything-c-source-bookmark-set
    anything-c-source-my-register-set))

(defun anything-for-movement-in-buffer ()
  (interactive)
  (anything-other-buffer anything-for-movement-in-buffer-sources "*anything for movement in buffer*"))

(global-set-key (kbd "C-M-;") 'anything-for-movement-in-buffer)
(define-key anything-my-command-keymap
  (kbd "b") 'anything-for-movement-in-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-for-manuals
(defvar anything-for-manuals-sources
  '(anything-c-source-man-pages
    anything-c-source-info-pages))

(defun anything-for-manuals ()
  (interactive)
  (anything anything-for-manuals-sources))

(define-key anything-my-command-keymap (kbd "m") 'anything-for-manuals)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-for-elscreen
(defun anything-for-elscreen ()
  (interactive)
  (anything anything-c-source-elscreen))

(define-key anything-my-command-keymap (kbd "z") 'anything-for-elscreen)
(global-set-key (kbd "C-z C-z") 'anything-for-elscreen)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-match-plugin.el
;;;   候補の絞り込みに複数の正規表現を書けるようにする elisp
;;;   http://d.hatena.ne.jp/rubikitch/20080908/anything
;;;   http://www.emacswiki.org/cgi-bin/wiki/download/anything-match-plugin.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'anything-match-plugin)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-compelte.el
;;;   様々な補完を anything.el のインターフェースで行う elisp
;;;   http://d.hatena.ne.jp/rubikitch/20080908/anything
;;;   http://www.emacswiki.org/cgi-bin/wiki/download/anything-complete.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'anything-complete)
(anything-lisp-complete-symbol-set-timer 150)
(anything-read-string-mode 1)

;;; emacs コマンドの実行を anything.el インタフェースで行う
;; (global-set-key (kbd "C-x M-x") 'anything-execute-extended-command)


;;; anything-find-file に追加する source
(setq anything-find-file-additional-sources
      '(anything-c-source-recentf anything-c-source-locate))


(define-key anything-my-command-keymap (kbd "C-f") 'anything-find-file)
(define-key anything-map (kbd "C-' C-f") 'anything-quit-and-find-file)
(define-key anything-map (kbd "C-f") 'anything-quit-and-find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-kry.el
;;;   http://d.hatena.ne.jp/rubikitch/20090222/anything_kyr
;;;   http://www.emacswiki.org/cgi-bin/wiki/download/anything-kyr.el
;;;   http://www.emacswiki.org/cgi-bin/wiki/download/anything-kyr-config.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'anything-kyr-config)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-grep.el
;;;   http://d.hatena.ne.jp/rubikitch/20090106/anythinggrep
;;;   http://www.emacswiki.org/cgi-bin/wiki/download/anything-grep.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'anything-grep)

(define-key anything-my-command-keymap (kbd "g") 'anything-grep) ;ディレクトリ

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; descbinds-anythint.el
;;;   change interface of descirbe-bindings (\M-? b) to anythings interface
;;;   http://d.hatena.ne.jp/buzztaiki/20081115/1226760184
;;;   http://www.emacswiki.org/cgi-bin/wiki/download/descbinds-anything.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'descbinds-anything)
(descbinds-anything-install)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-c-moccur.el
;;;   enable cooperation color-moccur and anything
;;;   http://d.hatena.ne.jp/IMAKADO/20080724/1216882563
;;;   http://svn.coderepos.org/share/lang/elisp/anything-c-moccur/trunk/anything-c-moccur.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (featurep 'color-moccur)
  (require 'anything-c-moccur)
  (setq anything-c-moccur-anything-idle-delay 0.2
        anything-c-moccur-higligt-info-line-flag t
        anything-c-moccur-enable-auto-look-flag t
        anything-c-moccur-enable-initial-pattern t
        anything-c-moccur-push-mark-flag t)

;;;  anything-c-moccur-higligt-info-line-flag
;;;      anything-c-moccur-dmoccur などのコマンドでバッファの情報をハイライト
;;;      する
;;;  anything-c-moccur-enable-auto-look-flag
;;;      現在選択中の候補の位置を他のwindowに表示する
;;;  anything-c-moccur-enable-initial-pattern
;;;      anything-c-moccur-occur-by-moccur の起動時にポイントの位置の単語を初
;;;      期パターンにする
;;;  anything-c-moccur-push-mark-flag
;;;      anything-c-moccur-by-moccur の検索結果からバッファを移動した後 C-x C-x で検索する前の
;;;      位置に戻ることができる

  (global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur) ; バッファ内検索
  (define-key anything-my-command-keymap
    (kbd "M-o") 'anything-c-moccur-occur-by-moccur)
  (global-set-key (kbd "C-M-o") 'anything-c-moccur-dmoccur) ;ディレクトリ
  (define-key anything-my-command-keymap
    (kbd "C-M-o") 'anything-c-moccur-dmoccur) ;ディレクトリ

  ;;; 本来、インクリメンタルサーチ中に C-o で occur に移行するところを
  ;;; anything-c-moccur-by-moccur に移行するよう変更し、旧来の
  ;;; isearch-occur は C-M-o へ引越し
  (define-key isearch-mode-map (kbd "C-o") 'anything-c-moccur-by-moccur)
  (define-key isearch-mode-map (kbd "C-M-o") 'isearch-occur)

  (add-hook 'dired-mode-hook ;dired
            '(lambda ()
               (local-set-key (kbd "O")
                              'anything-c-moccur-dired-do-moccur-by-moccur))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-gtags.el
;;;   gtags.el の anything.el インタフェース
;;;   http://d.hatena.ne.jp/rubikitch/20080908/anything
;;;   http://www.emacswiki.org/cgi-bin/wiki/download/anything-gtags.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'anything-gtags)
(setq anything-gtags-enable-initial-pattern t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-rcodetools
;;;   http://d.hatena.ne.jp/rubikitch/20080104/1199438501
;;;   http://www.emacswiki.org/cgi-bin/wiki/download/anything-rcodetools.el
;;;   http://d.hatena.ne.jp/rubikitch/20080817/1218958046
;;;   http://rubyforge.org/frs/download.php/41362/rcodetools-0.8.1.0.tar.gz
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'anything-rcodetools)
(setq rct-get-all-methods-command "PAGER=cat fri -l")
(add-hook 'ruby-mode-hook
  '(lambda ()
     (local-set-key (kbd "M-C-i") 'rct-complete-symbol)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-c-source-refe2x
;;;   http://d.hatena.ne.jp/rubikitch/20080102/rubyrefm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-rurima で置き換えのため コメントアウト

;; (require 'anything-c-source-refe2x)

;; (setq anything-for-manuals-sources
;;       (append anything-for-manuals-sources
;;               '(anything-c-source-refe2x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-rurima
;;;   http://d.hatena.ne.jp/rubikitch/20080102/rubyrefm
;;;   http://www.emacswiki.org/cgi-bin/wiki/download/anything-rurima.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'anything-rurima)
(setq anything-rurima-index-file "~/usr/lib/rurima/rubydoc/rurima.e")

;;; M-x anything-rurima でプロンプトが出てくるのでマニュアルを検索する。
;;; M-x anything-rurima-at-point でカーソル位置の単語をるりまで検索することができる。

;;; anything-for-manuals で検索できるように source に追加
(add-to-list 'anything-for-manuals-sources
             'anything-c-source-rurima)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-c-yasnippet
;;;   http://d.hatena.ne.jp/IMAKADO/20080401/1206715770
;;;   http://svn.coderepos.org/share/lang/elisp/anything-c-yasnippet/anything-c-yasnippet.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 今は動かない
(require 'anything-c-yasnippet)
(setq anything-c-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'anything-c-yas-complete)
(setq anything-c-yas-display-key-on-candidate t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-call-source-buffer
;;;    http://d.hatena.ne.jp/rubikitch/20090128/1233141924
;;;    2009-2-12: Comment: anything-select-source を使うことにしたため不要
;;;    2009-2-15: Comment: anything-select-source と統合
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; comment: anything-config.el に含まれるようになったので不要
;; anything-source を選択して anything を起動する anything-source
;; (defvar anything-call-source-buffer "*anything source select*")
;; (defvar anything-c-source-call-source
;;   `((name . "Call anything source")
;;     (candidate-number-limit . 9999)
;;     (candidates
;;      . (lambda ()
;;          (loop for vname in (all-completions "anything-c-source-" obarray)
;;                for var = (intern vname)
;;                for name = (ignore-errors (assoc-default 'name (symbol-value var)))
;;                if name collect (cons (format "%s (%s)" name vname) var))))
;;     (action . (("Invoke anything with selected source" .
;;                 (lambda (candidate)
;;                   (anything candidate nil nil nil nil
;;                             anything-call-source-buffer)))
;;                ("Describe variable" . describe-variable)))
;;     (persistent-action . describe-variable)))

;; (defun anything-call-source ()
;;   "Call anything source."
;;   (interactive)
;;   (anything 'anything-c-source-call-source nil nil nil nil
;;             anything-call-source-buffer))

;; (global-set-key (kbd "C-'") 'anything-call-source)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ac-anything
;;;   auto-compelte の補完候補を anything で絞り込むことができる elisp
;;;   http://www.emacswiki.org/cgi-bin/wiki/download/ac-anything.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (featurep 'auto-complete)
  (require 'ac-anything)
  (define-key ac-complete-mode-map (kbd "C-c u") 'ac-complete-with-anything))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-hatena-bookmark
;;;   anything ではてなブックマークを検索
;;;   http://trac.codecheck.in/share/export/1401/lang/elisp/anything-hatena-bookmark/trunk/anything-hatena-bookmark.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'anything-hatena-bookmark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  windows.el の windows を anything から選択する
;;;    http://www.emacswiki.org/emacs/RubikitchAnythingConfiguration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (featurep 'windows)
  (defvar anything-c-source-windows-select
    '((name . "windows.el")
      (configs . 9)
      (init . (lambda () (win:switch-window win:current-config)))
      (candidates
       . (lambda ()
           (loop for i from 1 to (anything-attr 'configs)
                 for key = (+ win:base-key i)
                 for name-prefix = (aref win:names-prefix i)
                 for name = (aref win:names i)
                 when (and name (aref win:configs i))
                 collecting (cons (format "[%c]%s:%s" key name-prefix name) i))))
      (action
       ("Switch" .
        (lambda (n)
          (unless (= win:current-config n)
            (win-switch-to-window 1 n)))))))
  ;; (cond ((aref win:configs (1+ win:current-config))
  ;;        (win:switch-window (1+ win:current-config))
  ;;        (unless (= win:current-config n) (win-swap-with (+ win:base-key n))))
  ;;       (t
  ;;        (let ((cc win:current-config))
  ;;          (win:switch-window (1+ cc) nil t)
  ;;          (win:set-wc n)
  ;;          (win:switch-window n)
  ;;          (win-delete-current-window nil)
  ;;          (win:squeeze-config)
  ;;          (win:switch-window cc)
  ;;          (win-tab-updater)))))))))

  (defun anything-c-windows-select ()
    (interactive)
    (anything 'anything-c-source-windows-select))

  (define-key win:switch-map "u" 'anything-c-windows-select))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  register や bookmark の anything
;;;    参考: http://d.hatena.ne.jp/jimo1001/20090921/1253481388
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar anything-c-source-my-register-set
      '((name . "Set Register")
        (dummy)
        (action . point-to-register)))

(defun anything-c-select-bookmarks-and-registers ()
  (interactive)
  (anything (list
             'anything-c-source-bookmarks
             'anything-c-source-register
             'anything-c-source-bookmark-set
             'anything-c-source-my-register-set
             )))

(define-key anything-my-command-keymap
  (kbd "r") 'anything-c-select-bookmarks-and-registers)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  anything-flymake
;;;    http://namikister.blog101.fc2.com/blog-entry-21.html
;;;    http://d.hatena.ne.jp/kiris60/20091003/1254579747
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake のエラー行を anything で表示する

;;; (auto-install-from-url "https://gist.github.com/raw/396108/335c955df4641625cc9dc713ccfc138f6468cba6/anything-flymake.el")

(when (require 'anything-flymake nil t)
;; (eval-when-compile (require 'cl))
;; (require 'flymake)
;; (setq anything-c-source-flymake
;;   '((name . "Flymake")
;;     (init . (lambda ()
;;               (setq anything-flymake-err-list
;;                     (loop for err-info in flymake-err-info
;;                           for err = (nth 1 err-info)
;;                           append err))))
;;     (candidates
;;      . (lambda ()
;;          (mapcar
;;           (lambda (err)
;;             (let* ((text (flymake-ler-text err))
;;                    (line (flymake-ler-line err)))
;;               (cons (format "[%s] %s" line text) err)))
;;           anything-flymake-err-list)))
;;     (action
;;      . (("Goto line" . (lambda (candidate)
;;                          (goto-line (flymake-ler-line candidate)
;;                                     anything-current-buffer)))))))

;; (defun anything-flymake ()
;;   (interactive)
;;   (anything (list anything-c-source-flymake)))

  (set-face-background 'anything-flymake-errline "red4")
  (set-face-background 'anything-flymake-warnline "midnight blue")

  (define-key anything-my-command-keymap (kbd "`") 'anything-flymake))

(provide 'config-anything)
