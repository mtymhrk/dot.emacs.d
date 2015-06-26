;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; c-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load 'config-theme
  '(progn
     ;;; 括弧に色付けするよう設定
     (defvar my-c-paren-face 'my-paren-face)
     (defvar my-c-brace-face 'my-brace-face)
     (defvar my-c-bracket-face 'my-bracket-face)

     (eval-after-load 'cc-mode
       '(progn
          (push '("(\\|)" . my-c-paren-face) c-font-lock-keywords-3)
          (push '("{\\|}" . my-c-brace-face) c-font-lock-keywords-3)
          (push '("\\[\\|\\]" . my-c-bracket-face) c-font-lock-keywords-3)
          ))
     ))

(defun c-mode-common-hook--0 ()
  (show-paren-mode t)
  (setq indent-tabs-mode nil))

(add-hook 'c-mode-common-hook 'c-mode-common-hook--0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; build-info
;;; .dir-locals.el に build-information 変数を設定してその値を元に flycheck
;;; 等の設定をするための前準備

(require 'cl-lib)

(defvar-local build-information nil)
(defvar build-info-update-hook nil)

(defun build-info-list-of-strings-p (val)
  (and (listp val)
       (cl-loop for v in val
                always (stringp v))))

(defun build-info-safe-p (val)
  (or (null val)
      (and (listp val)
           (stringp (cdr (assoc 'project-name val)))
           (symbolp (cdr (assoc 'compiler-type val)))
           (stringp (cdr (assoc 'compiler-path val)))
           (build-info-list-of-strings-p
            (cdr (assoc 'compile-flags val)))
           (build-info-list-of-strings-p
            (cdr (assoc 'compile-warnings val)))
           (build-info-list-of-strings-p
            (cdr (assoc 'compile-quoted-include-paths val)))
           (build-info-list-of-strings-p
            (cdr (assoc 'compile-include-paths val)))
           (build-info-list-of-strings-p
            (cdr (assoc 'compile-definitions val))))))

(put 'build-information
     'safe-local-variable #'build-info-safe-p)

(defun build-info-updated ()
  (run-hooks 'build-info-update-hook))

(defun build-info-init (val)
  (when (build-info-safe-p (val))
    (setq-default build-information val)))

(defun build-info-update (val)
  (when (build-info-safe-p (val))
    (setq build-information val)
    (build-info-updated)))

(add-hook 'hack-local-variables-hook 'build-info-updated)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ff-find-other-file

(require 'cl-lib)

(defun my-setup-ff-find-other-file-with-build-info ()
  (if build-information
      (let* ((paths (append (cdr (assoc 'compile-quoted-include-paths
                                        build-information))
                            (cdr (assoc 'compile-include-paths
                                        build-information)))))
        (setq ff-search-directories
              (cons "."
                    (cl-loop for x in paths
                             collect x
                             if (string-match-p "/include/?$" x)
                             collect (concat x "/*")))))
    (setq ff-search-directories 'cc-search-directories)))


(defun c-mode-common-hook--ff-find-other-file ()
  (my-setup-ff-find-other-file-with-build-info)
  (cond ((eq major-mode 'c-mode)
         (define-key c-mode-map (kbd "C-c .") 'ff-find-other-file))
        ((eq major-mode 'c++-mode)
         (define-key c++-mode-map (kbd "C-c .") 'ff-find-other-file))))

(add-hook 'c-mode-common-hook 'c-mode-common-hook--ff-find-other-file)
(add-hook 'build-info-update-hook 'my-setup-ff-find-other-file-with-build-info)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake for c-mode

;;; 2014/09/17: flycheck へ移行

;; (eval-after-load 'config-flymake
;;   '(progn
;;      ;;; C、C++ ファイルの flymake では Makefile が用意されていればそれを利用
;;      ;;; してシンタックスチェックを行う(デフォルトの
;;      ;;; flymake-simple-make-init)。用意されていない場合は直接 gcc を呼出して
;;      ;;; チェックを行う
;;      (defvar my-flymake-gcc-warning-options
;;        '("-std=gnu99"
;;          "-Wall" "-Wextra" "-Wformat=2" "-Wstrict-aliasing=2" "-Wcast-qual"
;;          "-Wcast-align" "-Wwrite-strings" "-Wconversion" "-Wfloat-equal"
;;          "-Wpointer-arith" "-Wswitch-enum" "-Wno-unused-parameter" "-Winline"))

;;      (defun my-flymake-get-gcc-cmdline (source base-dir)
;;        (let ((args (append my-flymake-gcc-warning-options
;;                            (list "-fsyntax-only" source))))
;;          (if (string-match "\\.c\\'" source)
;;              (list "gcc" args)
;;            (list "g++" args))))

;;      (defun my-flymake-cc-init ()
;;        (let* ((build-file-name "Makefile")
;;               (source-file-name buffer-file-name)
;;               (buildfile-dir (flymake-find-buildfile
;;                               build-file-name
;;                               (file-name-directory source-file-name))))
;;          (if buildfile-dir
;;              ;; Makefile が用意されている場合
;;              (flymake-simple-make-init)
;;            ;; Makefile が用意されていない場合
;;            (flymake-get-syntax-check-program-args
;;             (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace)
;;             (file-name-directory source-file-name)
;;             t t
;;             'my-flymake-get-gcc-cmdline))))

;;      ;;; C、C++ 用の設定の初期関数を flymake-cc-init に置き換える。
;;      ;;; 初期関数が無ければ新規に追加する。
;;      (let ((mask (assoc "\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'"
;;                         flymake-allowed-file-name-masks)))
;;        (if mask
;;            (setcdr mask '(my-flymake-cc-init))
;;          (push '("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" my-flymake-cc-init)
;;                flymake-allowed-file-name-masks)))

;;      ;;; flymake-mode を有効にする
;;      (defun c-mode-common-hook--flymake ()
;;        (flymake-mode t))

;;      (add-hook 'c-mode-common-hook 'c-mode-common-hook--flymake)
;;      ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flycheck for c-mode

(eval-after-load 'config-flycheck
  '(progn

     ;;; include した先でエラー等がある場合のエラーメッセージの解析や、
     ;;; エラー情報のフォーマットに問題があったので独自に作成
     (defun my-flycheck-gcc-error-format-for-fold (err)
       (let ((file (flycheck-error-filename err))
             (line (flycheck-error-line err))
             (column (flycheck-error-column err))
             (message (flycheck-error-message err)))
         (if column
             (format "%s:%s:%s: %s" file line column message)
           (format "%s:%s: %s" file line message))))

     (defun my-flycheck-gcc-fold-include-errors (errors)
       (let (include-error)
         (dolist (err errors)
           (-when-let* ((message (flycheck-error-message err))
                        (filename (flycheck-error-filename err)))
             (cond
              ((and (string= message "In file included from")
                    (not include-error))
               (setq include-error err))
              ((and include-error (string= message "from"))
               (setq include-error err))
              (include-error
               (setf (flycheck-error-message include-error)
                     (my-flycheck-gcc-error-format-for-fold err))
               (setf (flycheck-error-level include-error)
                     (flycheck-error-level err))
               (setq include-error nil)))))
         (when include-error
           (let ((including (elt errors-in-include 0)))
             (setf (flycheck-error-message include-error)
                   (my-flycheck-gcc-format-msg-for-fold err))
             (setf (flycheck-error-level include-error)
                   (flycheck-error-level err)))))
       errors)

     ;;; check 対象がヘッダーファイルの場合、サフイックスに .c を付ける。
     ;;; gcc にコンパイル対象として .h ファイルを渡すとエラーになってし
     ;;; まうため。
     (defun my-flycheck-add-suffix-if-needed (filename suffix)
       (if (and (derived-mode-p 'c-mode)
                filename
                (string-match-p "-gcc-?"
                                (symbol-name (or flycheck-last-checker
                                                 flycheck-checker)))
                (string-match-p "\\.h$" filename))
           (concat filename suffix)
         filename))

     (defadvice flycheck-temp-file-inplace
       (before change-filename-of-headerfile activate)
       (ad-set-arg 0 (my-flycheck-add-suffix-if-needed (ad-get-arg 0) ".c")))

     (defadvice flycheck-temp-file-system
       (before change-filename-of-headerfile activate)
       (ad-set-arg 0 (my-flycheck-add-suffix-if-needed (ad-get-arg 0) ".c")))

     ;;; make の -C で指定するディレクトリ
     (defvar-local my-flycheck-make-execute-directory nil)

     ;;; my-flycheck-make-execute-directory を元に -C オプションを作成する
     (defun my-flycheck-make--C-option (opt val)
       (unless val
         (setq val "."))
       (list opt val))

     ;;; Makefile があるかどうかで checker が有効かどうかを判断する。ある場合
     ;;; はチェック実施用の準備も行う
     (defun my-find-build-file (build-file-name source-file-name)
       (and source-file-name
            (locate-dominating-file (file-name-directory source-file-name)
                                    build-file-name)))

     (defun my-flycheck-make-enable-p ()
       (or my-flycheck-make-execute-directory
           (let ((build-file-dir
                  (my-find-build-file "Makefile" buffer-file-name)))
             (if build-file-dir
                 (progn
                   (setq my-flycheck-make-execute-directory build-file-dir)
                   t)
               nil))))

     ;;; flymake のように make check-syntax を使って静的解析を行う flycheck
     ;;; の checker
     (flycheck-define-checker c/c++-make-gcc
       ""
       :command ("make"
                 "-s"
                 (option "-C"
                         my-flycheck-make-execute-directory
                         my-flycheck-make--C-option)
                 "SYNTAX_CHECK_MODE=1"
                 (eval
                  (format "CHK_SOURCES=%s"
                          (flycheck-save-buffer-to-temp
                           #'flycheck-temp-file-inplace)))
                 "check-syntax")
       :error-patterns
       ((info line-start
              (message "In file included from") " " (file-name)
              ":" line ":" column (or ":" ",") line-end)
        (info line-start
              (message (and (one-or-more " ") "from")) " " (file-name)
              ":" line ":" line-end)
        (info line-start (file-name) ":" line ":" column
              ": note: " (message) line-end)
        (warning line-start (file-name) ":" line ":" column
                 ": warning: " (message) line-end)
        (error line-start (file-name) ":" line ":" column
               ": " (or "fatal error" "error") ": " (message) line-end))
       :error-filter
       (lambda (errors)
         (my-flycheck-gcc-fold-include-errors
          (flycheck-sanitize-errors errors)))
       :modes (c-mode c++-mode)
       :predicate my-flycheck-make-enable-p
       :next-checkers ((warning . c/c++-cppcheck))
       )

     (push 'c/c++-make-gcc flycheck-checkers)

     ;;; gcc 実行時に追加したいオプションを文字列のリスト形式で設定
     (defvar-local my-flycheck-gcc-argument-appendix nil)

     ;;; gcc -iquote で指定するパスを文字列のリスト形式で設定
     (defvar-local my-flycheck-gcc-quoted-include-path nil)

     (defun my-flycheck-gcc-enable-p ()
       (or (not build-information)
           (eq (cdr (assoc 'compiler-type build-information))
               'gcc)))

     ;;; gcc による checker
     ;;; 元々 flycheck で定義してあるものでは不満があるので、再定義
     (flycheck-define-checker c/c++-gcc
       "A C/C++ syntax checker using GCC.

Requires GCC 4.8 or newer.  See URL `https://gcc.gnu.org/'."
       :command ("gcc"
                 "-fshow-column"
                 "-fno-diagnostics-show-caret" ; Do not visually indicate the source location
                 "-fno-diagnostics-show-option" ; Do not show the corresponding
                                        ; warning group
                 "-iquote" (eval (flycheck-c/c++-quoted-include-directory))
                 (option-list "-iquote" my-flycheck-gcc-quoted-include-path)
                 (option "-std=" flycheck-gcc-language-standard concat)
                 (option-flag "-fno-exceptions" flycheck-gcc-no-exceptions)
                 (option-flag "-fno-rtti" flycheck-gcc-no-rtti)
                 (option-list "-include" flycheck-gcc-includes)
                 (option-list "-W" flycheck-gcc-warnings concat)
                 (option-list "-D" flycheck-gcc-definitions concat)
                 (option-list "-I" flycheck-gcc-include-path)
                 (eval my-flycheck-gcc-argument-appendix)
                 "-x" (eval
                       (pcase major-mode
                         (`c++-mode "c++")
                         (`c-mode "c")))
                 source
                 ;; GCC performs full checking only when actually compiling, so
                 ;; `-fsyntax-only' is not enough. Just let it generate assembly
                 ;; code.
                 "-S" "-o" null-device)
       :error-patterns
       ((info line-start
              (message "In file included from") " " (file-name)
              ":" line ":" column (or ":" ",") line-end)
        (info line-start
              (message (and (one-or-more " ") "from")) " " (file-name)
              ":" line ":" line-end)
        (info line-start (file-name) ":" line ":" column
              ": note: " (message) line-end)
        (warning line-start (file-name) ":" line ":" column
                 ": warning: " (message) line-end)
        (error line-start (file-name) ":" line ":" column
               ": " (or "fatal error" "error") ": " (message) line-end))
       :error-filter
       (lambda (errors)
         (my-flycheck-gcc-fold-include-errors
          (flycheck-sanitize-errors errors)))
       :modes (c-mode c++-mode)
       :predicate my-flycheck-gcc-enable-p
       :next-checkers ((warning . c/c++-cppcheck)))


     ;;; build-information 変数 (.dir-locals.el で設定)が設定されている場合は
     ;;; それらの変数を使って、flycheck 実行のためのセットアップを行なう
     (defun my-setup-flycheck-with-build-info ()
       (when (and build-information
                  (eq 'gcc (cdr (assoc 'compiler-type build-information))))
         (setq flycheck-gcc-warnings
               (cdr (assoc 'compile-warnings
                           build-information)))
         (setq my-flycheck-gcc-quoted-include-path
               (cdr (assoc 'compile-quoted-include-paths
                           build-information)))
         (setq flycheck-gcc-include-path
               (cdr (assoc 'compile-include-paths build-information)))
         (setq flycheck-gcc-definitions
               (cdr (assoc 'compile-definitions build-information)))
         (setq my-flycheck-gcc-argument-appendix
               (cdr (assoc 'compile-flags build-information)))))


     (defun c-mode-common-hook--flycheck ()
       (my-setup-flycheck-with-build-info)
       (flycheck-mode t))

     (add-hook 'c-mode-common-hook 'c-mode-common-hook--flycheck)
     (add-hook 'build-info-update-hook 'my-setup-flycheck-with-build-info)
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gtags for c-mode

(eval-after-load 'config-gtags
  '(progn
     ;;; gtags-mode ではなく helm-gtags-mode を使用する
     ;; (add-hook 'c-mode-common-hook 'gtags-mode)
     (add-hook 'c-mode-common-hook 'helm-gtags-mode)
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-complete for c-mode

(eval-after-load 'config-auto-complete
  '(progn
     ;;; c-mode でのカーソル位置の文字削除で auto-complete が起動するのを抑制
     (push 'c-electric-delete-forward ac-non-trigger-commands)
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; c-eldoc

;;; c-eldoc は本家のものではなく、deferred.el を使ってプリプロセッサを非同期
;;; 実行するバージョンを使用している。(https://github.com/mooz/c-eldoc)

(require 'c-eldoc)

(add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode)

(defun my-setup-c-eldoc-with-build-info ()
  (when build-information
    (setq-local c-eldoc-includes
                (mapconcat
                 (lambda (x) (concat "-I " x))
                 (cdr (assq 'compile-include-paths build-information))
                 " "))))

(add-hook 'build-info-update-hook 'my-setup-c-eldoc-with-build-info)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1 行の文字数上限を越えた場合にハイライトする

(require 'highlight-column-limit)

(hcl:add-hook c-mode-common-hook 80)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-c-mode)
