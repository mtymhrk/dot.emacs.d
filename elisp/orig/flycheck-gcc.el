
(require 'flycheck)

;;; include した先でエラー等がある場合のエラーメッセージの解析や、
;;; エラー情報のフォーマットに問題があったので独自に作成
(defun flycheck-gcc:error-format-for-fold (err)
  (let ((file (flycheck-error-filename err))
        (line (flycheck-error-line err))
        (column (flycheck-error-column err))
        (message (flycheck-error-message err)))
    (if column
        (format "%s:%s:%s: %s" file line column message)
      (format "%s:%s: %s" file line message))))

(defun flycheck-gcc:fold-include-errors-gcc (errors)
  (let (include-error)
    (dolist (err errors)
      (-when-let* ((message (flycheck-error-message err)))
        (cond
         ((or (string= message "In file included from") (string= message "from"))
          (setq include-error err))
         (include-error
          (setf (flycheck-error-message include-error)
                (flycheck-gcc:error-format-for-fold err))
          (setf (flycheck-error-level include-error)
                (flycheck-error-level err))
          (setq include-error nil))))))
  errors)

(defun flycheck-gcc:fold-include-errors-clang (errors)
  (let (include-error)
    (dolist (err errors)
      (-when-let* ((message (flycheck-error-message err)))
        (cond
         ((string= message "In file included from")
          (unless include-error
            (setq include-error err)))
         (include-error
          (setf (flycheck-error-message include-error)
                (flycheck-gcc:error-format-for-fold err))
          (setf (flycheck-error-level include-error)
                (flycheck-error-level err))
          (setq include-error nil))))))
  errors)

;;; check 対象がヘッダーファイルの場合、サフイックスに .c を付ける。
;;; gcc にコンパイル対象として .h ファイルを渡すとエラーになってし
;;; まうため。
(defun flycheck-gcc:add-suffix-if-needed (filename suffix)
  (if (and (derived-mode-p 'c-mode)
           filename
           (string-match-p "\\.h$" filename))
      (concat filename suffix)
    filename))

(defun flycheck-gcc:temp-file-inplace (filename)
  (flycheck-temp-file-inplace (flycheck-gcc:add-suffix-if-needed filename ".c")))

(defun flycheck-gcc:temp-file-system (filename)
  (flycheck-temp-file-system (flycheck-gcc:add-suffix-if-needed filename ".c")))

;;; make の -C で指定するディレクトリ
(defvar-local flycheck-gcc:execute-directory-make nil)

;;; flycheck-gcc:execute-directory-make を元に -C オプションを作成する
(defun flycheck-gcc:make--C-option (opt val)
  (unless val
    (setq val "."))
  (list opt val))

;;; Makefile があるかどうかで checker が有効かどうかを判断する。ある場合
;;; はチェック実施用の準備も行う
(defun flycheck-gcc:find-build-file (build-file-name source-file-name)
  (and source-file-name
       (locate-dominating-file (file-name-directory source-file-name)
                               build-file-name)))

(defun flycheck-gcc:make-enable-p ()
  (or flycheck-gcc:execute-directory-make
      (let ((build-file-dir
             (flycheck-gcc:find-build-file "Makefile" buffer-file-name)))
        (if build-file-dir
            (progn
              (setq flycheck-gcc:execute-directory-make build-file-dir)
              t)
          nil))))

;;; flymake のように make check-syntax を使って静的解析を行う flycheck
;;; の checker
(flycheck-define-checker c/c++-make-gcc
  ""
  :command ("make"
            "-s"
            (option "-C"
                    flycheck-gcc:execute-directory-make
                    flycheck-gcc:make--C-option)
            "SYNTAX_CHECK_MODE=1"
            (eval
             (format "CHK_SOURCES=%s"
                     (flycheck-save-buffer-to-temp
                      #'flycheck-gcc:temp-file-inplace)))
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
    (flycheck-gcc:fold-include-errors
     (flycheck-sanitize-errors errors)))
  :modes (c-mode c++-mode)
  :predicate flycheck-gcc:make-enable-p
  :next-checkers ((warning . c/c++-cppcheck))
  )

;; (push 'c/c++-make-gcc flycheck-checkers)

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
            (option "-std=" flycheck-gcc-language-standard concat)
            (option-flag "-fno-exceptions" flycheck-gcc-no-exceptions)
            (option-flag "-fno-rtti" flycheck-gcc-no-rtti)
            (option-list "-include" flycheck-gcc-includes)
            (option-list "-W" flycheck-gcc-warnings concat)
            (option-list "-D" flycheck-gcc-definitions concat)
            (option-list "-I" flycheck-gcc-include-path)
            (eval flycheck-gcc-args)
            "-x" (eval
                  (pcase major-mode
                    (`c++-mode "c++")
                    (`c-mode "c")))
            ;; GCC performs full checking only when actually compiling, so
            ;; `-fsyntax-only' is not enough. Just let it generate assembly
            ;; code.
            "-S" "-o" null-device
            ;; Read from standard input
            "-")
  :standard-input t
  :error-patterns
  ((info line-start
         (message "In file included from") " " (or "<stdin>" (file-name))
         ":" line ":" column (or ":" ",") line-end)
   (info line-start
         (message (and (one-or-more " ") "from")) " " (or "<stdin>" (file-name))
         ":" line ":" line-end)
   (info line-start (or "<stdin>" (file-name)) ":" line ":" column
         ": note: " (message) line-end)
   (warning line-start (or "<stdin>" (file-name)) ":" line ":" column
            ": warning: " (message) line-end)
   (error line-start (or "<stdin>" (file-name)) ":" line ":" column
          ": " (or "fatal error" "error") ": " (message) line-end))
  :error-filter
  (lambda (errors)
    (flycheck-gcc:fold-include-errors-gcc
     (flycheck-sanitize-errors errors)))
  :modes (c-mode c++-mode)
  :next-checkers ((warning . c/c++-cppcheck)))

;;; clang による checker
;;; 元々 flycheck で定義してあるものでは不満があるので、再定義
(flycheck-define-checker c/c++-clang
  "A C/C++ syntax checker using Clang.

See URL `http://clang.llvm.org/'."
  :command ("clang"
            "-fsyntax-only"
            "-fno-color-diagnostics"    ; Do not include color codes in output
            "-fno-caret-diagnostics"    ; Do not visually indicate the source
                                        ; location
            "-fno-diagnostics-show-option" ; Do not show the corresponding
                                        ; warning group
            "-iquote" (eval (flycheck-c/c++-quoted-include-directory))
            (option "-std=" flycheck-clang-language-standard concat)
            (option-flag "-pedantic" flycheck-clang-pedantic)
            (option-flag "-pedantic-errors" flycheck-clang-pedantic-errors)
            (option "-stdlib=" flycheck-clang-standard-library concat)
            (option-flag "-fms-extensions" flycheck-clang-ms-extensions)
            (option-flag "-fno-exceptions" flycheck-clang-no-exceptions)
            (option-flag "-fno-rtti" flycheck-clang-no-rtti)
            (option-flag "-fblocks" flycheck-clang-blocks)
            (option-list "-include" flycheck-clang-includes)
            (option-list "-W" flycheck-clang-warnings concat)
            (option-list "-D" flycheck-clang-definitions concat)
            (option-list "-I" flycheck-clang-include-path)
            (eval flycheck-clang-args)
            "-x" (eval
                  (pcase major-mode
                    (`c++-mode "c++")
                    (`c-mode "c")))
            ;; Read from standard input
            "-")
  :standard-input t
  :error-patterns
  ((info line-start
          (message "In file included from") " " (or "<stdin>" (file-name))
          ":" line ":" line-end)
   (info line-start (or "<stdin>" (file-name)) ":" line ":" column
         ": note: " (optional (message)) line-end)
   (warning line-start (or "<stdin>" (file-name)) ":" line ":" column
            ": warning: " (optional (message)) line-end)
   (error line-start (or "<stdin>" (file-name)) ":" line ":" column
          ": " (or "fatal error" "error") ": " (optional (message)) line-end))
  :error-filter
  (lambda (errors)
    (let ((errors (flycheck-sanitize-errors errors)))
      (dolist (err errors)
        ;; Clang will output empty messages for #error/#warning pragmas without
        ;; messages.  We fill these empty errors with a dummy message to get
        ;; them past our error filtering
        (setf (flycheck-error-message err)
              (or (flycheck-error-message err) "no message")))
      (flycheck-gcc:fold-include-errors-clang errors)))
  :modes (c-mode c++-mode)
  :next-checkers ((warning . c/c++-cppcheck)))

(provide 'flycheck-gcc)
