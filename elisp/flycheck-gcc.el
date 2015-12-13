
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

(defun flycheck-gcc:fold-include-errors (errors)
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
                (flycheck-gcc:error-format-for-fold err))
          (setf (flycheck-error-level include-error)
                (flycheck-error-level err))
          (setq include-error nil)))))
    (when include-error
      (let ((including (elt errors-in-include 0)))
        (setf (flycheck-error-message include-error)
              (flycheck-gcc:error-format-for-fold err))
        (setf (flycheck-error-level include-error)
              (flycheck-error-level err)))))
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

(defadvice flycheck-gcc:flycheck-temp-file-inplace
    (before change-filename-of-headerfile activate)
  (ad-set-arg 0 (flycheck-gcc:add-suffix-if-needed (ad-get-arg 0) ".c")))

(defadvice flycheck-gcc:temp-file-system
    (before change-filename-of-headerfile activate)
  (ad-set-arg 0 (flycheck-gcc:add-suffix-if-needed (ad-get-arg 0) ".c")))

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
    (flycheck-gcc:fold-include-errors
     (flycheck-sanitize-errors errors)))
  :modes (c-mode c++-mode)
  :next-checkers ((warning . c/c++-cppcheck)))


(provide 'flycheck-gcc)
