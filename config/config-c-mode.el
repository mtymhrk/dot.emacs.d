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

(require 'build-info)

(build-info:setup-default '((project-name . "unknown")
                            (compiler-type . gcc)
                            (compiler-path . "gcc")
                            (compile-flags . ("-g" "-std=gnu99" "-O2"))
                            (compile-warnings . ("all" "extra" "format=2" "strict-aliasing=2" "cast-qual" "cast-align" "write-strings" "conversion" "float-equal" "pointer-arith" "switch-enum" "no-unused-parameter" "no-format-nonliteral"))
                            (compile-quoted-include-paths . nil)
                            (compile-include-paths . nil)
                            (compile-definitions . nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ff-find-other-file

(defun c-mode-common-hook--ff-find-other-file ()
  (cond ((eq major-mode 'c-mode)
         (define-key c-mode-map (kbd "C-c .") 'ff-find-other-file))
        ((eq major-mode 'c++-mode)
         (define-key c++-mode-map (kbd "C-c .") 'ff-find-other-file))))

(add-hook 'c-mode-common-hook 'c-mode-common-hook--ff-find-other-file)


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
    (require 'flycheck-gcc)

    (defun c-mode-common-hook--flycheck ()
      (flycheck-mode t))

    (add-hook 'c-mode-common-hook 'c-mode-common-hook--flycheck)))


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


     ;;; auto-complete-c-headers の設定
     (require 'auto-complete-c-headers)
     (require 'build-info)

     (setq my:achead:include-directories-c
           '("/usr/lib/gcc/x86_64-linux-gnu/4.8/include"
             "/usr/local/include"
             "/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed"
             "/usr/include/x86_64-linux-gnu"
             "/usr/include"
             "."))

     (setq my:achead:include-directories-c++
           '("/usr/include/c++/4.8"
             "/usr/include/x86_64-linux-gnu/c++/4.8"
             "/usr/include/c++/4.8/backward"
             "/usr/lib/gcc/x86_64-linux-gnu/4.8/include"
             "/usr/local/include"
             "/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed"
             "/usr/include/x86_64-linux-gnu"
             "/usr/include"
             "."))

     (setcdr (assq 'c-mode build-info:auto-complete-c-headers:directories)
             my:achead:include-directories-c)
     (setcdr (assq 'c++-mode build-info:auto-complete-c-headers:directories)
             my:achead:include-directories-c++)

     (defun my:achead:init ()
       (unless (featurep 'build-info)
         (make-local-variable 'achead:include-directories)
         (cond ((eq major-mode 'c-mode)
                (setq achead:include-directories my:achead:include-directories-c))
               ((eq major-mode 'c++-mode)
                (setq achead:include-directories my:achead:include-directories-c++))))
       (add-to-list 'ac-sources 'ac-source-c-headers))

     (add-hook 'c-mode-common-hook 'my:achead:init)
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; c-eldoc
;;; c-eldoc は本家のものではなく、deferred.el を使ってプリプロセッサを非同期
;;; 実行するバージョンを使用している。(https://github.com/mooz/c-eldoc)

(require 'c-eldoc)
(require 'build-info)

(add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1 行の文字数上限を越えた場合にハイライトする

(require 'highlight-column-limit)

(hcl:add-hook c-mode-common-hook 80)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-c-mode)
