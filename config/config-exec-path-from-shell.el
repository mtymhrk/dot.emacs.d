;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; exec-path-from-shell
;;;   PATH 等の環境変数を shell の値をもとに設定する
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package exec-path-from-shell
  :custom
  ;; 取得する変数
  (exec-path-from-shell-variables '("PATH" "MANPATH" "INFOPATH"))

  :config
  ;; NTEmacs 用設定
  (when (eq system-type 'windows-nt)
    (defvar my:cygwin-bin-path
      "D:/lib/gnupack_devel-13.01-2015.05.10/app/cygwin/cygwin/bin/")
    (setq shell-file-name (concat my:cygwin-bin-path "bash"))
    (setenv "SHELL" shell-file-name)

    ;; 環境変数 PATH のときのみパスの変換を実行
    (defun ad-exec-path-from-shell-setenv (orig-fun &rest args)
      (when (string=  (car args) "PATH")
        (setf (nth 1 args)
              (with-temp-buffer
                (call-process (concat my:cygwin-bin-path "cygpath")
                              nil '(t nil) nil "-amp" (nth 1 args))
                (unless (bobp)
                  (goto-char (point-min))
                  (buffer-substring-no-properties (point)
                                                  (line-end-position))))))
      (apply orig-fun args))
    (advice-add 'exec-path-from-shell-setenv
                :around 'ad-exec-path-from-shell-setenv))

  (exec-path-from-shell-initialize))


(provide 'config-exec-path-from-shell)
