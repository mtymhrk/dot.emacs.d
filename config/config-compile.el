;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compilation-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package compile
  :config
  (setq compile-command "make")
  (setq compile-history (list "make" "make clean"))

  ;; compilation-mode で ansi color が化けてしまうことへの対処
  (add-hook 'compilation-mode-hook #'ansi-color-for-comint-mode-on)
  (add-hook 'compilation-filter
            #'(lambda ()
                (let ((start-marker (make-marker))
                      (end-marker (process-mark (get-buffer-process
                                                 (current-buffer)))))
                  (set-marker start-marker (point-min))
                  (ansi-color-apply-on-region start-marker end-marker))))

  ;; コンパイルプロセスの出力を追ってコンパイルバッファをスクロースする
  (setq compilation-scroll-output t)

  :bind
  (:map mode-specific-map
        ;; C-c c で compile コマンドを呼び出す
        ("c" . compile)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-compile)
