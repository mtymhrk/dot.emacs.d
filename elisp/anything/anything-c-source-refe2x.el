;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anything-c-source-refe2x.el
;;   http://d.hatena.ne.jp/rubikitch/20080102/rubyrefm
;;
;;   anything から、Ruby リファレンスマニュアルを参照する elisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun refe2x (kw)
  (interactive "sReFe2x: ")
  (with-current-buffer (get-buffer-create (concat "*refe2x:" kw "*"))
    (when (zerop (buffer-size))
      (call-process "refe2x" nil t t kw)
      (diff-mode))
    (setq minibuffer-scroll-window (get-buffer-window (current-buffer) t))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun anything-c-source-static-escript (symbol desc filename &rest other-attrib)
  `((name . ,desc)
    (candidates . ,symbol)
    ,@other-attrib
    (init
     . (lambda ()
         (unless (and (boundp ',symbol) ,symbol)
           (with-current-buffer (find-file-noselect ,filename)
             (setq ,symbol (split-string (buffer-string) "\n" t))))))
    (action
     ("Eval it"
      . (lambda (cand)
          (with-temp-buffer
            (insert cand)
            (cd ,(file-name-directory filename))
            (backward-sexp 1)
            (eval (read (current-buffer)))))))))
(setq anything-c-source-refe2x
      (anything-c-source-static-escript
       'anything-c-refe2x-candidates "ReFe2x"
       "~/lib/emacs/site-lisp/refe2x.e"
       '(delayed)
       '(requires-pattern . 3)))

(provide 'anything-c-source-refe2x)