;;; popup-keybindings.el --- popup a tip of key bindings

;; http://github.com/m2ym/auto-complete
(require 'popup)

;;; User variables

(defvar popup-kbs-tip-delay 1.5
  "*Delay to popup a tip.")

(defvar popup-kbs-tip-height 50
  "*Maximum height of a tip.")

(defvar popup-kbs-tip-display-currnet-keys nil
  "*If non-nil, current key events are displayed on a tip.
If nil, omitted from a tip.")

(defvar popup-kbs-tip-disable-modes nil
  "*List of major modes in which a tip for key bindings doesn't popup.")

;;; Internal variables

(defconst popup-kbs-cache (make-hash-table :test 'equal))
(defvar popup-kbs-current-maps nil)
(defvar popup-kbs-tip-timer nil)

;;;

(defun kill-line-without-mod-kill-ring (&optional arg)
  (interactive "P")
  (delete-region (point)
               ;; It is better to move point to the other end of the kill
               ;; before killing.  That way, in a read-only buffer, point
               ;; moves across the text that is copied to the kill ring.
               ;; The choice has no effect on undo now that undo records
               ;; the value of point from before the command was run.
               (progn
                 (if arg
                     (forward-visible-line (prefix-numeric-value arg))
                   (if (eobp)
                       (signal 'end-of-buffer nil))
                   (let ((end
                          (save-excursion
                            (end-of-visible-line) (point))))
                     (if (or (save-excursion
                               ;; If trailing whitespace is visible,
                               ;; don't treat it as nothing.
                               (unless show-trailing-whitespace
                                 (skip-chars-forward " \t" end))
                               (= (point) end))
                             (and kill-whole-line (bolp)))
                         (forward-visible-line 1)
                       (goto-char end))))
                 (point))))

;;; Collect key bindings

(defun popup-kbs-align-buffer (regexp)
  (let ((min (point-min))
        (re (concat ".*\\(" regexp "\\)"))
        mb lst target gap)
    (goto-char min)
    (while (looking-at re)
      (push (- (match-beginning 1) (point)) lst)
      (forward-line))
    (when (consp lst)
      (setq target (apply 'max lst))
      (goto-char min)
      (while (looking-at re)
        (setq mb (match-beginning 1)
              gap (- (+ (point) target) mb))
        (delete-region mb (match-end 1))
        (goto-char mb)
        (insert " ")
        (when (< 0 gap)
          (goto-char mb)
          (insert (format (concat "%" (number-to-string gap) "s") " ")))
        (forward-line)))))

(defun popup-kbs-collect-kbs-inner (key-len)
  (let ((min (point-min))
        (kill-whole-line t)
        (key-re (format "\\([^ \t\n]+ \\)\\{%d\\}" key-len))
        (omit-cur-keys (not popup-kbs-tip-display-currnet-keys))
        (sep " // ")
        buf-str lst pt)
    (goto-char min)
    (while (not (eobp))
      (cond
       ((= (char-after) ?\f)
        (kill-line-without-mod-kill-ring 5))
       ((or (and (bolp) (eolp))
            (looking-at ".+\\<self-insert-command\\>"))
        (kill-line-without-mod-kill-ring 1))
       ((looking-at "Key translations")
        (setq pt (point))
        (if (re-search-forward "\f" nil t)
            (progn
              (delete-region pt (match-end 0))
              (kill-line-without-mod-kill-ring 5))
          (delete-region pt (point-max))))
       ((and omit-cur-keys
             (looking-at key-re))
        (delete-region (match-beginning 0) (match-end 0))
        (forward-line))
       (t
        (forward-line))))
    (setq buf-str (replace-regexp-in-string "\t+" sep (buffer-string)))
    (erase-buffer)
    (insert buf-str)
    (popup-kbs-align-buffer sep)
    (buffer-substring-no-properties min (1- (point-max)))))

(defun popup-kbs-collect-kbs ()
  (let* ((keys (this-command-keys-vector))
         (key-len (length keys))
         (cur-buf (current-buffer))
         kbs)
    (cond
     ((gethash keys popup-kbs-cache))
     ((not (zerop key-len))
      (with-current-buffer (get-buffer-create " *popup-kbs*")
        (erase-buffer)
        (let ((indent-tabs-mode t))
          (describe-buffer-bindings cur-buf keys))
        (puthash keys
                 (setq kbs (popup-kbs-collect-kbs-inner key-len))
                 popup-kbs-cache))
      kbs))))

;;; Popup

(defun popup-kbs-clear-cache ()
  (let ((cur-maps (list major-mode
                        ;; (current-global-map)
                        ;; function-key-map
                        (current-minor-mode-maps)
                        (current-local-map))))
    (unless (equal cur-maps popup-kbs-current-maps)
      (clrhash popup-kbs-cache)
      (setq popup-kbs-current-maps cur-maps))))

(defun popup-kbs-tip ()
  "Popup a tip of key bindings."
  (unless (or (minibufferp)
              (memq major-mode popup-kbs-tip-disable-modes))
    (popup-kbs-clear-cache)
    (let ((kbs (popup-kbs-collect-kbs)))
      (and kbs (popup-tip kbs :height popup-kbs-tip-height)))))

;;; Timer

(defun popup-kbs-set-tip-timer ()
  (interactive)
  (and (numberp popup-kbs-tip-delay)
       (not (timerp popup-kbs-tip-timer))
       (setq popup-kbs-tip-timer
             (run-with-idle-timer popup-kbs-tip-delay t 'popup-kbs-tip))))

(defun popup-kbs-cancel-tip-timer ()
  (interactive)
  (when (timerp popup-kbs-tip-timer)
    (cancel-timer popup-kbs-tip-timer)
    (setq popup-kbs-tip-timer nil)))

;;; Setup

(add-hook 'after-init-hook 'popup-kbs-set-tip-timer)

(provide 'popup-keybindings)
;;; popup-keybindings.el ends here
