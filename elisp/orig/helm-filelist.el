;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-filelist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'cl))
(require 'helm)
(require 'helm-files)

(defvar helm-filelist-need-to-update-source t)

(defgroup helm-filelist nil
  ""
  :group 'helm)

(defcustom helm-filelist-file-name "/tmp/all.filelist"
  ""
  :group 'helm-filelist)

(defcustom helm-filelist-case-fold-search helm-case-fold-search
  ""
  :group 'helm-filelist)

(defcustom helm-filelist-grep-command "LANG=C grep"
  ""
  :group 'helm-filelist)

(defcustom helm-filelist-candidate-number-limit 200
  ""
  :group 'helm-filelist
  :set (lambda (variable limit)
         (set-default variable limit)
         (setq helm-filelist-need-to-update-source t)))

(defcustom helm-filelist-async t
  ""
  :group 'helm-filelist
  :set (lambda (variable flag)
         (set-default variable flag)
         (setq helm-filelist-need-to-update-source t)))

(defun helm-filelist-split-pattern (patterns)
  (delq "" (split-string patterns " ")))

(defun helm-filelist-make-grep-cmd (pattern file ignore-case)
  (format "%s %s %s %s 2>/dev/null"
          helm-filelist-grep-command
          (if ignore-case "-i" "")
          (shell-quote-argument pattern)
          (if file (shell-quote-argument file) "")))

(defun helm-filelist-ignore-case-p (pattern)
  (case helm-filelist-case-fold-search
        (smart (let ((case-fold-search nil))
                 (not (string-match-p "[A-Z]" pattern))))
        (t (if helm-filelist-case-fold-search t nil))))

(defun helm-filelist-make-cmd-line (patterns file &optional limit)
  (with-temp-buffer
    (loop for pattern in (helm-filelist-split-pattern patterns)
          for ignore-case-flag = (helm-filelist-ignore-case-p pattern)
          for i from 0
          do
          (unless (zerop i) (insert " | "))
          (insert (helm-filelist-make-grep-cmd pattern
                                               (if (zerop i) file nil)
                                               ignore-case-flag)))
    (when limit (insert (format " | head -n %d" limit)))
    (buffer-string)))

(defun helm-filelist-init-async ()
  (let ((process-connection-type))
    (prog1
        (start-process-shell-command
         "filelist-grep-process" helm-buffer
         (helm-filelist-make-cmd-line helm-pattern
                                      helm-filelist-file-name
                                      helm-filelist-candidate-number-limit))
      (set-process-sentinel
       (get-process "filelist-grep-process")
       #'(lambda (process event)
           (unless (string= event "finished\n")
             (helm-log "Error: Filelist %s"
                       (replace-regexp-in-string "\n" "" event))))))))

(defun helm-filelist-init ()
  (split-string
   (shell-command-to-string
    (helm-filelist-make-cmd-line helm-pattern
                                 helm-filelist-file-name
                                 helm-filelist-candidate-number-limit))
   "\n"))

(defclass helm-source-filelist-async (helm-source-async helm-type-file)
  ((candidates-process :initform 'helm-filelist-init-async)
   (delayed :initform nil)
   (require-pattern :initform 3)
   (keymap :initform helm-generic-files-map)
   (helm-message :initform helm-generic-file-help-message)
   (candidate-number-limit :initform helm-filelist-candidate-number-limit)))

(defclass helm-source-filelist-sync (helm-source-sync helm-type-file)
  ((candidates :initform 'helm-filelist-init)
   (volatile :initform t)
   (require-pattern :initform 3)
   (keymap :initform helm-generic-files-map)
   (helm-message :initform helm-generic-file-help-message)
   (candidate-number-limit :initform helm-filelist-candidate-number-limit)))

(defvar helm-source-filelist nil)

(defun helm-source-filelist ()
  (when helm-filelist-need-to-update-source
    (setq helm-source-filelist
          (helm-make-source "FileList"
              (if helm-filelist-async
                  'helm-source-filelist-async 'helm-source-filelist-sync)
            :candidate-number-limit helm-filelist-candidate-number-limit))
    (setq helm-filelist-need-to-update-source nil))
  helm-source-filelist)

(defun helm-filelist ()
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer (helm-source-filelist) "*helm filelist*")))

(provide 'helm-filelist)
