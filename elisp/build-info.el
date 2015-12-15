
(require 'cl-lib)

(defvar build-info:enabled-major-mode '(c-mode c++mode))

(defvar-local build-info:information nil)
(defvar build-info:update-hook nil)

(defun build-info:enable-p ()
  (if (and (memq major-mode build-info:enabled-major-mode)
           build-info:information)
      t nil))

(defmacro build-info:defun-accessor (acc)
  (let ((sym-get (intern (format "build-info:get-%s" acc)))
        (sym-info (intern (format "build-info:info-%s" acc))))
    `(progn
       (defun ,sym-get (val)
         (cdr (assoc (quote ,acc) val)))
       (defun ,sym-info ()
         (,sym-get build-info:information)))))

(build-info:defun-accessor project-name)
(build-info:defun-accessor compiler-type)
(build-info:defun-accessor compiler-path)
(build-info:defun-accessor compile-flags)
(build-info:defun-accessor compile-warnings)
(build-info:defun-accessor compile-quoted-include-paths)
(build-info:defun-accessor compile-include-paths)
(build-info:defun-accessor compile-definitions)

(defun build-info:list-of-strings-p (val)
  (and (listp val)
       (cl-loop for v in val
                always (stringp v))))

(defun build-info:safe-p (val)
  (or (null val)
      (and (listp val)
           (stringp (build-info:get-project-name val))
           (symbolp (build-info:get-compiler-type val))
           (stringp (build-info:get-compiler-path val))
           (build-info:list-of-strings-p (build-info:get-compile-flags val))
           (build-info:list-of-strings-p (build-info:get-compile-warnings val))
           (build-info:list-of-strings-p
            (build-info:get-compile-quoted-include-paths val))
           (build-info:list-of-strings-p
            (build-info:get-compile-include-paths val))
           (build-info:list-of-strings-p
            (build-info:get-compile-definitions val)))))

(put 'build-info:information
     'safe-local-variable #'build-info:safe-p)

(defun build-info:updated ()
  (run-hooks 'build-info:update-hook))

(defun build-info:init (val)
  (when (build-info:safe-p (val))
    (setq-default build-info:information val)))

(defun build-info:setup-default (val)
  (when (build-info:safe-p val)
    (setq-default build-info:information val)))

(defun build-info:update (val)
  (when (build-info:safe-p val)
    (setq build-info:information val)
    (build-info:updated)))

(add-hook 'hack-local-variables-hook 'build-info:updated)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ff-find-other-file

(defun build-info:setup-ff-find-other-file ()
  (when (build-info:enable-p)
    (let* ((paths (append (build-info:info-compile-quoted-include-paths)
                          (build-info:info-compile-include-paths))))
      (setq-local ff-search-directories
                  (cons "."
                        (cl-loop for x in paths
                                 collect x
                                 if (string-match-p "/include/?$" x)
                                 collect (concat x "/*")))))))

(defun build-info:enable-ff-find-other-file ()
  (dolist (hook '(c-mode-common-hook build-info:update-hook))
    (add-hook hook 'build-info:setup-ff-find-other-file)))

(defun build-info:disable-ff-find-other-file ()
  (dolist (hook '(c-mode-common-hook build-info:update-hook))
    (remove-hook hook 'build-info:setup-ff-find-other-file)))

(build-info:enable-ff-find-other-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flycheck

(defvar build-info:flycheck-variables
  '(flycheck-gcc-warnings flycheck-gcc-include-path flycheck-gcc-definitions
    flycheck-gcc-args flycheck-clang-warnings flycheck-clang-include-path
    flycheck-clang-definitions flycheck-clang-args))

(defun build-info:setup-flycheck-args ()
  (append (build-info:info-compile-flags)
          (cl-mapcar (lambda (x) (format "-iquote %s" x))
                     (build-info:info-compile-quoted-include-paths))))

(defun build-info:setup-flycheck ()
  (when (and (featurep 'flycheck) (build-info:enable-p))
    (dolist (var build-info:flycheck-variables)
      (make-local-variable var))
    (cond ((or (eq 'gcc (build-info:info-compiler-type))
               (eq 'clang (build-info:info-compiler-type)))
           (setq flycheck-gcc-warnings (build-info:info-compile-warnings))
           (setq flycheck-gcc-include-path
                 (build-info:info-compile-include-paths))
           (setq flycheck-gcc-definitions (build-info:info-compile-definitions))
           (setq flycheck-gcc-args (build-info:setup-flycheck-args))

           (setq flycheck-clang-warnings flycheck-gcc-warnings)
           (setq flycheck-clang-include-path flycheck-gcc-include-path)
           (setq flycheck-clang-definitions flycheck-gcc-definitions)
           (setq flycheck-clang-args flycheck-gcc-args)))
    (cond ((eq 'gcc (build-info:info-compiler-type))
           (flycheck-select-checker 'c/c++-gcc))
          ((eq 'clang (build-info:info-compiler-type))
           (flycheck-select-checker 'c/c++-clang)))
    (flycheck-buffer-deferred)))

(defun build-info:enable-flycheck ()
  (dolist (hook '(c-mode-common-hook build-info:update-hook))
    (add-hook hook 'build-info:setup-flycheck)))

(defun build-info:disable-flycheck ()
  (dolist (hook '(c-mode-common-hook build-info:update-hook))
    (remove-hook hook 'build-info:setup-flycheck)))

(eval-after-load 'flycheck
  '(progn
     (build-info:enable-flycheck)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; c-eldoc

(defun build-info:setup-c-eldoc ()
  (when (and (featurep 'c-eldoc) (build-info:enable-p))
    (setq-local c-eldoc-includes
                (mapconcat
                 (lambda (x) (concat "-I " x))
                 (build-info:info-compile-include-paths)
                 " "))))

(defun build-info:enable-c-eldoc ()
  (dolist (hook '(c-mode-common-hook build-info:update-hook))
    (add-hook hook 'build-info:setup-c-eldoc)))

(defun build-info:disable-c-eldoc ()
  (dolist (hook '(c-mode-common-hook build-info:update-hook))
    (remove-hook hook 'build-info:setup-c-eldoc)))

(eval-after-load 'c-eldoc
  '(progn
     (build-info:enable-c-eldoc)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-complete-c-headers

(defvar build-info:auto-complete-c-headers:directories
  '((c-mode . ("." "/usr/include" "/usr/local/include"))
    (c++-mode . ("." "/usr/include" "/usr/local/include"))))

(defun build-info:setup-achead ()
  (when (and (featurep 'auto-complete-c-headers) (build-info:enable-p))
    (make-local-variable 'achead:include-directories)
    (setq achead:include-directories
          (append (cdr (assq major-mode
                             build-info:auto-complete-c-headers:directories))
                  (build-info:info-compile-quoted-include-paths)
                  (build-info:info-compile-include-paths)))))

(defun build-info:enable-auto-complete-c-headers ()
  (dolist (hook '(c-mode-common-hook build-info:update-hook))
    (add-hook hook 'build-info:setup-achead)))

(defun build-info:disable-auto-complete-c-headers ()
  (dolist (hook '(c-mode-common-hook build-info:update-hook))
    (remove-hook hook 'build-info:setup-achead)))

(eval-after-load 'auto-complete-c-headers
  '(progn
     (build-info:enable-auto-complete-c-headers)))

(provide 'build-info)
