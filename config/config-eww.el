;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eww
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package eww
  :config
  (require 'mod-eww)
  (setq eww-search-prefix "https://www.google.co.jp/search?q=")
  :bind
  (:map eww-mode-map
        ("h" . backword-char)
        ("j" . next-line)
        ("k" . previous-line)
        ("l" . forward-char)
        ("n" . scroll-up)
        ("p" . scroll-down)
        ("r" . eww-reload)
        ("<" . eww-back-url)
        (">" . eww-forward-url)))

