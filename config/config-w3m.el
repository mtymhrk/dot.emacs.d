;(autoload 'w3m "w3m" "Interface for w3m on Emacs" t)
;(autoload 'w3m-find-file "w3m" "w3m interface function for local file." t)
;(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;(autoload 'w3m-search "w3m-search" "Search QUERY using SEARCH-ENGINE." t)
;(autoload 'w3m-weather "w3m-weather" "Display weather report." t)
;(autoload 'w3m-antenna "w3m-antenna" "Report change of WEB sites." t)

(add-to-list 'load-path (concat user-emacs-directory "elisp/w3m"))

(require 'w3m-load)
(setq w3m-home-page "http://www.google.co.jp")
(setq w3m-search-default-engine "google")
(setq w3m-default-display-inline-images t)



