;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; multiple-cursors.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when-compile (require 'use-package))

(use-package multiple-cursors
  :config
  (use-package hydra
    :config
    (defhydra hydra-multiple-cursors (:hint nil)
"
^Mark^         ^Unmark^       ^Skip^         ^All^           ^Edit                      ^Quit^
^^^^^^^^-----------------------------------------------------------------------------------------
_n_: next      _u_: next      _s_: next      _*_: all        _i_: insert numbers         _q_: done
_p_: prev      _U_: prev      _S_: prev      _d_: dwim       _o_: sort regions
_m_: more      ^ ^            ^ ^            ^ ^             _O_: reverse regions
"
      ("n" mc/mark-next-like-this)
      ("p" mc/mark-previous-like-this)
      ("m" mc/mark-more-like-this-extended)
      ("u" mc/unmark-next-like-this)
      ("U" mc/unmark-previous-like-this)
      ("s" mc/skip-to-next-like-this)
      ("S" mc/skip-to-previous-like-this)
      ("*" mc/mark-all-like-this)
      ("d" mc/mark-all-like-this-dwim)
      ("i" mc/insert-numbers)
      ("o" mc/sort-regions)
      ("O" mc/reverse-regions)
      ("q" nil :eixt t))
    (bind-key "m" 'hydra-multiple-cursors/body keymap-ctrl-meta-space)))


