; first change 2013-02-28
; last change 2014-11-26

(defmodule montyhall
  (export (run 0)))

(defun game ()
  (let ((actual-loc-of-car (random:uniform 3))
        (loc-picked-by-player (random:uniform 3)))
    (if (== actual-loc-of-car loc-picked-by-player)
      0
      1)))

(defun run ()
  (montecarlo:run #'game/0))
