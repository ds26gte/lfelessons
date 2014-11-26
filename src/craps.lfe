; last change 2014-11-26

(defmodule craps
  (export (run 0))
  )

(defun game ()
  (random:seed (now))
  (let ((throw-1 (+ (random:uniform 6)
                    (random:uniform 6))))
    (cond ((orelse (== throw-1 7) (== throw-1 11))
           1)
          ((orelse (== throw-1 2) (== throw-1 3) (== throw-1 12))
           0)
          ('true
           (fletrec ((loop ()
                           (let ((throw-i (+ (random:uniform 6) (random:uniform 6))))
                             (cond ((== throw-i throw-1) 1)
                                   ((== throw-i 7) 0)
                                   ('true (loop))))))
             (loop))))))

(defun run ()
  (montecarlo:run
     (montecarlo:timed-experiment #'game/0
        ;if taking more than
        (* 2 60 1000) ;ms
        ;let customer win
        1
        )))
