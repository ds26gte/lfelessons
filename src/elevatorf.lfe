; first change 2013-02-24
; last change 2014-11-26

(defmodule elevatorf
  (export (run 1)))

(defrecord elevator
  distance-to-gamow
  direction-seen-by-gamow)

(defun trial (num-elevators)
  (let ((gamow-height (/ 1 6)))
    (fletrec ((calc-elevs
                (i elevs-so-far)
                (if (=< i num-elevators)
                  (let* ((i-position (random:uniform))
                         (i-direction (if (< (random:uniform) 0.5) 1 -1))
                         (distance-to-gamow
                           (cond ((> i-position gamow-height)
                                  (case i-direction
                                    (+1 (+ (- 1 i-position) (- 1 gamow-height)))
                                    (-1 (- i-position gamow-height))))
                                 ((< i-position gamow-height)
                                  (case i-direction
                                    (+1 (- gamow-height i-position))
                                    (-1 (+ i-position gamow-height))))
                                 ((== i-position gamow-height)
                                  0)))
                         (direction-seen-by-gamow
                           (cond ((> i-position gamow-height) -1)
                                 ((< i-position gamow-height) 1)
                                 ((== i-position gamow-height) i-direction))))
                    (calc-elevs (+ i 1)
                                (cons (make-elevator
                                        distance-to-gamow
                                        distance-to-gamow
                                        direction-seen-by-gamow
                                        direction-seen-by-gamow)
                                      elevs-so-far)))
                  elevs-so-far)))
      (let ((elevs (calc-elevs 1 ())))
        (fletrec ((find-closest-elev
                    (i closest-so-far)
                    (if (=< i num-elevators)
                      (let ((ith-elev (lists:nth i elevs)))
                        (find-closest-elev
                          (+ i 1)
                          (if (< (elevator-distance-to-gamow ith-elev)
                                 (elevator-distance-to-gamow closest-so-far))
                            ith-elev
                            closest-so-far)))
                      closest-so-far)))
          (let ((closest-elev (find-closest-elev 2 (car elevs))))
            (if (== (elevator-direction-seen-by-gamow closest-elev) -1)
              1
              0)))))))

(defun run (num-elevators)
  (montecarlo:run (lambda () (trial num-elevators))))
