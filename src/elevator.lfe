; first change 2013-02-24
; last change 2014-11-26

(defmodule elevator
  (export (run 1)))

(defrecord elevator
  label
  distance-to-gamow
  direction-seen-by-gamow)

(defun trial (num-elevators)
  (let ((gamow-height (/ 1 6)))
    (fletrec ((calc-elevs
                (i)
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
                    (ets:insert 'elevators
                       (set-elevator
                         (car (ets:lookup 'elevators i))
                         distance-to-gamow distance-to-gamow
                         direction-seen-by-gamow direction-seen-by-gamow))
                    (calc-elevs (+ i 1))))))
      (calc-elevs 1))
    (ets:insert 'elevators
       (set-elevator
         (car (ets:lookup 'elevators 1))
         label 'closest-so-far))
    (fletrec ((find-closest-elev
                (i)
                (if (=< i num-elevators)
                  (progn
                    (let ((ith-elev (car (ets:lookup 'elevators i))))
                      (if (< (elevator-distance-to-gamow ith-elev)
                             (elevator-distance-to-gamow
                               (car (ets:lookup 'elevators 'closest-so-far))))
                        (ets:insert 'elevators
                           (set-elevator ith-elev
                                         label 'closest-so-far)))
                      (find-closest-elev (+ i 1)))))))
      (find-closest-elev 2))
    (let ((closest-elev
            (car (ets:lookup 'elevators 'closest-so-far))))
      (if (== (elevator-direction-seen-by-gamow closest-elev) -1)
        1
        0))))

(defun run (num-elevators)
  (if (== (ets:info 'elevators) 'undefined)
    (ets:new 'elevators (list 'named_table (tuple 'keypos (elevator-label)))))
  (fletrec ((loop (i)
                  (if (=< i num-elevators)
                    (progn
                      (ets:insert 'elevators (make-elevator label i))
                      (loop (+ i 1))))))
    (loop 1))
  (montecarlo:run (lambda () (trial num-elevators))))
