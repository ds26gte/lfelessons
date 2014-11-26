; first change 2013-02-28
; last change 2014-11-26

(defmodule probutil
  (export all))

(defun randperm (n m)
  (fletrec ((loop (j result already-selected)
                  (if (=< j m)
                    (fletrec ((loop2 (i)
                                     (let ((i (if (> i n) 1 i)))
                                       (if (lists:member i already-selected)
                                         (loop2 (+ i 1))
                                         (loop (+ j 1) (cons i result) (cons i already-selected))))))
                      (loop2 (random:uniform n)))
                    result)))
    (loop 1 () ())))

(defun randperm (n)
  (randperm n n))
