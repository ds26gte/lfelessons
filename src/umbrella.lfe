; first change 2013-03-07
; last change 2014-11-26

(defmodule umbrella
  (export (run 2) (run 1) (run 0)))

(defun location (rain-prob max-num-walks)
  (random:seed (now))
  (let ((me (self)))
    (fletrec ((loop (num-umbrellas)
                    (receive
                      ((tuple pid num-walks umbrella)
                       (let ((num-umbrellas (+ num-umbrellas umbrella)))
                         (cond ((>= num-walks max-num-walks) (exit num-walks))
                               ((< (random:uniform) rain-prob)
                                (if (> num-umbrellas 0)
                                  (progn
                                    (!  pid (tuple me (+ num-walks 1) 1))
                                    (loop (- num-umbrellas 1)))
                                  (exit num-walks)))
                               ('true
                                (! pid (tuple me (+ num-walks 1) 0))
                                (loop num-umbrellas))))))))
      (loop 1))))

(defun walk (trial-pid rain-prob max-num-walks)
  (process_flag 'trap_exit 'true)
  (let ((home-pid (spawn_link (lambda () (location rain-prob max-num-walks))))
        (office-pid (spawn_link (lambda () (location rain-prob max-num-walks)))))
    (! home-pid (tuple office-pid 0 0))
    (receive
      ((tuple 'EXIT _ value)
       (! trial-pid value)))))

(defun trial (rain-prob max-num-walks)
  (let ((trial-pid (self)))
    (spawn (lambda () (walk trial-pid rain-prob max-num-walks)))
    (receive
      (result
        result))))

(defun run (rain-prob max-num-walks)
  (montecarlo:run
     (lambda () (trial rain-prob max-num-walks))))

(defun run (rain-prob)
  (run rain-prob (* 365 2 5)))

(defun run ()
  (run 0.7))
