; first change 2013-02-23
; last change 2013-02-28

(defmodule montecarlo
  (export (run 1) (run 2)
          (timed-experiment 3)))

(defun run (experiment num-trials)
  (fletrec ((loop (i acc)
                  (if (=< i num-trials)
                    (loop (+ i 1) (+ acc (funcall experiment)))
                    (/ acc num-trials))))
    (loop 1 0)))

(defun run (experiment)
  (run experiment 
       ;if num-trials not specified, assume
       10000))

(defun timed-experiment (experiment timeout default-result)
  (lambda ()
    (let* ((experiment-ref (make_ref))
           (spawner (self))
           (experiment-pid (spawn (lambda ()
                                    (! spawner (tuple experiment-ref (funcall experiment)))))))
      (receive
        ((tuple experiment-ref result) result)
        (after timeout
               (exit experiment-pid '"taking too much time; return default result")
               default-result)))))
