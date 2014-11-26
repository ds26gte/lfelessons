; first change 2013-02-28
; last change 2014-11-26

(defmodule guess
  (export (run 1)))

(defun count-correct-guesses (num-questions)
  (let ((guesses (probutil:randperm num-questions)))
    (fletrec ((loop (i correct-so-far)
                    (if (=< i num-questions)
                      (loop (+ i 1)
                            (if (== (lists:nth i guesses) i)
                              (+ correct-so-far 1)
                              correct-so-far))
                      correct-so-far)))
      (loop 1 0))))

(defun run (num-questions)
  (montecarlo:run (lambda () (count-correct-guesses num-questions))))
