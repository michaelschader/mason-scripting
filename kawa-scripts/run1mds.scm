(set! MAX 10000)

(load "tutorial1.scm")
(load "ca.scm")

(set! start-time (java.lang.System:currentTimeMillis))

(let ((t1 (<tutorial1> 0)))
  (t1:start)
  (do ((x 0 (+ x 1))) ((= x MAX))
    (t1:schedule:step t1)
;    (if (= (modulo (+ x 1) 1000) 0) 
;      (begin (write Steps: ) (write (+ x 1)) (newline)))
  )
  (t1:finish))

(set! end-time (java.lang.System:currentTimeMillis))

(write (* 1000.0 (/ MAX (- end-time start-time)))) (newline)

(exit)
