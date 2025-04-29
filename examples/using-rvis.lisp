;;;; Using real-valued iterators (example)
;;;

(in-package :cl-user)

(needs
 ((:org.tfeb.star
   :org.tfeb.hax.collecting)
  :compile t :use t)
 ("real-valued-iterators"
  :compile t))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (use-package :org.tfeb.star/examples/real-valued-iterators))

(define-real-valued-iterators
  double-float)

;;; This ...
;;;
(defun ssq/for (n m)
  (declare (type fixnum n m)
           (optimize speed))
  (with-accumulators ((s + :initially 0.0d0 :type double-float))
    (for* ((iota (in-double-floats n))
           (kappa (in-double-floats m)))
      (s (* iota kappa)))))

;;; ... is a lot simpler than this
;;;
(defun ssq/loop (n m)
  (declare (type fixnum n m)
           (optimize speed))
  (loop for iota double-float from 0.0d0 below (float n 0.0d0)
        summing (loop for kappa double-float from 0.0d0 below (float m 0.0d0)
                      summing (* iota kappa)
                        into s double-float
                      finally (return s))
          into ssq double-float
        finally (return ssq)))
