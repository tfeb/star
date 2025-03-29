;;;; Playing with unrolling
;;;
;;; The conclusion from this is mostly that branch prediction really
;;; works nowadays.
;;;

(org.tfeb.tools.require-module:needs
 ((:org.tfeb.star/unroll
   :org.tfeb.dsm
   :org.tfeb.hax.utilities
   :org.tfeb.tools.timing)
  :compile t))

(defpackage :unrolling-play
  (:use :cl)
  (:use
   :org.tfeb.star/unroll
   :org.tfeb.dsm
   :org.tfeb.hax.utilities
   :org.tfeb.tools.timing))

(in-package :unrolling-play)

(define-iterator-unroller in-vector (form vars unroll-by)
  (destructuring-match form
    ((iv v &rest kws &key (by 1 byp) &allow-other-keys)
     (:when (not byp))
     (declare (ignore by))
     (destructuring-match vars
       ((e)
        (:when e)
        (with-names (<v> <i>)
          (values
           t
           `(((,<v> ,<i>) (values ,v 0)
              (declare (type vector ,<v>)
                       (type fixnum ,<i>))))
           `(,iv ,<v> :by ,unroll-by ,@kws)
           (make-list (1- unroll-by) :initial-element
                      `((setf ,e (aref ,<v> (incf ,<i>))))))))
       ((i e)
        (:when (and i e))
        (with-names (<v>)
          (values
           t
           `(((,<v>) ,v
              (declare (type vector ,<v>))))
           `(,iv ,<v> :by ,unroll-by ,@kws)
           (make-list (1- unroll-by) :initial-element
                      `((setf ,e (aref ,<v> (incf ,i))))))))
       (otherwise
        (values nil nil nil nil))))
    (otherwise
     (values nil nil nil nil))))

(defun inner-product (v1 v2)
  (declare (type (simple-array double-float (*)) v1 v2))
  (declare (optimize speed (safety 0)))
  #+LispWorks
  (declare (optimize (float 0))
           (:explain :boxing :floats))
  (assert (zerop (mod (min (length v1) (length v2)) 8)))
  (let ((s 0.0d0))
    (declare (type double-float s))
    (for ((e1 (in-vector v1))
          (e2 (in-vector v2)))
      (declare (type double-float e1 e2))
      (incf s (* e1 e2)))
    s))

(defun bench-ip (&key (l 1000000) (n 2000))
  (let ((v (make-array (* 8 (ceiling l 8)) :element-type 'double-float))
        (s 0.0d0))
    (declare (type (simple-array double-float (*)) v)
             (type double-float s))
    (/ (* l 2)
       (timing (n (floor n 10))
         (incf s (inner-product v v))))))
