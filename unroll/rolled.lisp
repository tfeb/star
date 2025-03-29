;;;; The rolled iterator
;;;

(in-package :org.tfeb.star/unroll/impl)

(defmacro rolled (iterator)
  "Wrapping this around an iterator will prevent unrolling"
  iterator)

(define-iterator-optimizer rolled (form environment stack)
  (destructuring-match form
    ((_ (&whole rolled-form iterator &rest _))
     (:when (symbolp iterator))
     (multiple-value-bind (optimizer stail)
         (find-iterator-optimizer iterator stack)
       (if optimizer
           (funcall optimizer rolled-form environment stail)
         (values nil nil nil nil nil))))
    (otherwise
     (values nil nil nil nil nil))))

(define-iterator-unroller rolled (form vars unroll-by)
  (declare (ignore form vars unroll-by))
  (values nil nil nil nil))
