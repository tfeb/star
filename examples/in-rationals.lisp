;;;; Enumerate the rationals
;;;

(in-package :cl-user)

(needs ((:org.tfeb.hax.collecting
         :org.tfeb.hax.utilities
         :org.tfeb.star)
        :use t :compile t))

;;; Calkin-Wilf tree
;;; https://en.wikipedia.org/wiki/Calkin%E2%80%93Wilf_tree
;;;

(defun left-child (p q)
  (values p (+ p q)))

(defun right-child (p q)
  (values (+ p q) q))

(defun in-rationals (&optional (p 1) (q 1))
  ;; Enumerate the numerators & denominators of the positive rationals
  ;; using a Calkin-Wilf tree.
  (let ((numerators (make-collector :initial-contents (list p) :copy nil))
        (denominators (make-collector :initial-contents (list q) :copy nil))) ;
    (values
     (constantly t)
     (thunk
       (let ((p (pop-collector numerators))
             (q (pop-collector denominators)))
         (multiple-value-bind (lp lq) (left-child p q)
           (collect-into numerators lp)
           (collect-into denominators lq))
         (multiple-value-bind (rp rq) (right-child p q)
           (collect-into numerators rp)
           (collect-into denominators rq))
         (values p q))))))

(define-iterator-optimizer in-rationals (form)
  ;; this probably makes no difference
  (destructuring-bind (&optional (p 1) (q 1)) (rest form)
    (with-names (<numerators> <denominators>)
      (values
       t
       `(((,<numerators> ,<denominators>)
          (values (make-collector :initial-contents (list ,p) :copy nil)
                  (make-collector :initial-contents (list ,q) :copy nil))))
       t
       `(let ((p (pop-collector ,<numerators>))
             (q (pop-collector ,<denominators>)))
         (multiple-value-bind (lp lq) (left-child p q)
           (collect-into ,<numerators> lp)
           (collect-into ,<denominators> lq))
         (multiple-value-bind (rp rq) (right-child p q)
           (collect-into ,<numerators> rp)
           (collect-into ,<denominators> rq))
         (values p q))
       nil))))

#||
(defun test-uniqueness (n)
  (let ((ut (make-hash-table)))
    (for ((_ (in-naturals n))
          ((p q) (in-rationals)))
      (let ((r (/ p q)))
        (when (gethash r ut)
          (final nil p q r)))))
  (values t nil nil nil))
||#
