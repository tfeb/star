;;;; Test non-range iterators
;;;
;;; This is not nearly enough tests
;;;

(in-package :org.tfeb.star/test)

(define-test ("org.tfeb.star" "org.tfeb.star.iterators")
  :depends-on ("org.tfeb.star.sanity")
  :time-limit 10)

(define-test ("org.tfeb.star.iterators" "in-list")
  (dolist (l '(() (1 2 3)))
    (is equal l (collecting (for ((e (in-list l)))
                              (collect e))))
    (pessimizing-for ((p o) (collecting (for ((e (in-list l)))
                                           (collect e))))
      (is equal (p) (o))))
  (is equal '((1 3) (1 4) (2 3) (2 4))
      (collecting
        (for* ((i (in-list '(1 2)))
               (j (in-list '(3 4))))
          (collect (list i j))))))

(define-test ("org.tfeb.star.iterators" "in-package-symbols")
  (flet ((sorted (l)
           (sort l #'string< :key #'symbol-name)))
    (let ((wpi-list
           (with-package-iterator (pi "CL" :internal :external :inherited)
             (sorted
              (collecting
                (for (((found symbol _ _) (sequentially* (pi))))
                  (if found
                      (collect symbol)
                    (final)))))))
          (ds-list
           (sorted
            (collecting (do-symbols (s "CL") (collect s))))))
      (multiple-value-bind (wpsp-list wpso-list)
          (pessimizing-for ((p o) (collecting
                                    (for ((s (in-package-symbols "CL")))
                                      (collect s))))
            (values (sorted (p))
                    (sorted (o))))
        (is equal ds-list wpi-list)
        (is equal ds-list wpsp-list)
        (is equal ds-list wpso-list)))))

(when *test-individually*
  (test "org.tfeb.star.iterators" :report *test-report-class*))
