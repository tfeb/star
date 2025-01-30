;;;; Test non-range iterators
;;;
;;; This is not nearly enough tests
;;;

(in-package :org.tfeb.star/test)

(define-test ("org.tfeb.star" "org.tfeb.star.iterators")
  :depends-on ("org.tfeb.star.sanity")
  :time-limit 10)

(define-test ("org.tfeb.star.iterators" "in-naturals")
  (is = (let ((v 0))
          (dotimes (i 10)
            (incf v i))
          v)
      (let ((v 0))
        (for ((i (in-naturals 10)))
          (incf v i))
        v))
  (is = (let ((v 0))
          (dotimes (i 11)
            (incf v i))
          v)
      (let ((v 0))
        (for ((i (in-naturals :bound 10 :inclusive t)))
          (incf v i))
        v)))

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

(define-test ("org.tfeb.star.iterators" "stepping")
  (let ((l '(1 2 3)))
    (is equal l
        (collecting (for ((tail (stepping (tail :initially l
                                             :then (cdr tail)
                                             :while tail))))
                      (collect (car tail)))))
    (is equal l
        (collecting (for ((e (stepping*
                              (tail :initially l :then (cdr tail) :while tail :value nil)
                              (e :as (car tail)))))
                      (collect e)))))
  (is-values (with-collectors (a b)
               (for (((i j) (stepping-values (i j)
                              :initially (values 0 0)
                              :then (values (+ j 1) (+ i 2))
                              :while (< j 10))))
                 (a i) (b j)))
   (equal '(0 1 3 4 6 7 9))
   (equal '(0 2 3 5 6 8 9))))

(when *test-individually*
  (test "org.tfeb.star.iterators" :report *test-report-class*))
