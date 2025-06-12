(in-package :cl-user)

(needs ((:org.tfeb.star
         :org.tfeb.hax.collecting
         :org.tfeb.hax.iterate
         :org.tfeb.dsm)
        :compile t :use t))

(defmacro thunk (&body forms)
  ;; from org.tfeb.hax.utilities, but that clashes with SBCL's CL-USER
  `(lambda () ,@forms))

(defun in-cons-tree (cons-tree)
  (typecase cons-tree
    (cons
     (let ((agenda (list cons-tree)))
       (values
        (thunk
          (not (null agenda)))
        (thunk
          (destructuring-bind (car . cdr) (pop agenda)
            (when (consp cdr)
              (push cdr agenda))
            (when (consp car)
              (push car agenda))
            (values car cdr))))))
    (t
     (values
      (constantly nil)
      (thunk (error "what even is this"))))))

(define-iterator-optimizer in-cons-tree (form)
  ;; This is not likely useful
  (destructuring-match form
    ((_ tree)
     (let ((agenda (make-symbol "AGENDA")))
       (values
        t
        `(((,agenda) (let ((it ,tree))
                       (if (consp it)
                           (list it)
                         '()))))
        `(not (null ,agenda))
        `(destructuring-bind (car . cdr) (pop ,agenda)
            (when (consp cdr)
              (push cdr ,agenda))
            (when (consp car)
              (push car ,agenda))
            (values car cdr))
        nil)))))

(defun in-nested-list (list &key
                            (include-nils t)
                            (order ':depth-first))
  (check-type list list)
  (check-type order (member :depth-first :breadth-first))
  (let* ((tail list)
         (agenda '()))
    (values
     (case order
       (:depth-first
        (if include-nils
            (thunk
              (iterate try ((tl tail)
                            (ag agenda))
                (if (null tl)
                    (if (null ag)
                        nil
                      (try (first ag) (rest ag)))
                  (destructuring-bind (this . more) tl
                    (typecase this
                      (cons
                       (try this (cons more ag)))
                      (t
                       (setf tail tl
                             agenda ag)
                       t))))))
          (thunk
            (iterate try ((tl tail)
                          (ag agenda))
              (if (null tl)
                  (if (null ag)
                      nil
                    (try (first ag) (rest ag)))
                (destructuring-bind (this . more) tl
                  (typecase this
                    (null
                     (try more ag))
                    (cons
                     (try this (cons more ag)))
                    (t
                     (setf tail tl
                           agenda ag)
                     t))))))))
       (:breadth-first
        (if include-nils
            (thunk
              (iterate try ((tl tail)
                            (ag agenda))
                (if (null tl)
                    (if (null ag)
                        nil
                      (try (first ag) (rest ag)))
                  (destructuring-bind (this . more) tl
                    (typecase this
                      (cons
                       (try more (cons this ag)))
                      (t
                       (setf tail tl
                             agenda ag)
                       t))))))
          (thunk
            (iterate try ((tl tail)
                          (ag agenda))
              (if (null tl)
                  (if (null ag)
                      nil
                    (try (first ag) (rest ag)))
                (destructuring-bind (this . more) tl
                  (typecase this
                    (null
                     (try more ag))
                    (cons
                     (try more (cons this ag)))
                    (t
                     (setf tail tl
                           agenda ag)
                     t)))))))))
     (thunk (pop tail)))))

(unless
    (and
     (equal (collecting
              (for ((e (in-nested-list '((a) b nil c))))
                (collect e)))
            '(a b nil c))
     (equal (collecting
              (for ((e (in-nested-list '((a) b nil c) :include-nils nil)))
                (collect e)))
            '(a b c))
     (equal (collecting
              (for ((e (in-nested-list '((a) b nil c) :order :breadth-first)))
                (collect e)))
            '(b nil c a))
     (equal (collecting
              (for ((e (in-nested-list '((a) b nil c) :order :breadth-first
                                       :include-nils nil)))
                (collect e)))
            '(b c a)))
  (warn "in-nested-list is buggy"))

#||
(defun flatten (l)
  (collecting
    (for ((e (in-nested-list l :include-nils nil)))
      (collect e))))
||#
