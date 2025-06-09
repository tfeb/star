(in-package :cl-user)

(needs ((:org.tfeb.star
         :org.tfeb.hax.collecting
         :org.tfeb.dsm)
        :compile t :use t))

(defmacro thunk (&body forms)
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

(defun in-nested-list (list)
  (assert (typep list 'list) (list) "not a list")
  (let ((tail list)
        (agenda '()))
    (values
     (thunk
       (or (not (null tail))
           (not (null agenda))))
     (thunk
       (labels ((try (this)
                  (typecase this
                    (cons
                     (unless (null tail)
                       (push tail agenda))
                     (setf tail this)
                     (try (pop tail)))
                    (t
                     this))))
       (cond
        ((not (null tail))
         (try (pop tail)))
        ((not (null agenda))
         (setf tail (pop agenda))
         (try (pop tail)))
        (t
         (error "um"))))))))

(defun flatten (l)
  (collecting
    (for ((e (in-nested-list l)))
      (collect e))))
