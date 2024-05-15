;;;; Štar: ranges
;;;
;;; This is the hairy thing that needs to be fast.  This is still
;;; pretty naive about types and in particular it doesn't use any
;;; fancy (signed-byte ...) types which could easily be useful but are
;;; a lot more work.  I am hoping that this is at least correct (but
;;; perhaps it is not).
;;;
;;; This is by far the most complicated case (and it's the most
;;; complicated case I ever want to deal with)
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (("pkg" "common" "star")
  :compile t))

(in-package :org.tfeb.star/iterators)

(setf *star-bootstrap* t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant distinct-float-types
    ;; the distinct float types for an implementation, thinnest to
    ;; widest.
    (if (boundp 'distinct-float-types)
        (symbol-value 'distinct-float-types)
      (iterate next ((types (mapcar #'type-of '(1.0l0 1.0d0 1.0f0 1.0s0)))
                     (the-types '()))
        (if (null types)
            the-types
          (destructuring-bind (this-type . more-types) types
            (let ((root-type
                   (etypecase this-type
                     ;; It is legal to return (short-float ...)  say
                     ;; and that might be useful.  It is also legal
                     ;; (but discouraged) to return some
                     ;; implementation-specific type specifier: this
                     ;; isn't handled.
                     (symbol this-type)
                     (cons (first this-type)))))
              (unless (member root-type '(short-float single-float double-float long-float))
                (catastrophe "unexpected root type ~S, you'll need to conditionalize for ~A"
                             root-type (lisp-implementation-type)))
              (next more-types (adjoin root-type the-types)))))))))

(defun widest-float-type (&rest numbers)
  ;; Return the name of the widest float type among NUMBERS, NIL if
  ;; there are no floats.
  (declare (dynamic-extent numbers))
  (let ((wideness 0)
        (indexed (load-time-value (iterate next ((types distinct-float-types)
                                                 (index 1)
                                                 (indexed '()))
                                    (if (null types)
                                        (nreverse indexed)
                                      (next (rest types)
                                            (+ index 1)
                                            (acons (first types) index indexed)))))))
    (dolist (type (mapcar #'type-of numbers)
                  (car (rassoc wideness indexed)))
      (when (subtypep type 'float)
        (let ((r (assoc (etypecase type
                          (symbol type)
                          (cons (first type)))
                        indexed)))
          (when (null r)
            (catastrophe "unknown float type ~S" type))
          (setf wideness (max wideness (cdr r))))))))

(defun in-range (&rest arg/s)
  ;; This is probably wrong and certainly needs an optimizer (which is
  ;; going to be very, very hairy)
  "Range iterator

See the manual.  Optimizable."
  (declare (dynamic-extent arg/s))
  (destructuring-match arg/s
    ((before)
     (:when (typep before '(or real (member *))))
     ;; Try and make the zero be of the appropriate type
     (in-hairy-range :from (typecase before
                             (float
                              (coerce 0 (type-of before)))
                             ((or real (member *))
                              0))
                     :before before))
    ((&key from to after before by type)
     (declare (ignore from to after before by type))
     (apply #'in-hairy-range arg/s))
    (otherwise
     (syntax-error `(in-range ,@arg/s) "bad range"))))

(defun in-hairy-range (&key (from 0 from-p)
                            (after 0 after-p)
                            (before 0 before-p)
                            (to 0 to-p)
                            (by 1 by-p)
                            (type 'real))
  ;; Defaultify arguments, check types, work out effective type, then
  ;; call IN-SIMPLE-RANGE.  TYPE is not particularly useful here, but
  ;; it will be useful for the optimizer.
  (declare (type real from by)
           (type (or real (member *)) to before))
  (when (or (and from-p after-p)
            (not (or from-p after-p)))
    (star-error "need exactly one of FROM and AFTER"))
  (when (or (and to-p before-p)
            (not (or to-p before-p)))
    (star-error "need exactly one of TO and BEFORE"))
  (unless (subtypep type 'real)
    (star-error "~S is not a recognizable subtype of REAL" type))
  (let* ((by (cond
              (by-p
               by)
              ((or (eq to '*) (eq before '*))
               1)
              (t
               (let ((s (signum (- (if to-p to before)
                                   (if from-p from after)))))
                 (if (zerop s)
                     (1+ s)
                   s)))))
         (start (if from-p from (+ after by)))
         (limit (cond
                 ((or (eq to '*) (eq before '*))
                  nil)
                 (to-p
                  (+ to by))
                 (t
                  before)))
         (widest-float-type (widest-float-type start limit by)))
    (if widest-float-type
        ;; Some things are floats, make everything be
        (let ((by (coerce by widest-float-type))
              (start (coerce start widest-float-type))
              (limit (coerce limit widest-float-type)))
          (assert (and (typep start type)
                       (typep by type)
                       (typep limit type))
              (start by limit)
            "at least one of range start ~S, range step ~S & range limit ~S is not a ~S"
            start by limit type)
          (in-simple-range start limit by))
      ;; Nothing is a float
      (progn
        (assert (and (typep start type)
                     (typep by type)
                     (typep limit type))
            (start by limit)
          "at least one of range start ~S, range step ~S & range limit ~S is not a ~S"
          start by limit type)
        (in-simple-range start limit by)
        (in-simple-range start limit by)))))

(defun in-simple-range (start limit by)
  ;; This is now very simple-minded: the optimizer will do the things
  ;; that need to be done.
  (let ((v start)
        (by by))
    (values
     (if (not limit)
         (constantly t)
       (case (round (signum by))
         (-1
          (lambda ()
            (> v limit)))
         (0
          (constantly t))
         (1
          (lambda ()
            (< v limit)))))
     (case (round (signum by))
       (0
        (constantly v))
       ((-1 1)
       (lambda ()
         (prog1 v
           (incf v by))))))))

(define-iterator-optimizer (in-range *builtin-iterator-optimizer-table*) (form environment)
  (destructuring-match form
    ((_ before)
     (in-hairy-range-optimizer :environment environment :from 0 :before before))
    ((_ &rest args &key from to after before by type)
     (declare (ignore from to after before by type))
     (apply #'in-hairy-range-optimizer :environment environment args))
    (otherwise
     (syntax-error form "bad range"))))

(defun compute-effective-step (begin begin-literal end end-literal by by-p)
  ;; Return the effective step and whether it is literal.  If it is
  ;; not literal it can return NIL and NIL meaning 'needs computed at
  ;; runtime'.  If BEGIN and END are literals they are known to be sane.
  (if by-p
      (multiple-value-bind (by by-literal) (literal by)
        (if by-literal
            (values by t)
          (values by nil)))
    ;; no given BY
    (cond
     ((and begin-literal end-literal)
      (cond
       ((realp end)
        (values (case (round (signum (- end begin)))
                  (-1 -1)
                  ((0 1) 1))
                t))
       ((eql end '*)
        (values 1 t))
       (t
        (catastrophe "mutant end"))))
      (end-literal
       (cond
        ((eql end '*)
         (values 1 t))
        ((realp end)
         (values nil nil))
        (t
         (catastrophe "mutant end"))))
      (t
       (values nil nil)))))

(defun in-hairy-range-optimizer (&key environment
                                      (from nil from-p)
                                      (after nil after-p)
                                      (before nil before-p)
                                      (to nil to-p)
                                      (by nil by-p)
                                      (type '(quote real) type-p))
  (declare (ignore environment))        ;a cleverer version might use this
  ;; We want to optimize the cases where the type is a literal, and
  ;; also where the ranges are literals, so we need to work all this
  ;; out.  Other cases we just give up on.
  (when (or (and from-p after-p)
            (not (or from-p after-p)))
    (star-error "exactly one of FROM and AFTER"))
  (when (or (and to-p before-p)
            (not (or to-p before-p)))
    (star-error "exactly one of TO and BEFORE"))
  (multiple-value-bind (begin begin-literal) (literal (if from-p from after))
    (multiple-value-bind (end end-literal) (literal (if to-p to before))
      (when (and begin-literal (not (realp begin)))
        (star-error "given begin ~S which is not a real" begin))
      (when (and end-literal (not (or (realp end) (eql end '*))))
        (star-error "given end ~S which is not either a real or '*" end))
      (multiple-value-bind (step step-literal) (compute-effective-step begin begin-literal
                                                                   end end-literal
                                                                   by by-p)
        (when (and step-literal (not (realp step)))
          (star-error "given step ~S which is not a real" step))
        (cond
         ((and begin-literal end-literal step-literal)
          ;; literal bounds
          (literal-values-range-optimizer (if from-p begin (+ begin step))
                                          (cond
                                           ((eql end '*)
                                            nil)
                                           (to-p
                                            (+ end step))
                                           (t
                                            end))
                                          step type type-p))
         (type-p
          ;; user-given type
          (multiple-value-bind (type type-literal) (literal type)
            (if type-literal
                ;; We can use the declared type
                (literal-type-range-optimizer begin end step step-literal by-p type from-p to-p)
              ;; give up & reject it
              (note "giving up on a range with no literal type"))))
         (t
          ;; No user-given type at all: just reject it
          (note "giving up on a range with no type information at all")))))))

(defun literal-values-range-optimizer (start limit step type type-p)
  ;; START STEP are numeric literals, LIMIT is either numeric or NIL
  ;; if it is unbounded, so we can infer good types at compile time in
  ;; many cases
  (let* ((bounded limit)
         (widest-float-type (if bounded
                                (widest-float-type start step limit)
                              (widest-float-type start step))))
    (if widest-float-type
        ;; They're all floats
        (let ((start (coerce start widest-float-type))
              (limit (and bounded (coerce limit widest-float-type)))
              (step (coerce step widest-float-type)))
          (if (not type-p)
              ;; This is the easy case: no user type so just use the
              ;; inferred float type
              (with-names (<v>)
                (values
                 t
                 `(((,<v>) ,start
                    (declare (type ,widest-float-type ,<v>))))
                 (if bounded
                     (case (round (signum step))
                       (-1
                        `(> ,<v> ,limit))
                       ((0 1)
                        `(< ,<v> ,limit)))
                   't)
                 `(prog1 ,<v>
                    (incf ,<v> ,step))))
            ;; There is a user type
            (multiple-value-bind (type type-literal) (literal type)
              (cond
               (type-literal
                ;; it's literal
                (unless (subtypep widest-float-type type)
                  (star-error "given type ~S is incompatible with inferred type ~S"
                              type widest-float-type))
                ;; Now just use the inferred type as before
                (with-names (<v>)
                  (values
                   t
                   `(((,<v>) ,start
                      (declare (type ,widest-float-type ,<v>))))
                   (if bounded
                       (case (round (signum step))
                         (-1
                          `(> ,<v> ,limit))
                         ((0 1)
                          `(< ,<v> ,limit)))
                     't)
                   `(prog1 ,<v>
                      (incf ,<v> ,step)))))
               (t
                ;; we have floats, There is a type, it's not a
                ;; literal.  We should check at run-time then use the
                ;; inferred type
                (with-names (<v>)
                  (values
                   t
                   `(((,<v>)
                      (let ((type ,type))
                        (unless (subtypep ',widest-float-type type)
                          (star-error "given type ~S is incompatible with inferred type ~S"
                                      type widest-float-type))
                        ,start)
                      (declare (type ,widest-float-type ,<v>))))
                   (if bounded
                       (case (round (signum step))
                         (-1
                          `(> ,<v> ,limit))
                         ((0 1)
                          `(< ,<v> ,limit)))
                     't)
                   `(prog1 ,<v>
                      (incf ,<v> ,step)))))))))
      ;; They are not floats
      (if (not type-p)
          ;; No user type given
          (if (and (typep start 'fixnum)
                   (typep step 'fixnum)
                   bounded
                   (typep (+ limit step) 'fixnum))
              ;; All fixnums, allowing for overflow.  We're only
              ;; willing to declare the type if it is bounded
              (with-names (<v>)
                (values
                 t
                 `(((,<v>) ,start (declare (type fixnum ,<v>))))
                 (case (round (signum step))
                   (-1
                    `(> ,<v> ,limit))
                   ((0 1)
                    `(< ,<v> ,limit)))
                 `(prog1 ,<v>
                    (incf ,<v> ,step))))
            ;; Just give up, there's nothing useful to say
            (with-names (<v>)
              (values
               t
               `(((,<v>) ,start))
               (if bounded
                   (case (round (signum step))
                     (-1
                      `(> ,<v> ,limit))
                     ((0 1)
                      `(< ,<v> ,limit)))
                 't)
               `(prog1 ,<v>
                  (incf ,<v> ,step)))))
        ;; There is a user-given type
        (multiple-value-bind (type type-literal) (literal type)
          (cond
           (type-literal
            ;; User type is literal, we can check and use it now
            (unless (subtypep type 'rational)
              (star-error "given type ~S is not a subtype of RATIONAL" type))
            (unless (and (typep start type)
                         (typep step type)
                         (or (not bounded) (typep limit type)))
              (star-error "not all of start ~S~@[, limit ~S~] and step ~S are of type ~S"
                          start limit step type))
            (with-names (<v>)
              (values
               t
               `(((,<v>) ,start (declare (type ,type ,<v>))))
               (if bounded
                   (case (round (signum step))
                     (-1
                      `(> ,<v> ,limit))
                     ((0 1)
                      `(< ,<v> ,limit)))
                 't)
               `(prog1 ,<v>
                  (incf ,<v> ,step)))))
           ((and (typep start 'fixnum)
                 (typep step 'fixnum)
                 bounded
                 (typep (+ limit step) 'fixnum))
            ;; not a literal type, but fixnums, allowing for overflow.  It is bounded
            (with-names (<v>)
              (values
               t
               `(((,<v>) (let ((type ,type))
                           (unless (subtype ,type 'rational)
                             (star-error "given type ~S is not a subtype of RATIONAL" type))
                           (unless (and (typep ,start type)
                                        (typep ,limit type)
                                        (typep ,step type))
                             (star-error
                              "not all of start ~S, limit ~S and step ~S are of type ~S"
                              ,start ,limit ,step type))
                           ,start
                           (declare (type fixnum ,<v>)))))
               (case (round (signum step))
                 (-1
                  `(> ,<v> ,limit))
                 ((0 1)
                  `(< ,<v> ,limit)))
               `(prog1 ,<v>
                  (incf ,<v> ,step)))))
           (t
            ;; It's hopeless.
            (with-names (<v>)
              (values
               t
               `(((,<v>) (let ((type ,type))
                           (unless (subtype ,type 'rational)
                             (star-error "given type ~S is not a suntype of RATIONAL" type))
                           ,@(if bounded
                                 `((unless (and (typep ,start type)
                                                (typep ,limit type)
                                                (typep ,step type))
                                     (star-error
                                      "not all of start ~S, limit ~S and step ~S are of type ~S"
                                      ,start ,limit ,step type)))
                               `((unless (and (typep ,start type)
                                              (typep ,step type))
                                   (star-error
                                    "not all of start ~S and step ~S are of type ~S"
                                    ,start ,limit ,step type))))
                           ,start)))
               (if bounded
                   (case (round (signum step))
                     (-1
                      `(> ,<v> ,limit))
                     ((0 1)
                      `(< ,<v> ,limit)))
                 't)
               `(prog1 ,<v>
                  (incf ,<v> ,step)))))))))))

(defun literal-type-range-optimizer (begin end step step-literal step-p type from-p to-p)
  ;; There is a literal type which can be used, and the step may be literal
  (if step-literal
      ;; This is the nice case
      (let ((step (coerce step type)))
        (with-names (<begin> <end> <v> <limit>)
          (values
           t
           `(((,<begin> ,<end>)
              (values ,begin
                      (let ((end ,end))
                        (etypecase end
                          (number end)
                          ((member *) nil))))
              (declare (type ,type ,<begin>)
                       (type (or ,type null) ,<end>)))
             ((,<v> ,<limit>)
              (values
               ,(if from-p
                    <begin>
                  `(+ ,<begin> ,step))
               (if ,<end>
                   ,(if to-p
                        `(+ ,<end> ,step)
                      <end>)
                 ,<begin>))
              (declare (type ,type ,<v> ,<limit>))))
           (if (>= step 0)
               `(if ,<end>
                    (< ,<v> ,<limit>)
                  t)
             `(if ,<end>
                  (> ,<v> ,<limit>)))
           `(prog1 ,<v>
              (incf ,<v> ,step)))))
    ;; Step is not literal
    (with-names (<begin> <end> <step> <v> <limit>)
      (note "a literal step might help")
      (values
       t
       `(((,<begin> ,<end>)
          (values ,begin
                  (let ((end ,end))
                    (etypecase end
                      (number end)
                      ((member *) nil))))
          (declare (type ,type ,<begin>)
                   (type (or ,type null) ,<end>)))
         ((,<step>)
          ,(if step-p
               `(coerce ,step ',type)
             `(if ,<end>
                  (coerce (case (round (signum (- ,<end> ,<begin>)))
                            (-1 -1)
                            ((0 1) 1))
                          ',type)
                ,(coerce 1 type)))
          (declare (type ,type ,<step>)))
         ((,<v> ,<limit>)
          (values
           ,(if from-p
                <begin>
              `(+ ,<begin> ,<step>))
           (if ,<end>
               ,(if to-p
                    `(+ ,<end> ,<step>)
                  <end>)
             ,<begin>))
          (declare (type ,type ,<v> ,<limit>))))
       `(if ,<end>
            (if (> ,<step> 0)
                (< ,<v> ,<limit>)
              (> ,<v> ,<limit>))
          t)
       `(prog1 ,<v>
          (incf ,<v> ,<step>))))))

(setq *star-bootstrap* nil)
