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
;;; I hate this code
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
  ;; there are no floats.  This is called in code written by
  ;; optimizers expansions so it wants to be quick: hence all this
  ;; silly hair.
  (declare (dynamic-extent numbers)
           (optimize speed))
  (let ((wideness 0))
    (declare (type fixnum wideness))
    (macrolet ((bump (v)
                 `(when (typep ,v 'float)
                    (setf wideness
                          (max wideness
                               (etypecase ,v
                                 ,@(collecting
                                     (for ((ft (in-list distinct-float-types))
                                           (j (stepping (i :initially 1
                                                           :then (1+ i)))))
                                         (collect `(,ft ,j)))))))))
               (lookup ()
                 `(ecase wideness
                    (0 nil)
                    ,@(collecting
                        (for ((ft (in-list distinct-float-types))
                              (j (stepping (i :initially 1
                                              :then (1+ i)))))
                            (collect `(, j ',ft)))))))
      (dolist (n numbers (lookup))
        (bump n)))))

(defun in-range (&rest arg/s)
  ;; This is probably wrong and certainly needs an optimizer (which is
  ;; going to be very, very hairy)
  "Range iterator

See the manual.  Optimizable."
  (declare (dynamic-extent arg/s))
  (destructuring-match arg/s
    (()
     (in-hairy-range :from 0 :by 1))
    ((before)
     (:when (typep before 'real))
     ;; Make the zero be of the appropriate type
     (in-hairy-range :from (coerce 0 (type-of before))
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
  (declare (type real from after before to by))
  (when (or (and from-p after-p)
            (not (or from-p after-p)))
    (star-error "need exactly one of FROM and AFTER"))
  (when (and to-p before-p)
    (star-error "need at most one of TO and BEFORE"))
  (unless (subtypep type 'real)
    (star-error "~S is not a recognizable subtype of REAL" type))
  (let* ((by (cond
              (by-p
               by)
              ((not (or to-p before-p))
               ;; unbounded range
               1)
              (t
               (let ((s (signum (- (if to-p to before)
                                   (if from-p from after)))))
                 (if (zerop s)
                     (1+ s)
                   s)))))
         (start (if from-p from (+ after by)))
         (limit (cond
                 (to-p
                  (+ to by))
                 (before-p
                  before)
                 (t
                  nil)))
         (widest-float-type (if limit
                                (widest-float-type start limit by)
                                (widest-float-type start by))))
    (multiple-value-bind (start limit by)
        (if widest-float-type
            ;; Some things are floats, make everything be
            (values (coerce start widest-float-type)
                    (if limit (coerce limit widest-float-type) nil)
                    (coerce by widest-float-type))
          (values start limit by))
      ;; Check things are compatible with the given type
      (assert (and (typep start type)
                   (typep by type))
          (start by)
        "at least one of range start ~S & range step ~S is not a ~S"
        start by type)
      (when limit
        (assert (typep limit type)
            (limit)
          "range limit ~S is not a ~S" limit type))
      (in-simple-range start limit by))))

(defun in-simple-range (start limit by)
  ;; This is now very simple-minded: the optimizer will do the things
  ;; that need to be done.
  (let ((v start))
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
    ((_)
     (in-hairy-unbounded-range-optimizer :environment environment :from 0))
    ((_ before)
     (in-hairy-bounded-range-optimizer :environment environment :from 0 :before before))
    ((_ &rest args &key
        (from nil from-p) (after nil after-p)
        (to nil to-p) (before nil before-p)
        by type)
     (declare (ignore from to after before by type))
     (when (or (and from-p after-p)
               (not (or from-p after-p)))
       (star-error "need exactly one of FROM and AFTER"))
     (when (and to-p before-p)
       (star-error "need at most one of TO and BEFORE"))
     (apply (if (or to-p before-p)
                #'in-hairy-bounded-range-optimizer
                #'in-hairy-unbounded-range-optimizer)
            :environment environment args))
    (otherwise
     (syntax-error form "bad range"))))

(defun compute-effective-step (begin begin-literal end end-literal by by-p)
  ;; Return the effective step and whether it is literal.  If it is
  ;; not literal it can return NIL and NIL meaning 'needs computed at
  ;; runtime'.  If BEGIN and END are literals they are known to be
  ;; sane.  END may be NIL which is the case for an unbounded
  ;; iteration.
  (if by-p
      (multiple-value-bind (by by-literal) (literal by)
        (if by-literal
            (values by t)
          (values by nil)))
    ;; no given BY
    (cond
     ((and begin-literal end-literal)
      (typecase end
       (real
        (values (case (round (signum (- end begin)))
                  (-1 -1)
                  ((0 1) 1))
                t))
       (null
        (values 1 t))
       (t
        (catastrophe "mutant end"))))
     (end-literal
      (typecase end
        (null
          (values 1 t))
        (real
         ;; Don't know because start is not literal
         (values nil nil))
        (t
         (catastrophe "mutant end"))))
     (t
      ;; Just hopeless
      (values nil nil)))))

(defun in-hairy-bounded-range-optimizer (&key environment
                                              (from nil from-p)
                                              (after nil)
                                              (before nil)
                                              (to nil to-p)
                                              (by nil by-p)
                                              (type '(quote real) type-p))
  ;; The range is bounded
  (declare (ignore environment))        ;a cleverer version might use this
  ;; We want to optimize the cases where the type is a literal, and
  ;; also where the ranges are literals, so we need to work all this
  ;; out.  Other cases we just give up on.
  (multiple-value-bind (begin begin-literal) (literal (if from-p from after))
    (when (and begin-literal (not (realp begin)))
      (star-error "given begin ~S which is not a real" begin))
    (multiple-value-bind (end end-literal) (literal (if to-p to before))
      (when (and end-literal (not (realp end)))
        (star-error "given end ~S which not a real" end))
      (multiple-value-bind (step step-literal) (compute-effective-step begin begin-literal
                                                                       end end-literal
                                                                       by by-p)
        (when (and step-literal (not (realp step)))
          (star-error "given step ~S which is not a real" step))
        (if (and begin-literal end-literal step-literal)
            ;; literal bounds
            (literal-values-range-optimizer (if from-p begin (+ begin step))
                                            (cond
                                             (to-p
                                              (+ end step))
                                             (t
                                              end))
                                            step type type-p)
          (multiple-value-bind (type type-literal) (literal type)
            (if type-literal
                ;; We can use the declared type
                (literal-type-bounded-range-optimizer
                 begin end step by-p type from-p to-p)
              ;; give up & reject it
              (note "giving up on a range with no literal type"))))))))

(defun in-hairy-unbounded-range-optimizer (&key environment
                                                (from nil from-p)
                                                (after nil)
                                                (by nil by-p)
                                                (type '(quote real) type-p))
  ;; The range is unbounded: this is similar to the previous case
  (declare (ignore environment))        ;a cleverer version might use this
  ;; We want to optimize the cases where the type is a literal, and
  ;; also where the ranges are literals, so we need to work all this
  ;; out.  Other cases we just give up on.
  (multiple-value-bind (begin begin-literal) (literal (if from-p from after))
    (when (and begin-literal (not (realp begin)))
      (star-error "given begin ~S which is not a real" begin))
    (multiple-value-bind (step step-literal) (compute-effective-step begin begin-literal
                                                                     nil t
                                                                     by by-p)
        (when (and step-literal (not (realp step)))
          (star-error "given step ~S which is not a real" step))
        (if (and begin-literal step-literal)
          ;; literal begin and step
          (literal-values-range-optimizer (if from-p begin (+ begin step))
                                          nil step type type-p)
          (multiple-value-bind (type type-literal) (literal type)
            (if type-literal
                ;; We can use the declared type
                (literal-type-unbounded-range-optimizer begin step by-p type from-p)
              ;; give up & reject it
              (note "giving up on a range with no literal type")))))))

(defun literal-values-range-optimizer (start limit step type type-p)
  ;; START, STEP are numeric literals, LIMIT is either numeric or NIL
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

(defun literal-type-bounded-range-optimizer (begin end step step-p type from-p to-p)
  ;; There is a literal type which can be used, and the step may be
  ;; literal.  The range is bounded so end is never NIL.
  (unless (subtypep type 'real)
    (star-error "type ~S is not a subtype of REAL" type))
  (cond
   (step-p
    (with-names (<b> <e> <s> <begin> <end> <step> <v> <limit>)
      (values
       t
       `(((,<b> ,<e> ,<s>)
          (values ,begin ,end ,step))
         ((,<begin> ,<end> ,<step>)
          ,(cond
            ((subtypep type 'float)
             `(let ((ft (widest-float-type ,<b> ,<e> ,<s>)))
                (cond
                 ((not ft)
                  (star-error "not floats"))
                 ((subtypep ft ',type)
                  ;; We're good
                  (values (coerce ,<b> ',type) (coerce ,<e> ',type)
                          (coerce ,<s> ',type)))
                 (t
                  (star-error "no floats of type ~S: widest is ~S" ',type ft)))))
            (t
             `(progn
                (unless (and (typep ,<b> ',type)
                             (typep ,<e> ',type)
                             (typep ,<s> ',type))
                  (star-error "begin ~S, end ~S and step ~S are not all of type ~S"
                              ,<b> ,<e> ,<s> ',type))
                (let ((ft (widest-float-type ,<b> ,<e> ,<s>)))
                  (if ft
                      (values (coerce ,<b> ft)
                              (coerce ,<e> ft)
                              (coerce ,<s> ft))
                    (values ,<b> ,<e> ,<s>))))))
          (declare (type ,type ,<begin> ,<end> ,<step>)))
         ((,<v> ,<limit>)
          (values
           ,(if from-p
                <begin>
              `(+ ,<begin> ,<step>))
           ,(if to-p
                `(+ ,<end> ,<step>)
              <end>))
          (declare (type ,type ,<v> ,<limit>))))
       `(if (> ,<step> ,(coerce 0 type))
            (< ,<v> ,<limit>)
          (> ,<v> ,<limit>))
       `(prog1 ,<v>
          (incf ,<v> ,<step>)))))
   (t
    ;; no step
    (with-names (<b> <e> <begin> <end> <step> <v> <limit>)
      (values
       t
       `(((,<b> ,<e>)
          (values ,begin ,end))
         ((,<begin> ,<end>)
          ,(cond
            ((subtypep type 'float)
             `(let ((ft (widest-float-type ,<b> ,<e>)))
                (cond
                 ((not ft)
                  (star-error "not floats"))
                 ((subtypep ft ',type)
                  ;; We're good
                  (values (coerce ,<b> ',type) (coerce ,<e> ',type)))
                 (t
                  (star-error "no floats of type ~S: widest is ~S" ',type ft)))))
            (t
             `(progn
                (unless (and (typep ,<b> ',type)
                             (typep ,<e> ',type))
                  (star-error "begin ~S and end ~S are not both of type ~S"
                              ,<b> ,<e> ',type))
                (let ((ft (widest-float-type ,<b> ,<e>)))
                  (if ft
                      (values (coerce ,<b> ft)
                              (coerce ,<e> ft))
                    (values ,<b> ,<e>))))))
          (declare (type ,type ,<begin> ,<end>)))
         ((,<step>)
          (if (>= ,<end> ,<begin>)
              ,(coerce 1 type)
            ,(coerce -1 type))
          (declare (type ,type ,<step>)))
         ((,<v> ,<limit>)
          (values
           ,(if from-p
                <begin>
              `(+ ,<begin> ,<step>))
           ,(if to-p
                `(+ ,<end> ,<step>)
              <end>))
          (declare (type ,type ,<v> ,<limit>))))
       `(if (> ,<step> ,(coerce 0 type))
            (< ,<v> ,<limit>)
          (> ,<v> ,<limit>))
       `(prog1 ,<v>
          (incf ,<v> ,<step>)))))))

(defun literal-type-unbounded-range-optimizer (begin step step-p type from-p)
  ;; There is a literal type which can be used, and the step may be
  ;; literal.  There is no bound, which makes things significantly simpler.
  (unless (subtypep type 'real)
    (star-error "type ~S is not a subtype of REAL" type))
  (cond
   (step-p
    (with-names (<b> <s> <begin> <v> <step>)
      (values
       t
       `(((,<b> ,<s>)
          (values ,begin ,step))
         ((,<begin> ,<step>)
          ,(cond
            ((subtypep type 'float)
             `(let ((ft (widest-float-type ,<b> ,<s>)))
                (cond
                 ((not ft)
                  (star-error "not floats"))
                 ((subtypep ft ',type)
                  ;; We're good
                  (values (coerce ,<b> ',type) (coerce ,<s> ',type)))
                 (t
                  (star-error "no floats of type ~S: widest is ~S" ',type ft)))))
            (t
             `(progn
                (unless (and (typep ,<b> ',type)
                             (typep ,<s> ',type))
                  (star-error "begin ~S and step ~S are not both of type ~S"
                              ,<b> ,<s> ',type))
                (let ((ft (widest-float-type ,<b> ,<s>)))
                  (if ft
                      (values (coerce ,<b> ft) (coerce ,<s> ft))
                    (values ,<b> ,<s>))))))
          (declare (type ,type ,<begin> ,<step>)))
         ((,<v>)
          ,(if from-p <begin> `(+ ,<begin> ,<step>))
          (declare (type ,type ,<v>))))
       't
       `(prog1 ,<v>
          (incf ,<v> ,<step>)))))
   (t
    ;; no step: perhaps this should make the effective step be the
    ;; right float type.
    (let ((es (coerce 1 type)))
      (with-names (<b> <v>)
        (values
         t
         `(((,<b>) ,begin)
           ((,<v>)
            (cond
             ((subtypep type 'float)
              `(let ((ft (widest-float-type ,<b>)))
                 (cond
                  ((not ft)
                   (star-error "not a float"))
                  ((subtypep et ',type)
                   ;; We're good
                   ,(if from-p
                        `(coerce ,<v> ',type)
                      `(+ (coerce ,<v> ',type) ,es)))
                  (t
                   (star-error "begin is not a float of type ~S: it is an ~S" ',type et)))))
             (t
              `(progn
                 (unless (typep ,<b> ',type)
                   (star-error "begin ~S is not of type ~S" ,<b> ',type))
                 ,(if from-p
                      <b>
                    `(+ ,<b> ,es)))))
            (declare (type ,type ,<v>))))
         't
         `(prog1 ,<v>
            (incf ,<v> ,es))))))))

(setq *star-bootstrap* nil)
