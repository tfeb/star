;;;; Štar: builtin iterators, other than ranges
;;;
;;; Very preliminary experimental versions: most of this is to test
;;; functionality
;;;
;;; More of these should dogfood

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (("pkg" "common" "star")
  :compile t))

(in-package :org.tfeb.star/iterators)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *star-bootstrap* t))

(defun literal (form)
  ;; If FORM is a literal return its value and T.  Otherwise return
  ;; the form & NIL.  This should be safe but could think some things
  ;; are not literals which are (which is safe).  This is useful for optimizers
  (typecase form
    (boolean                            ;NIL, T
     (values form t))
    ((or symbol cons)
       (destructuring-match form
         ((q v)                         ;quoted form is a literal
          (:when (eql q 'quote))
          (values v t))
         (otherwise                     ;not a literal
          (values form nil))))
    (t                                  ;all other types are self-evaluating
     (values form t))))

(defun in-naturals (&rest arg/s)
  "Iterate over the natural numbers

Natural numbers start from 0.

BOUND, if given is an integer exclusive upper bound.  FIXNUM, if true,
says that the numbers are all fixnums, including BOUND.

Optimizable."
  (declare (dynamic-extent arg/s))
  (multiple-value-bind (bound boundp fixnum inclusive)
      (destructuring-match arg/s
        (()
         (values 0 nil nil nil))
        ((b)
         (:when (realp b))
         (values (floor b) t nil nil))
        ((&key (bound 0 boundp) fixnum inclusive)
         (:when (integerp bound))
         (values bound boundp fixnum inclusive))
        (otherwise
         (star-syntax-error `(in-naturals ,@arg/s) "what even is this?")))
    (when (and (not boundp) inclusive)
      ;; star note or warning?
      (warn "It makes no sense to ask for both no bound and an inclusive bound!"))
    (if fixnum
        (let ((v 0) (b bound))
          (declare (type fixnum v b))
          (values
           (cond
            ((not boundp)
             (constantly t))
            (inclusive
             (thunk (<= v b)))
            (t
             (thunk (< v b))))
           (thunk (prog1 v (incf v)))))
      (let ((v 0) (b bound))
        (declare (type integer v b))
        (values
         (cond
          ((not boundp)
           (constantly t))
          (inclusive
           (thunk (<= v b)))
          (t
           (thunk (< v b))))
         (thunk (prog1 v (incf v))))))))

(define-iterator-optimizer (in-naturals *builtin-iterator-optimizer-table*) (form)
  (destructuring-match form
    ((_)
     (with-names (<v>)
       (values
        t
        `(((,<v>) 0 (declare (type integer ,<v>))))
        t
        `(prog1 ,<v> (incf ,<v>))
        nil)))
    ((_ bound)
     (multiple-value-bind (b b-literal) (literal bound)
       (if b-literal
           (with-names (<v>)
             (values
              t
              `(((,<v>) 0 (declare (type integer ,<v>))))
              `(< ,<v> ,b)
              `(prog1 ,<v> (incf ,<v>))
              nil))
         (with-names (<v> <b>)
           (values
              t
              `(((,<v> ,<b>) (values 0 ,b) (declare (type integer ,<v> ,<b>))))
              `(< ,<v> ,<b>)
              `(prog1 ,<v> (incf ,<v>))
              nil)))))
    ((_ &key (bound 0 boundp) fixnum inclusive)
     (let-values (((b b-literal) (literal bound))
                  ((f f-literal) (literal fixnum))
                  ((i i-literal) (literal inclusive)))
       (let ((integer-type
              (cond ((and f-literal f)
                     'fixnum)
                    (f-literal
                     'integer)
                    (t
                     (star-note "can't assume a fixnum as ~S is not literal" f)
                     'integer))))
         (cond
          ((and b-literal i-literal)
           (let ((test (if i '<= '<)))
             (with-names (<v>)
               (values
                t
                `(((,<v>) 0 (declare (type ,integer-type ,<v>))))
                `(,test ,<v> ,b)
                `(prog1 ,<v> (incf ,<v>))
                nil))))
          (b-literal
           (star-note "INCLUSIVE ~S is not literal so compiling runtime test" i)
           (with-names (<v> <i>)
             (values
              t
              `(((,<v> ,<i>) (values 0 ,i) (declare (type ,integer-type ,<v>))))
              `(if ,<i> (<= ,<v> ,b) (< ,<v> ,b))
              `(prog1 ,<v> (incf ,<v>))
              nil)))
          (i-literal
           (let ((test (if i '<= '<)))
             (with-names (<v> <b>)
               (values
                t
              `(((,<v> ,<b>) (values 0 ,b) (declare (type ,integer-type ,<v> ,<b>))))
              `(,test ,<v> ,<b>)
              `(prog1 ,<v> (incf ,<v>))
              nil))))
          (t
           (star-note "INCLUSIVE ~S is not literal so compiling runtime test" i)
           (with-names (<v> <b> <i>)
             (values
              t
              `(((,<v> ,<b> ,<i>) (values 0 ,b ,i) (declare (type ,integer-type ,<v> ,<b>))))
              `(if ,<i> (<= ,<v> ,<b>) (< ,<v> ,<b>))
              `(prog1 ,<v> (incf ,<v>))
              nil)))))))
    (otherwise
     (star-syntax-error form "bad in-naturals form"))))

(defgeneric in-sequence (s &key)
  ;; This can't be optimized away and there's no real point in trying
  ;; (the only case where you could do anything useful is with literal
  ;; lists etc, and ... just use IN-LIST or whatever then).  But you
  ;; can extend it if you have your own sequence types
  (:method ((s list) &rest args &key)
   (apply #'in-list s args))
  (:method ((s vector) &rest args &key)
   (apply #'in-vector s args))
  (:documentation
   "Iterator for sequences

Not optimizable.  You can define methods on this for non-CL-defined
sequence types."))

(defun in-list (l &key (by #'cdr byp))
  "Iterate over a list

BY is the step, which defaults to #'CDR.

Optimizable."
  (declare (type list l) (type (function (list) list) by))
  (values
   (thunk
     (not (endp l)))
   (if (not byp)
       (thunk
         (pop l))
     (thunk
       (prog1 (car l)
         (setf l (funcall by l)))))))

(define-iterator-optimizer (in-list *builtin-iterator-optimizer-table*) (form)
  (destructuring-match form
    ((_ l)
     ;; simple case
     (with-names (<tail>)
       (values
        t
        `(((,<tail>) ,l (declare (type list ,<tail>))))
        `(not (endp ,<tail>))
        `(pop ,<tail>))))
    ((_ l &key by)
     (destructuring-match by
       ((f n)
        (:when (and (eq f 'function)
                    (symbolp n)))
        ;; This means that we have something like #'FOO where FOO is a
        ;; lexically-apparent function name
        (with-names (<tail>)
          (values
           t
           `(((,<tail>) ,l (declare (type list ,<tail>))))
           `(not (endp ,<tail>))
           `(prog1 (first ,<tail>)
              (setf ,<tail> (,n ,<tail>))))))
       (otherwise
        ;; Have to bind a function variable
        (with-names (<tail> <by>)
          (values
           t
           `(((,<tail> ,<by>) (values ,l ,by)
              (declare (type list ,<tail>)
                       (type (function (list) list) ,<by>))))
           `(not (endp ,<tail>))
           `(prog1 (first ,<tail>)
              (setf ,<tail> (funcall ,<by> ,<tail>))))))))
    (otherwise
     nil)))

(defun on-list (l &key (by #'cdr byp))
  "Iterate over tails of a list

BY gets the next tail, defailt #'CDR.

Optimizable."
  (declare (type list l) (type (function (list) list) by))
  (values
   (thunk
     (not (endp l)))
   (if (not byp)
       (thunk
         (prog1 l
           (pop l)))
     (thunk
       (prog1 l
         (setf l (funcall by l)))))))

(define-iterator-optimizer (on-list *builtin-iterator-optimizer-table*) (form)
  (destructuring-match form
    ((_ l)
     ;; simple case
     (with-names (<tail>)
       (values
        t
        `(((,<tail>) ,l (declare (type list ,<tail>))))
        `(not (endp ,<tail>))
        `(prog1 ,<tail>
           (pop ,<tail>)))))
    ((_ l &key by)
     (destructuring-match by
       ((f n)
        (:when (and (eq f 'function)
                    (symbolp n)))
        ;; This means that we have something like #'FOO where FOO is a
        ;; lexically-apparent function name
        (with-names (<tail>)
          (values
           t
           `(((,<tail>) ,l (declare (type list ,<tail>))))
           `(not (endp ,<tail>))
           `(prog1 ,<tail>
              (setf ,<tail> (,n ,<tail>))))))
       (otherwise
        ;; Have to bind a function variable
        (with-names (<tail> <by>)
          (values
           t
           `(((,<tail> ,<by>) (values ,l ,by)
              (declare (type list ,<tail>)
                       (type (function (list) list) ,<by>))))
           `(not (endp ,<tail>))
           `(prog1 ,<tail>
              (setf ,<tail> (funcall ,<by> ,<tail>))))))))
    (otherwise
     nil)))

(deftype array-dim ()
  `(integer 0 ,array-dimension-limit))

(defun in-vector (v &key (element-type 't) (simple nil))
  "Iterate over indices and values of a vector

ELEMENT-TYPE is the element type, SIMPLE says the  vector is simple.

Optimizable."
  (declare (type vector v))
  (when (and simple (not (eql element-type 't)))
    (warn "element-type not T when vector is simple (but nothing cares)"))
  (let ((l (length v))
        (i 0))
    (declare (type array-dim l i))
    (values
     (thunk
       (< i l))
     (if simple
         (thunk
           (multiple-value-prog1 (values (svref v i) i)
             (incf i)))
       (thunk
         (thunk
           (multiple-value-prog1 (values (aref v i) i)
             (incf i))))))))

(define-iterator-optimizer (in-vector *builtin-iterator-optimizer-table*) (form)
  (destructuring-match form
    ((_ vector &key (element-type 't) (simple 'nil))
     (let-values (((element-type element-type-literal) (literal element-type))
                  ((simple simple-literal) (literal simple)))
       (when (and simple-literal simple element-type-literal (not (eql element-type t)))
         (warn "ignoring element-type for simple array"))
       (cond
        ((and simple-literal simple)
         (with-names (<v> <l> <i>)
           (values
            t
            ;; bind the vector then its indices
            `(((,<v>) ,vector
               (declare (type simple-vector ,<v>)))
              ((,<l> ,<i>) (values (length ,<v>) 0)
               (declare (type array-dim ,<l> ,<i>))))
            `(< ,<i> ,<l>)
            `(multiple-value-prog1 (values (svref ,<v> ,<i>) ,<i>)
               (incf ,<i>)))))
        (element-type-literal
         (with-names (<v> <l> <i>)
           (values
            t
            ;; bind the vector then its indices
            `(((,<v>) ,vector
               (declare (type (vector ,element-type) ,<v>)))
              ((,<l> ,<i>) (values (length ,<v>) 0)
               (declare (type array-dim ,<l> ,<i>))))
            `(< ,<i> ,<l>)
            `(multiple-value-prog1 (values (aref ,<v> ,<i>) ,<i>)
               (incf ,<i>)))))
        (t
         (with-names (<v> <l> <i>)
           (values
            t
            ;; bind the vector then its indices
            `(((,<v>) ,vector
               (declare (type vector ,<v>)))
              ((,<l> ,<i>) (values (length ,<v>) 0)
               (declare (type array-dim ,<l> ,<i>))))
            `(< ,<i> ,<l>)
            `(multiple-value-prog1 (values (aref ,<v> ,<i>) ,<i>)
               (incf ,<i>))))))))
    (otherwise
     nil)))

(defun in-hash-table (h)
  ;; This iterator maps two values.  As it stands it has to cons lists
  ;; of keys & values for the hashtable.  See the optimizer for how
  ;; this is avoided
  "Iterate the keys and values of a hash-table

Optimizable."
  (declare (type hash-table h))
  (multiple-value-bind (keys values)
      (with-collectors (key value)
        (maphash (lambda (k v)
                   (key k)
                   (value v))
                 h))
    (values
     (thunk
       (not (null keys)))
     (thunk
       (multiple-value-prog1
           (values (first keys)
                   (first values))
         (setf keys (rest keys)
               values (rest values)))))))

(define-iterator-optimizer (in-hash-table *builtin-iterator-optimizer-table*) (form)
  ;; This optimizer is what wrappers are for
  (destructuring-match form
    ((_ h)
     (with-names (<hash-iterator> <hash-valid> <hash-key> <hash-value>)
       (values
        t
        `(((,<hash-valid> ,<hash-key> ,<hash-value>) (,<hash-iterator>)))
        <hash-valid>
        `(multiple-value-prog1
             (values ,<hash-key> ,<hash-value>)
           (multiple-value-setq (,<hash-valid> ,<hash-key> ,<hash-value>) (,<hash-iterator>)))
        (lambda (form)
          `(with-hash-table-iterator (,<hash-iterator> ,h) ,form)))))
    (otherwise
     nil)))

(defun in-package-symbols (package/s &key (when '(:internal :external :inherited)))
  "Iterate the symbols of a package or packages

This iterates two values: symbol and package

WHEN allows you to select symbols, it should be either one of the
symbols :INTERNAL, :EXTERNAL or :INHERITED or a list of zero or more
of these symbols.  The default is a list of all three.  If the
implementation supports other statuses for symbols, they can appear
here."
  (let* ((when (etypecase when
                 (list when)
                 (symbol (list when))))
         (spl
          (collecting
            ;; Use WITH-PACKAGE-ITERATOR rather than DO-SYMBOLS in the
            ;; hope that things come out in the same order
            (with-package-iterator (pi package/s :internal :external :inherited)
              ;; This is sequentially* but we can't use it yet.
              (for (((found symbol status package) (values (constantly t)
                                                           (thunk (pi)))))
                (cond
                 ((not found)
                  (final))
                 ((member status when)
                  (collect (cons symbol package)))))))))
    (values
     (thunk
       (not (null spl)))
     (thunk
       (destructuring-bind ((symbol . package) . nspl) spl
         (setf spl nspl)
         (values symbol package))))))

(define-iterator-optimizer (in-package-symbols *builtin-iterator-optimizer-table*) (form)
  ;; WITH-PACKAGE-ITERATOR is really poorly designed, sadly: you can't
  ;; tell it the times you want a symbol dynamically.  So the
  ;; optimizer works by selecting all symbols and filtering them.
  ;; Whether it's worth having an optimizer at all is not clear
  (destructuring-match form
    ((_ p &key (when ''(:internal :external :inherited)))
     (with-names (<when> <pi> <s> <p>)
       (values
        t
        `(((,<when> ,<s> ,<p>)
           (values (let ((when ,when))
                     (etypecase when
                       (list when)
                       (symbol (list when))))
                   nil nil)))
        `(for (((found symbol accessibility package) (sequentially* (,<pi>))))
           (cond
            ((not found)
             (final nil))
            ((member accessibility ,<when>)
             (setf ,<s> symbol
                   ,<p> package)
             (final t))))
        `(values ,<s> ,<p>)
        (lambda (form)
          `(with-package-iterator (,<pi> ,p :internal :external :inherited)
             ,form)))))
    (otherwise
     nil)))

;;;; The very general stepping iterator
;;;

(defun expand-stepping (clauses &optional (starred nil))
  (multiple-value-bind (bindings values tds tests thens)
      (with-collectors (binding value td test then)
        (for ((clause (in-list clauses)))
          (destructuring-match clause
            ((var &key (value t) (initially nil) (then nil thenp)
                  (type nil typep)
                  (while nil whilep) (until nil untilp))
             (:when (symbolp var))
             (binding `(,var ,initially))
             (when value (value var))
             (when typep (td `(type ,type ,var)))
             (when thenp (then `(,var ,then)))
             (when whilep (test while))
             (when untilp (test `(not ,until))))
            ((var &key (value t) (as nil asp)
                  (type nil typep)
                  (while nil whilep) (until nil untilp))
             (:when (and (symbolp var) asp))
             (binding `(,var ,as))
             (then `(,var ,as))
             (when value (value var))
             (when typep (td `(type ,type ,var)))
             (when whilep (test while))
             (when untilp (test `(not ,until))))
            (otherwise
             (star-syntax-error clause "bad stepping clause")))))
    (let ((assignments (mapcan (lambda (then)
                                 (list (first then) (second then)))
                               thens))
          (msetf (if starred 'setf 'psetf))
          (bind (if starred 'let* 'let)))
      `(,bind ,bindings
         (declare ,@tds)
         (values
          ,(case (length tests)
             (0
              '(constantly t))
             (1
              `(thunk
                 ,(first tests)))
             (otherwise
              `(thunk
                 (and ,@tests))))
          ,(case (length values)
             (0
              '#'values)
             (1
              (case (length thens)
                (0
                 `(thunk ,(first values)))
                (1
                 `(thunk
                    (prog1 ,(first values)
                      (setf ,@assignments))))
                (otherwise
                 `(thunk
                    (prog1 ,(first values)
                      (,msetf ,@assignments))))))
             (otherwise
              (case (length thens)
                (0
                 `(thunk (values ,@values)))
                (1
                 `(thunk
                    (multiple-value-prog1 (values ,@values)
                      (setf ,@assignments))))
                (otherwise
                 `(thunk
                    (multiple-value-prog1 (values ,@values)
                      (,msetf ,@assignments))))))))))))

(defmacro stepping (&rest clauses)
  "Iterator which can step multiple variables in parallel

See the manual.  Optimizable."
  (expand-stepping clauses))

(defmacro stepping* (&rest clauses)
  "Iterator which can step multiple variables in series

See the manual.  Optimizable."
  (expand-stepping clauses t))

(defun optimize-stepping (form environment stack starred)
  ;; The guts of the stepping / stepping* optimizer
  ;; The trick here is to establish bindings of the variables named in
  ;; the clauses around the various update forms.  This means that
  ;; explicit assignments to those bindings get lost, which will be
  ;; documented.  This is allowed to be true also, for instance, for
  ;; DOTIMES, so this is hardly a limitation.
  (declare (ignore environment stack))
  (multiple-value-bind (public-vars secret-vars inits values ptds stds tests thens)
      (with-collectors (public-var secret-var init value ptd std test then)
        (for ((clause (in-list (rest form))))
          (destructuring-match clause
            ((var &key (value t) (initially nil) (then nil thenp)
                  (type nil typep)
                  (while nil whilep) (until nil untilp))
             (:when (symbolp var))
             (with-names ((secret var))
               (public-var var)
               (secret-var secret)
               (init initially)
               (when value (value var))
               (when typep
                 (ptd `(type ,type ,var))
                 (std `(type ,type ,secret)))
               (when thenp
                 (if starred
                     (then `(,var ,then ,secret ,var))
                   (then `(,secret ,then))))
               (when whilep (test while))
               (when untilp (test `(not ,until)))))
            ((var &key (value t) (as nil asp)
                  (type nil typep)
                  (while nil whilep) (until nil untilp))
             (:when (and (symbolp var) asp))
             (with-names ((secret var))
               (public-var var)
               (secret-var secret)
               (init as)
               (then
                (if starred
                    `(,var ,as ,secret ,var)
                  `(,secret ,as)))
               (when value (value var))
               (when typep
                 (ptd `(type ,type ,var))
                 (std `(type ,type ,secret)))
               (when whilep (test while))
               (when untilp (test `(not ,until)))))
            (otherwise
             (star-syntax-error clause "bad stepping~A clause" (if starred "*" ""))))))
    (let ((public-type-declarations (if (not (null ptds))
                                        `((declare ,@ptds))
                                      ()))
          (public-ignorable-declarations (if (not (null public-vars))
                                             `((declare (ignorable ,@public-vars)))
                                           '()))
          (secret-declarations (if (not (null stds))
                                   `((declare ,@stds))
                                 '()))
          (rebindings (mapcar #'list public-vars secret-vars)))
      (values
       t
       (if (not (null secret-vars))
           (if starred
               `((,secret-vars (let* ,(mapcar #'list public-vars inits)
                                 ,@public-type-declarations
                                 (values ,@public-vars))
                               ,@secret-declarations))
             `((,secret-vars (values ,@inits) ,@secret-declarations)))
         '())
       (case (length tests)
         (0 t)
         (1 `(let ,rebindings
               ,@public-type-declarations
               ,@public-ignorable-declarations
               ,(first tests)))
         (otherwise `(let ,rebindings
                       ,@public-type-declarations
                       ,@public-ignorable-declarations
                       (and ,@tests))))
       (if starred
           `(let ,rebindings
              ,@public-type-declarations
              (multiple-value-prog1 ,(case (length values)
                                       (1 (first values))
                                       (otherwise `(values ,@values)))
                ,@(if (not (null thens))
                      `((setf ,@(mapcan #'copy-list thens)))
                    '())))
         `(let ,rebindings
            ,@public-type-declarations
            ,@public-ignorable-declarations
            ,@(if (not (null thens))
                  `((setf ,@(mapcan #'copy-list thens)))
                '())
            ,(case (length values)
               (1 (first values))
               (otherwise `(values ,@values)))))
       nil))))

(define-iterator-optimizer (stepping *builtin-iterator-optimizer-table*) (form environment stack)
  (optimize-stepping form environment stack nil))

(define-iterator-optimizer (stepping* *builtin-iterator-optimizer-table*) (form environment stack)
  (optimize-stepping form environment stack t))

(defmacro stepping-values (&whole form
                                  (&rest vars)
                                  &body kws
                                  &key
                                  (as nil asp)
                                  (initially (if asp
                                                 as
                                               `(values ,@(make-list (length vars)
                                                                     :initial-element 'nil)))
                                             initiallyp)
                                  (then (if asp as nil) thenp)
                                  (types nil typesp)
                                  (values `(values ,@vars))
                                  (while t whilep)
                                  (until nil untilp))
  "Iterator which steps multiple values

See the manual.  Optimizable."
  ;; Arguably this would be nicer if it was (stepping-values ...
  ;; (:initially ...) ...), but as it is it is as compatible with a
  ;; STEPPING clause as it can be.
  (declare (ignore kws))
  (unless (every #'symbolp vars)
    (star-syntax-error form "variables specification is not a list of symbols"))
  (when (and asp (or initiallyp thenp))
    (star-syntax-error form "can't have both AS and INITIALLY / THEN"))
  `(multiple-value-bind ,vars ,initially
     ,@(if typesp
           `((declare ,@(mapcar (lambda (type var)
                                  `(type ,type ,var))
                                types vars)))
         '())
     (values
      ,(cond
        ((and whilep untilp)
         `(thunk (and ,while (not ,until))))
        (whilep
         `(thunk ,while))
        (untilp
         `(thunk (not ,until)))
        (t
         '(constantly t)))
      ,(if (or thenp asp)
           `(thunk
              (multiple-value-prog1 ,values
                (multiple-value-setq ,vars ,then)))
         `(thunk ,values)))))

(define-iterator-optimizer (stepping-values *builtin-iterator-optimizer-table*) (form)
  (destructuring-match form
    ((_ (&rest vars) &key
        (as nil asp)
        (initially (if asp
                       as
                     `(values ,@(make-list (length vars)
                                           :initial-element 'nil)))
                   initiallyp)
        (then (if asp as nil) thenp)
        (types nil typesp)
        (values `(values ,@vars))
        (while t whilep)
        (until nil untilp))
     (:when (and (every #'symbolp vars)
                 (or (not asp) (not (or initiallyp thenp)))))
     (let* ((secret-vars (mapcar (lambda (var)
                                   (make-symbol (symbol-name var)))
                                 vars))
            (rebindings (mapcar #'list vars secret-vars))
            (secret-declarations (if typesp
                                     `((declare ,@(mapcar (lambda (type secret)
                                                            `(type ,type ,secret))
                                                          types secret-vars)))
                                   '()))
            (public-declarations (if typesp
                                     `((declare ,@(mapcar (lambda (type var)
                                                            `(type ,type ,var))
                                                          types vars))
                                       (declare (ignorable ,@vars)))
                                   `((declare (ignorable ,@vars))))))
       (values
        t
        (if (not (null secret-vars))
            `((,secret-vars ,initially ,@secret-declarations))
          '())
        (if (or whilep untilp)
            `(let ,rebindings
               ,@public-declarations
               ,(cond
                 ((and whilep untilp)
                  `(and ,while (not ,until)))
                 (whilep
                  while)
                 (untilp
                  `(not ,until))))
          t)
        `(let ,rebindings
           ,@public-declarations
           ,(if (or thenp asp)
                `(multiple-value-prog1 ,values
                   (multiple-value-setq ,secret-vars ,then))
              values))
        nil)))
    (otherwise
     (star-syntax-error form "bad stepping-values syntax"))))

;;;; Some more interesting iterators
;;;

(defun sequentially-calling (&rest functions)
  "Iterator which sequenstially calls functions

Optimizable."
  (if (null functions)
      (values
       (constantly nil)
       (constantly nil))
    (values
     (thunk
       (not (null functions)))
     (thunk
       (multiple-value-prog1 (funcall (first functions))
         (setf functions (rest functions)))))))

(define-iterator-optimizer (sequentially-calling *builtin-iterator-optimizer-table*) (form)
  (destructuring-match form
    ((_)
     (values
      t
      '()
      nil
      nil
      nil))
    ((_ &rest fns)
     (let ((functions (make-symbol "FUNCTIONS")))
       (values
        t
        `(((,functions) (list ,@fns)))
        `(not (null ,functions))
        `(multiple-value-prog1 (funcall (first ,functions))
           (setf ,functions (rest ,functions)))
        nil)))
    (otherwise
     (star-syntax-error form "what even is this?"))))

(defmacro sequentially (&rest forms)
  "Iterator which sequentually evaluates forms

Optimizable."
  `(sequentially-calling
    ,@(mapcar (lambda (form)
                `(thunk ,form))
              forms)))

(define-iterator-optimizer  (sequentially *builtin-iterator-optimizer-table*) (form)
  (destructuring-match form
    ((_)
     (values
      t
      '()
      nil
      nil
      nil))
    ((_ &rest forms)
     (let ((functions  (make-symbol "FUNCTIONS")))
       (values
        t
        `(((,functions) (list ,@(mapcar (lambda (form)
                                          `(thunk ,form))
                                        forms))))
        `(not (null ,functions))
        `(multiple-value-prog1 (funcall (first ,functions))
           (setf ,functions (rest ,functions)))
        nil)))
    (otherwise
     (star-syntax-error form "what even is this?"))))

(defun sequentially-calling* (&rest functions)
  "Iterator which sequentially calls functions, sticking on the last one

Optimizable."
  (when (null functions)
    (star-error "need at least one function"))
  (values
   (constantly t)
   (thunk
     (multiple-value-prog1 (funcall (first functions))
       (when (rest functions)
         (setf functions (rest functions)))))))

(define-iterator-optimizer  (sequentially-calling* *builtin-iterator-optimizer-table*) (form)
  (destructuring-match form
    ((_ &rest fns)
     (when (null fns)
       (star-error "need at least one function"))
     (let ((functions  (make-symbol "FUNCTIONS")))
       (values
        t
        `(((,functions) (list ,@fns)))
        t
        `(multiple-value-prog1 (funcall (first ,functions))
           (when (rest ,functions)
             (setf ,functions (rest ,functions))))
        nil)))
    (otherwise
     (star-syntax-error form "what even is this?"))))

(defmacro sequentially* (&rest forms)
  "Iterator which sequentially evaluates forms, sticking on the last one

Optimizable."
  `(sequentially-calling*
    ,@(mapcar (lambda (form)
                `(thunk ,form))
              forms)))

(define-iterator-optimizer  (sequentially* *builtin-iterator-optimizer-table*) (form)
  (destructuring-match form
    ((_ &rest forms)
     (when (null forms)
       (star-error "need at least one form"))
     (let ((functions  (make-symbol "FUNCTIONS")))
       (values
        t
        `(((,functions) (list ,@(mapcar (lambda (form)
                                          `(thunk ,form))
                                        forms))))
        t
        `(multiple-value-prog1 (funcall (first ,functions))
           (when (rest ,functions)
             (setf ,functions (rest ,functions))))
        nil)))
    (otherwise
     (star-syntax-error form "what even is this?"))))

(defun cyclically-calling (&rest functions)
  "Iterator which cycles round functions

Optimizable."
  (if (null functions)
      (values
       (constantly nil)
       (constantly nil))
    (let ((ftail functions))
      (values
       (constantly t)
       (thunk
         (multiple-value-prog1 (funcall (first ftail))
           (setf ftail (or (rest ftail) functions))))))))

(define-iterator-optimizer (cyclically-calling *builtin-iterator-optimizer-table*) (form)
  (destructuring-match form
    ((_)
     (values
      t
      '()
      nil
      nil
      nil))
    ((_ &rest fns)
     (let ((functions (make-symbol "FUNCTIONS"))
           (ftail (make-symbol "FTAIL")))
       (values
        t
        `(((,functions) (list ,@fns))
          ((,ftail ,functions)))
        t
        `(multiple-value-prog1 (funcall (first ,ftail))
           (setf ,ftail (or (rest ,ftail) ,functions)))
        nil)))
    (otherwise
     (star-syntax-error form "what even is this?"))))

(defmacro cyclically (&rest forms)
  "Iterator which cyclically evaluates forms

Optimizable."
  `(cyclically-calling
    ,@(mapcar (lambda (form)
                `(thunk ,form))
              forms)))

(define-iterator-optimizer  (cyclically *builtin-iterator-optimizer-table*) (form)
  (destructuring-match form
    ((_)
     (values
      t
      '()
      nil
      nil
      nil))
    ((_ &rest forms)
     (let ((functions (make-symbol "FUNCTIONS"))
           (ftail (make-symbol "FTAIL")))
       (values
        t
        `(((,functions) (list ,@(mapcar (lambda (form)
                                          `(thunk ,form))
                                        forms)))
          ((,ftail) ,functions))
        t
        `(multiple-value-prog1 (funcall (first ,ftail))
           (setf ,ftail (or (rest ,ftail) ,functions)))
        nil)))
    (otherwise
     (star-syntax-error form "what even is this?"))))

(defmacro in-iterators (&rest iterator-forms)
  "Iterato which evaluates its iterator arguments in sequence

Optimizable."
  `(in-delayed-iterators ,@(mapcar (lambda (iterator-form)
                                     `(thunk
                                        ,iterator-form))
                                   iterator-forms)))

(define-iterator-optimizer (in-iterators *builtin-iterator-optimizer-table*) (form)
  (destructuring-match form
    ((_)
     (values
      t
      '()
      nil
      nil
      nil))
    ((_ &rest iterator-forms)
     (with-names (<valid> <valids> <cursor> <cursors>)
       (values
        t
        `(((,<valids> ,<cursors>)
           (with-collectors (,<valid> ,<cursor>)
             ,@(mapcar (lambda (iterator-form)
                         `(multiple-value-bind (v c) ,iterator-form
                            (,<valid> v)
                            (,<cursor> c)))
                       iterator-forms)))
          ((,<valid> ,<cursor>) (values (pop ,<valids>)
                                        (pop ,<cursors>))))
        `(or (funcall ,<valid>)
             (for ((vtail (on-list ,<valids>))
                   (ctail (on-list ,<cursors>)))
               (destructuring-bind (valid . vtt) vtail
                 (when (funcall valid)
                   (setf ,<valid> valid
                         ,<valids> vtt
                         ,<cursor> (first ctail)
                         ,<cursors> (rest ctail))
                   (final t)))))
        `(funcall ,<cursor>)
        nil)))
    (otherwise
     nil)))

(defun in-delayed-iterators (&rest delayed-iterators)
  (if (null delayed-iterators)
      (values
       (constantly nil)
       (constantly nil))
    (multiple-value-bind (valid value) (funcall (first delayed-iterators))
      (let ((more-delayed-iterators (rest delayed-iterators)))
        (values
         (thunk
           (or
            (funcall valid)
            (iterate next ((itail more-delayed-iterators))
              (if (null itail)
                  nil
                (destructuring-bind (this . more) itail
                  (multiple-value-bind (new-valid new-value) (funcall this)
                    (let ((try (funcall new-valid)))
                      (cond
                       (try
                        (setf valid new-valid
                              value new-value
                              more-delayed-iterators more)
                        try)
                       (t
                        (next more))))))))))
         (thunk
           (funcall value)))))))

(defmacro in-parallel-iterators (&rest iterator-forms)
  "Iterator which returns multiple values from its component iterators

Optimizable."
  `(in-delayed-parallel-iterators ,@(mapcar (lambda (iterator-form)
                                              `(thunk
                                                 ,iterator-form))
                                            iterator-forms)))

(define-iterator-optimizer (in-parallel-iterators *builtin-iterator-optimizer-table*) (form)
  (destructuring-match form
    ((_)
     (values
      t
      '()
      nil
      nil
      nil))
    ((_ &rest iterator-forms)
     (let ((valid-vars (collecting
                         (iterate next ((i 0)
                                        (ift iterator-forms))
                           (unless (null ift)
                             (collect (make-symbol (format nil "VALID-~D" i)))
                             (next (1+ i) (rest ift))))))
           (cursor-vars (collecting
                          (iterate next ((i 0)
                                         (ift iterator-forms))
                            (unless (null ift)
                              (collect (make-symbol (format nil "CURSOR-~D" i)))
                              (next (1+ i) (rest ift)))))))
       (values
        t
        (mapcar (lambda (valid-var cursor-var iterator-form)
                  `((,valid-var ,cursor-var) ,iterator-form))
                valid-vars cursor-vars iterator-forms)
        `(and ,@(mapcar (lambda (v)
                          `(funcall ,v))
                        valid-vars))
        `(values ,@(mapcar (lambda (c)
                             `(funcall ,c))
                           cursor-vars))
        nil)))
    (otherwise
     nil)))

(defun in-delayed-parallel-iterators (&rest delayed-iterators)
  (if (null delayed-iterators)
      (values
       (constantly nil)
       (constantly nil))
    (multiple-value-bind (valids cursors)
        (with-collectors (valid cursor)
          (for ((delayed-iterator (in-list delayed-iterators)))
            (multiple-value-bind (v c) (funcall delayed-iterator)
              (valid v)
              (cursor c))))
      (values
       (thunk
         (every #'funcall valids))
       (thunk
         (let ((l (mapcar #'funcall cursors)))
           (declare (dynamic-extent l))
           (values-list l)))))))

(defun always (v)
  ;; Is this even useful?  I think it is in for*
  "Iterator which returns its argument always

Optimizable."
  (values
   (constantly t)
   (constantly v)))

(define-iterator-optimizer (always *builtin-iterator-optimizer-table*) (form)
  (destructuring-match form
    ((_ v)
     (with-names (<v>)
       (values
        t
        `(((,<v>) ,v))
        't
        <v>
        nil)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *star-bootstrap* nil))
