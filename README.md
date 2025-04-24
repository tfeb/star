# Štar: a simple and extensible iteration construct
Štar is a concise and extensible iteration construct for Common Lisp which aims to be pleasant to use, easy to understand, general, and to not look like Fortran.

For an overview of Štar, see [here](https://www.tfeb.org/fragments/2024/05/15/an-iteration-construct-for-common-lisp/ "Štar: an iteration construct for Common Lisp"): this document is just the reference manual.

At the time of writing, Štar itself is probably fairly complete.  The predefined iterators are less finished.  Until this paragraph goes away you should use them with some caution: things may change in incompatible ways.  I have tried to indicate the more flaky iterators below.

## Contents
- [Overview](#overview)
- [Iterators](#iterators)
- [Restrictions on iterator functions](#restrictions-on-iterator-functions)
- [The iteration constructs: for and for\*](#the-iteration-constructs-for-and-for)
	- [Parallel iteration: for](#parallel-iteration-for)
	- [Nested iteration: for\*](#nested-iteration-for)
- [Declaration handling](#declaration-handling)
- [Iterator optimizers](#iterator-optimizers)
	- [Specifications of variable bindings](#specifications-of-variable-bindings)
	- [Valid form](#valid-form)
	- [Cursor form](#cursor-form)
	- [Wrapping function](#wrapping-function)
	- [Variable type information](#variable-type-information)
	- [An example](#an-example)
	- [The iterator optimizer tables](#the-iterator-optimizer-tables)
	- [Defining iterator optimizers: define-iterator-optimizer](#defining-iterator-optimizers-define-iterator-optimizer)
	- [The second and third arguments](#the-second-and-third-arguments)
- [Some useful things](#some-useful-things)
	- [Errors](#errors)
	- [Compilation notes](#compilation-notes)
	- [Miscellany](#miscellany)
- [Packages](#packages)
- [Predefined iterators](#predefined-iterators)
	- [Simple numeric iteration: in-naturals](#simple-numeric-iteration-in-naturals)
	- [List iteration: in-list, on-list](#list-iteration-in-list-on-list)
	- [Vectors and general sequences: in-vector, in-sequence](#vectors-and-general-sequences-in-vector-in-sequence)
	- [Hash tables: in-hash-table](#hash-tables-in-hash-table)
	- [Packages: in-package-symbols](#packages-in-package-symbols)
	- [Three general iterators: stepping, stepping\* and stepping-values](#three-general-iterators-stepping-stepping-and-stepping-values)
	- [Meta iterators](#meta-iterators)
- [Package exports](#package-exports)
	- [org.tfeb.star/utilities](#orgtfebstarutilities)
	- [org.tfeb.star/iterator-optimizer-protocol](#orgtfebstariterator-optimizer-protocol)
	- [org.tfeb.\*](#orgtfeb)
	- [org.tfeb.star/iterators](#orgtfebstariterators)
	- [org.tfeb.star](#orgtfebstar)
- [An extension: loop unrolling](#an-extension-loop-unrolling)
	- [The unroller protocol](#the-unroller-protocol)
	- [Unrolling itself](#unrolling-itself)
	- [An example unroller](#an-example-unroller)
	- [Notes on unrolling](#notes-on-unrolling)
- [An approximate version history](#an-approximate-version-history)
- [Notes](#notes)

## Overview
Štar is an iteration construct.  That's all it is: it does not accumulate values, destructure bindings or do your washing.  It doesn't do all those other things because there are better tools which do them already which can be used freely with Štar.  Similarly there are no special magic conditional mechanisms: rather there are local functions which can skip to the next iteration or terminate the iteration, and you use these with normal conditional expressions.

## Iterators
Štar has no built-in knowledge of how to iterate over lists, or over ranges of reals or whatever.  Rather it treats iteration by asking two questions:

- is there more?
- if there is, what is it?

An *iterator* is an expression which can answer these two questions.  It does this by evaluating to two functions:

- a function of no arguments which returns true if there is more;
- a function of no arguments which is called only when the first function has returned true and should return the value or values for the next step.

There are a number of precanned iterators described below, but it is easy to write your own.  Here is a simple iterator which iterates over strings:

```lisp
(defun in-string (s)
  (let ((i 0)
        (l (length s)))
    (values
     (lambda ()
       (< i l))
     (lambda ()
       (prog1 (char s i)
         (incf i))))))
```

And this iterator can now be used:

```lisp
> (for ((c (in-string "foo")))
    (print c))

#\f
#\o
#\o
```

The 'cursor functions' of iterators can return more than one value:

```lisp
(defun in-alist (alist)
  (values
   (lambda ()
     (not (null alist)))
   (lambda ()
     (destructuring-bind ((k . v) . more) alist
       (setf alist more)
       (values k v)))))
```

And now

```lisp
> (for (((k v) (in-alist '((a . 1) (b . 2)))))
    (format t "~&~S -> ~S~%" k v))
a -> 1
b -> 2
```

That's all there is to writing iterators.  Note it's perfectly possible to construct iterators on the fly:

```lisp
(for ((c (let ((i 0)
               (l (length string)))
           (values
            (lambda ()
              (< i l))
            (lambda ()
              (prog1 (char string i)
                (incf i)))))))
  ...)
```

Iterators require two function calls per step.  To make them fast you can define *iterator optimizers* for named iterators, which return values which allow the system to do much better than this.  These are described below.

Note that, for Štar, *everything* is an iterator: there is no special syntax.  So any form

```lisp
(for ((<var/s> <iterator>))
  ...)
```

can be freely replaced by

```lisp
(multiple-value-bind (v c) <iterator>
  (for ((<var/s> (values v c)))
    ...)
```

with no cost other than performance.

## Restrictions on iterator functions
It is not specified which of the two functions steps the iterator.  So for an iterator *i* returning functions *v* and *c* you must call *v*, once, and only if it returns true you may then call *c*, once.  You may not call either *v* or *c* more than once without calling the other, and you may not call *c* unless an immediately preceeding call to *v* has returned true.  Once *v* has returned false you should not call either *v* or *c* again.

## The iteration constructs: `for` and `for*`
These are exported by `org.tfeb.*` and `org.tfeb.star`.
### Parallel iteration: `for`
`for` iterates over zero or more iterators.  The syntax is

`(for` `(`*clause*\*`)` *declaration*\* *form*\*`)`

Where each *clause* is either

`(`*var* *iterator*`)`

or

`((`*var*\*`)` *iterator*`)`

and finally a *var* is either a symbol naming a variable, or an expression like

`(`*name* `&key type special ignore ignorable anonymous)`

which allows various declarations to be made about variables.  See [below](#declaration-handling "Declaration handling") for some more about declaration handling: these options are very seldom needed now.

Variables whose name is `"_"` are anonymous by default: each such variable is unique and is ignored.  You could make such a variable not be anonymous by specifying it as `(_ :anonymous nil)`.

The other options for the complicated variable case are now mostly unneeded: there are some cases where they are in theory useful in`for*`: for `for` declarations at the start of the body do what you expect.

Finally an *iterator* is just a form which evaluates to two functions as specified above.

Within the lexical scope and dynamic extent of the body four local functions are defined:

- **`next`** skips immediatly to the next iteration;
- **`next*`** skips to the next *outer* iteration: this is the same as `next` for `for`.
- **`final`** exits the iteration, returning its arguments as multiple values;
- **`final*`** exits the *outer* iteration and is the same as `final` for `for`.

The body is surrounded by blocks named `for` and `nil`.

Variables are bound in parallel: it is in the nature of the construct that sequential bindings make no sense, since iterators are evaluated before any variables are bound.

Variables are bound for each step (it is possible this might change: don't rely on it).

Iterator optimizers may provide implicit type information for variable bindings.  If`*obey-iterator-optimizer-types*` is true at macroexpansion time then corresponding declarations for iteration variables will be interpolated into the expansion.  This means that code like

```lisp
(for ((i (in-iterator-with-fixnum-values)))
  ...
  (setf i t)
  ...)
```

may not be sanitary.  You can tell Štar to ignore this implicit type information by making `*obey-iterator-optimizer-types*`be `nil` at macroexpansion time.  But just don't assign values to iteration variables.

Here is an example which iterates over a list an a string, using the `in-string` iterator defined above.  This declares the type of the variable.

```lisp
(defun ts (l s)
  (for ((e (in-list l))
        ((c :type character) (in-string s)))
    (when (eql e c)
      (final c))))
```

This can be written less clumsily like this:

```lisp
(defun ts (l s)
  (for ((e (in-list l))
        (c (in-string s)))
    (declare (type character c))
    (when (eql e c)
      (final c))))
```

This version is entirely equivalent to the first.

### Nested iteration: `for*`
`for*` has exactly the same syntax as `for` but it defines a number of *nested* iterations, with each clause being nested inside the one before it.

```lisp
(defun search-for-char (string char-bag)
  (for* ((c (in-string string))
         (s (in-list char-bag)))
    (when (eql c s)
      (final* c))))
```

This is a case where the fancy form of variables is occasionally useful: if you have repeated variables but want a declaration to apply to only one of them, then you can use the fancy variable form to do this.  This is now, at best, an obscure case, but it's there just in case.  See [below](#declaration-handling "Declaration handling") for more about declaration handling.

```lisp
(defun search-for-char (string char-bag)
  (for* (((c :type character) (in-string string))
         (s (in-list char-bag)))
    (when (eql c s)
      (final* c))))
```

As an aside, here is one way that the above function could be made more interesting:

```lisp
(defun search-for-char (string char-bag)
  (for* (((c i) (in-parallel-iterators
                 (in-string string)
                 (in-range '*)))
         (s (in-list char-bag)))
    (when (eql c s)
      (final* c i))))
```

Now you get the index as well as the character:

```
> (search-for-char "foo" '(#\g #\o))
#\o
1
```

`for*` is just like nested `for`s, except:

- it wraps a block named `for*` around the whole iteration, and a block named `nil` around the body;
- `final*` returns from the whole iteration while `final` just terminates the innermost loop;
- `next*` skips to the next outer-level iteration while `next` skips to the next innermost loop.

Both `for` and `for*` return `nil` unless values are returned by `final` or `final*`.

## Declaration handling
The first version of Štar made no attempt to do anything smart with declarations, relying instead on the fancy variable form.  [Version 2](#an-approximate-version-history "An approximate version history") and later use my `process-declarations` hack to work out which declarations in a form apply to which variables and to raise them to the appropriate place.  Štar treats a bound variable declaration as applying to all variables with the given name[^1].  This doesn't matter for `for`, but it does for `for*`, since bindings may be repeated.  Thus in a form like

```lisp
(for* ((a (in-range 10))
       (a (in-range a)))
  (declare (type fixnum a))
  ...)
```

The `fixnum` declaration applies to both bindings of `a`.  If you want it to apply to only one, you need to use the hairy variable form (this is the only real instance where it's even potentially useful now):

```lisp
(for* (((a :type fixnum) (in-range ...))
       (a (in-range (1+ a))))
  ...)
```

It's rather hard to think of cases where this might actually be useful, but the facility is still there.

From [version 5](#an-approximate-version-history "An approximate version history"), iterator optimizers can implicitly specify the types of the variables which will be bound to their values.  `*obey-iterator-optimizer-types*` gives control over this at macroexpansion time.

Don't assign to bound iteration variables: they may have type restriction you don't know and perhaps, one day, they will not even be variables.

## Iterator optimizers
The protocol described here is exported by `org.tfeb.star/iop` and `org.tfeb.star`.

Iterator optimizers are functions which tell Štar how to essentially inline iterators.   They are attached to iterators which are named functions.  They are looked up at macroexpansion time in a stack of tables: this is usually just two, with a user table and a builtin table underneath it which is protected, but you can push and pop other entries, which might be useful, for instance, when compiling code where you can make specific assumptions about types which are not generally true.

Because iterator optimizers work on names, they suffer from the unavoidable 'upward macro hygiene' problem that most CL macros suffer from[^2].  Here is an example of an upward macro hygiene problem in Štar:

```lisp
(flet ((in-list (...) ...))
  (for ((e (in-list ...)))
    ...))
```

Here the iterator optimizer for `in-list` will assume it is optimizing the global function, not the local one.  There is no general solution to this problem in CL.  But, equally, it  is very seldom a real problem.

An iterator optimizer is usually defined with `define-iterator-optimizer` but it is possible to add entries to the tables by hand.

An iterator optimizer function takes one argument and two optional arguments: the form to be optimized optionally an environment object, or `nil`, and the tail of the stack of optimizer tables at the point is was found, or `nil`.   When defining an iterator optimizer with `define-iterator-optimizer` the second two arguments, or just the third can be omitted, and a suitable function will be created.  It should return either `nil` if it declines to optimize the form, or four or five values:

- true to say it can optimize the form;
- a specification for variable bindings;
- a form which says if the iterator is valid;
- a form which produces the values for the next step;
- optionally some additional information which is one of
	-  a function designator (a function or symbol with a global function binding) which is called with the whole form and can wrap code around it;
	- a plist with possible keys `:wrapper`, `:types` and optionally other, unused keys which specifies one or both of the wrapping function and possible variable types.

Originally it was only possible to specify a wrapper: by using a plist as the fifth value an optimizer can specify  a wrapper, type and in future perhaps other information.

### Specifications of variable bindings
This is a list of what are essentially the first two arguments to `multiple-value-bind`, together with zero or more declarations.  This list can have multiple elements which correspond to nested bindings and in which, therefore, later bindings can refer to earlier ones.

### Valid form
This is just a form which should evaluate to true if the iterator is valid;

### Cursor form
This is a form which should evaluate to the values for the next step.

### Wrapping function
This is a function of one argument, which is called at macroexpansion time with the whole body of the iteration form.  It is useful to wrap code around the whole form.

### Variable type information
Using the `:types` key, an optimizer can provide type information for variables.  The value of the key should be a list of type designators for each value of the iterator: `nil` means 'no type information for this value' (this is really the same as `t` except no declaration will be inserted in the `nil` case).  So, for instance, an optimizer for an iterator which returns a single value might provide `(... :types (fixnum) ...)` to say that it is always a fixnum.  See `*obey-iterator-optimizer-types*` below for whether type information is actually inserted.

### An example
Here is an iterator which maps over hash-tables:

```lisp
(defun in-hash-table (h)
  ;; This iterator maps two values.  As it stands it has to cons lists
  ;; of keys & values for the hashtable.  See the optimizer for how
  ;; this is avoided
  (declare (type hash-table h))
  (multiple-value-bind (keys values)
      (with-collectors (key value)
        (maphash (lambda (k v)
                   (key k)
                   (value v))
                 h))
    (values
     (lambda ()
       (not (null keys)))
     (lambda ()
       (multiple-value-prog1
           (values (first keys)
                   (first values))
         (setf keys (rest keys)
               values (rest values)))))))
```

This has the very obvious problem that it must build lists of keys and values ahead of time.   Here is an optimizer for this.

```lisp
(define-iterator-optimizer in-hash-table (form)
  ;; This optimizer is what wrappers are for
  (destructuring-match form
    ((_ h)
     (let ((iterator (make-symbol "HASH-ITERATOR"))
           (valid (make-symbol "HASH-VALID"))
           (key (make-symbol "HASH-KEY"))
           (value (make-symbol "HASH-VALUE")))
       (values
        t
        `(((,valid ,key ,value) (,iterator)))
        valid
        `(multiple-value-prog1
             (values ,key ,value)
           (multiple-value-setq (,valid ,key ,value) (,iterator)))
        (lambda (form)
          `(with-hash-table-iterator (,iterator ,h) ,form)))))
    (otherwise
     nil)))
```

This creates a bunch of gensymed names, and then returns `t` and a form which will turn into a binding like:

```lisp
(multiple-value-bind (valid key value) (iterator)
  ...)
```

then the valid form is just `valid`, while the cursor form is

```lisp
(multiple-alue-prog1 (values key value)
  (multiple-value-setq (valid key value) (iterator))
```

Finally it returns a function which wraps  a suitable `with-hash-table-iterator` form around the whole iteration.

### The iterator optimizer tables
**`make-iterator-optimizer-table`** makes an iterator optimizer table.  These are currently just hash-tables but they might not always be, and this function also tries to make them weak on their keys.

**`*iterator-optimizers*`** is the stack of optimizer tables.  It initially has two entries: a user table and underneath it a builtin table.  If you try to add, remove or mutate entries in the builtin table you will get a continuable error.

**`*enable-iterator-optimizers*`**, if true, enables iterator optimizers.  The default value is true.  This variable matters at macro-expansion time, not run time.

**`*obey-iterator-optimizer-types*`**, if true, enables insertion of type declarations for type informatiom from optimizers.  This restricts any assignments to variables to be of the right type.  The default is true.  This variable matters at macro-expansion time, not run time.

**`get-iterator-optimizer`** finds an iterator optimizer in a table.  It takes two arguments: the optimizer name and the table.  It is an accessor, so you can install new optimizers with it if you wish.

**`remove-iterator-optimizer`** removes an optimizer from a table.  It takes two arguments: the optimizer name and the table.

**`map-iterator-optimizer-table`** maps a function over an optimizer table.  It works like `maphash` (in fact it *is* `maphash` currently) and has the same restrictions.

Finally **`find-iterator-optimizer`**  finds an optimizer in the stack of tables.  It takes two arguments: the optimizer name and the stack of tables, defaultly the value of `*iterator-optimizers*`.  It returns tewo values: the optimizer and the point in the stack where it found it, or `nil` and `nil`.

### Defining iterator optimizers: `define-iterator-optimizer`
This is the normal way to define iterator optimizers.  The first argument is either the name of an optimizer or a list of a name and a table to define it in.  The second argument is an arglist with one to three elements: the first is the name for the form, the second, if given, for the environment, the third, if given, is the tail of the stack of optimizers where the optimizer was found.  The rest of the form is a function body.

There is no completely satisfactory answer as to when iterator optimizers should be defined.  Originally I decided that  `define-iterator-optimizer` should *not* wrap its expansion in an `eval-when`, because iterator optimizers should not be defined before their iterators are, ie not before load time.  However this means that a file which says

```lisp
(define in-foo (...) ...)

(define-iterator-optimizer in-foo (...) ...)

(defun spot (...)
  ...
  (for ((x (in-foo ...)))
    ...))
```

will not optimize the iteration.  It would obviously be better if it did.  In particular it's pretty clear that, at least within a file (or compilation unit), compiler macros *are* expanded even though their functions are not yet defined.  Whether they are expanded *after* a file containing a function definition & corresponding compiler macro definition is compiled but before it is loaded is not clear: two of the implementations I tried did so, one did not[^3].

After thinking about this I decided that examples like the above are so common that it is best for iterator optimizers to become defined in the compilation environment, even though this means they are defined before their functions.  So that is now what happens.

### The second and third arguments
The second argument to an iterator optimizer function is a CL [environment object](https://www.lispworks.com/documentation/HyperSpec/Body/26_glo_e.htm#environment_object "environment object") as passed to macro functions.  Optimizer functions can use it for the same purposes that macro functions do, if they want to.

The third argument is the tail of `*iterator-optimizers*` with the first element of the tail being the table where the optimizer was found.  This can be used to allow optimizers to partly override other versions of themselves by calling `find-iterator-optimizer` on the tail of the argument they got passed.  To use this mechanism seriously some syntax along the lines of `call-next-method` would be nice.

## Some useful things
These are exported by `org.tfeb.star/utilities` and `org.tfeb.star`.

### Errors
Any error that Štar thinks is your fault will be a condition inheriting from the `star-error` condition type.  Errors that it thinks are bugs will *not* inherit from this type.  `star-error`is a subclass of `simple-error`, and there is a function `star-error` which is like `error` for them, when `error`'s first argument is a string.

There is a subclass of `star-error` called `star-syntax-error` which is for syntax errors.  `syntax-error-form` retrieves the form that was responsible for the error.  The function `star-syntax-error` is like`error` for `star-syntax-error`s: it has a first argument which is the form and the remaining argument's are as for the stringy case of `error`.

### Compilation notes
Štar, and particularly iterator optimizers, can report various notes which might be helpful.  These are done by signalling `star-note`, which is a condition type which is a subtype of `simple-condition`.  There is a function, `star-note` which is like the stringy-case of `signal` for these conditions.

**`reporting-star-notes`** is a macro which will report star notes to a specified stream, by default `*debug-io*`.  You want to wrap this around a compilation which is when notes are reported:

```lisp
(reporting-star-notes (my-recording-stream)
  (compile "my-big-file"))
```

for instance.

### Miscellany
**`anonymous-variable-p`** returns true if a symbol names a variable that Štar would consider anonymous.  It avoids programs which want to process clauses having to second-guess this.

## Packages
The system is structured so that you can pick and choose which bits you want, starting from something extremely minimal.

- **`org.tfeb.*`** is Štar itself: just `for`, `for*`, `next`, `next*`, `final` and `final*`.
- **`org.tfeb.star/iterator-optimizer-protocol`** *aka* `org.tfeb.star/iop` is the iterator optimizer protocol.
- **`org.tfeb.star/iterators`** is the predefined iterators.
- **`org.tfeb.star/utilities`** is the names of conditions and `reporting-star-notes`.
- **`org.tfeb.star`** combines the above four packages.

See [below](#package-exports "Package exports") for an exhaustive list of what exports what.

---

## Predefined iterators
All of these are exported by `org.tfeb.star/iterators` and `org.tfeb.star`.

These are in a *much* more rudimentary state than Štar itself: many were written before it was finished, as proofs of concept, or both.  The list below may change significantly (and has done so in [version 3](#an-approximate-version-history "An approximate version history")).

### Simple numeric iteration: `in-naturals`
`in-naturals` iterates over the natural numbers: integers greater than or equal to zero.  It has two simple forms:

- `(in-naturals)` will iterate up from `0`with no bound;
- `(in-naturals bound)` will iterate up from `0` below `bound`, where `bound` should be a `real`: if it is not an `integer` the floor of it is used as the bound.

The fancy form has three keyword arguments:

- `bound` gives the bound as above;
- `fixnum`warrants that the values, including `bound` are all `fixnums`.
- `inclusive` makes the bound inclusive rather than exclusive.

This iterator is optimized pretty well.  Note that the cursor variable steps one beyond the last value returned.  This is unlikely to matter unless you want to iterate up to `most-positive-fixnum`.

See [below](#three-general-iterators-stepping-stepping-and-stepping-values "Three general iterators: stepping, stepping* and stepping-values") for a way of iterating more generally over numeric ranges.

### List iteration: `in-list`, `on-list`
`in-list` iterates over the elements of a list.  It has a single keyword argument, `by` which if given should be a function to select the appropriate nex tail of the list.  So `(in-list l :by #'cddr)` iterates in steps of two.

`on-list` is like `in-list` but iterates over successive tails.

### Vectors and general sequences: `in-vector`, `in-sequence`
`in-vector` iterates over vectors.  It iterates two values: value and index.  As well as the vector argument it takes two keyword arguments:

- `simple`, default `nil`, will assume the vector is simple;
- `element-type`, if true, will specify the element type.

`element-type`, if given as a literal type, is used by the optimizer.  If you give both a non-`t` `element-type` and `simple` as true you'll get a warning and `simple`will be ignored.

`in-vector` is perhaps subject to change.

`in-sequence` iterates over general sequences: it is a generic function.  You could define additional methods on this.  It dispatches on its first argument: all remaining arguments should be keywords.  There is no optimizer for this because you can't dispatch at macroexpansion time in general and it didn't seem worth handling literals (where you can).

### Hash tables: `in-hash-table`
This iterates two values, the key and value of the table.  See above for something very close to the real implementation.

Note that because there is no promise about the order in which things are returned by the underlying functions, `in-hash-table` can return things in any order.  In particular this means that

```lisp
(for (((k v) (in-hash-table table)))
  ...)
```

and

```lisp
(multiple-value-bind (c v) (in-hash-table table)
  (for (((k v) (values c v)))
    ...))
```

may iterate in different orders, and the order is in any case underfined.

### Packages: `in-package-symbols`
This iterates two values: symbol and package.  It takes one keyword argument which may either be a symbol which is one of the valid statuses for `find-symbol` when a symbol is found or a list of such symbols.  The default value is the list `(:internal :external :inherited)`.

`in-package-symbols` has the same caveats about order as `in-hash-table`.

### Three general iterators: `stepping`, `stepping*` and `stepping-values`
`stepping` and `stepping*` are a pair of iterators which make and step bindings.  They take a number of clauses which look either like `(var &key initially then type value while until)`, or like `(var &key as type value while until)`.

- `var` is a variable name which will be bound.
- In the first form, `initially` is the initial value of the variable, default `nil`, and `then`, if given, is an expression whose value will be the new value of the variable on each step.
- in the second form, `as` is a form which specifies the value of the variable at each step.
- `type`, if given, is a type for the variable.
- `value` means that the value of the variable will be returned from the iterator.  The default is true.
- `while` is an expression which, if it evaluates to true (for all the variables), will cause the iterator to contue.
- `until` is an expression which will stop the iteration when it evaluates to true.

`var`, `type` and `value` are literals, not evaluated as you would expect.

For `stepping` the initial values are bound with `let` and the new values with `psetf`.  For `stepping*` the initial values are bound with `let*` and the new values with `setf`.

`(stepping (a :initially 0 :then (1+ a) :while (< a 10)))` is equivalent to

```lisp
(let ((a 0))
  (declare)
  (values
   (lambda () (< a 10))
   (lambda () (prog1 a (setf a (1+ a))))))
```

Here's an example of `stepping`:

```lisp
> (for (((a b) (stepping (a :initially 0 :then (+ a 2))
                         (b :initially 10 :then (1+ b)
                            :while (> b a)))))
    (format t "~&a ~D b ~D~%" a b))
a 0 b 10
a 2 b 11
a 4 b 12
a 6 b 13
a 8 b 14
a 10 b 15
a 12 b 16
a 14 b 17
a 16 b 18
a 18 b 19
nil
```

In `stepping` and `stepping*` there is a question of what a form like

```lisp
(for ((s (stepping* (l :initially '(1 2 3) :then (rest l)
                       :until (null l) :value nil)
                    (s :initially (first l) :then (+ s (first l))))))
  ...)
```

should do, and in particular whether, after `l` becomes null, does `s` still get updated, causing an error?  There were versions of these macros which made this safe, but the performance cost, at least in the implementation I came up with, was almost a factor of two.  Notably, `do` and `do*` do *not* interleave the end test this way, so

```lisp
(do* ((l '(1 2 3) (rest l))
      (s (first l) (+ s (first l))))
     ((null l) s))
```

will signal an error.  So after some thought I have changed `stepping` and `stepping*` to behave like this as well: the termination tests happen *after* the updates, compatibly with `do` and `do*`.

`stepping-values` is conceptually similar to `stepping` but it steps a single set of multiple values.  Its syntax is `(stepping-values vars &key initially then types values while until)`, or `(stepping-values vars &key as types values while until)`.

- `vars` is a list of variables to step.
-  In the first form`initially` is a form returning multiple values for them: the default is to bind them all to`nil`, and `then` is a form returning new values, the default being not to assign new values.
- In the second form `as` is used to specify the values of the variables at each step.
- `values` is a form which returns values from the iteration.
- `types` is a list of types for the variables.
- `while` and `until` are as in `stepping`.

For example:

```lisp
(defun catching-up (a b)
  (stepping-values (small big)
    :initially (if (> a b) (values b a) (values a b))
    :types (real real)
    :then (values (+ small 2) (+ big 1))
    :while (< small big)))
```

```lisp
> (for (((a b) (catching-up 1 5)))
    (format t "~&~D ~D~%" a b))
1 5
3 6
5 7
7 8
nil
```

Here is an example of `stepping-values` which will somewhat perversely step lists:

```lisp
(defun stepping-in-list (l)
  (stepping-values (c v)
    :initially (in-list l)
    :while (funcall c)
    :values (funcall v)))
```

These three iterators are now well-optimized, and in particular forms like

```lisp
(for ((i (stepping (i :type fixnum
                      :initially 100
                      :then (1+ i)
                      :while (< i 200)))))
  (declare (type fixnum i))
  ...)
```

Should be reliably fast.

For all the `stepping` variants you should not explicitly assign to the stepped variables: in the optimized versions the stepped variables get bound in several different places, so the assignments will in general be lost.

### Meta iterators
These are iterators which iterate over things like other iterators, or functions.

**`sequentially-calling`** iterates over the functions which are its arguments, calling each in turn with no arguments.

**`sequentially`** is a macro version of `sequentially-calling`: it iterates over forms, evaluating each, once, in turn.

**`sequentially-calling*`** and **`sequentially*`** are like their unstarred versions but they 'stick' on the last argument.

**`cyclically-calling`** calls the functions which are its arguments cyclically.  **`cyclically`** is the macro equivalent.

**`in-iterators`** takes arguments which are iterators: it iterates the first until it is exhausted and so on.

**`in-parallel-iterators`** takes arguments which are iterators.  At each step it returns one value from each until one is exhausted.

**`always`** endlessly returns its argument's value.  This may go away.

## Package exports
This is a list of which public packages export what.

### `org.tfeb.star/utilities`
- `anonymous-variable-p`
- `reporting-star-notes`
- `star-error`
- `star-note`
- `star-syntax-error`
- `star-syntax-error-form`
### `org.tfeb.star/iterator-optimizer-protocol`
- `*enable-iterator-optimizers*`
- `*iterator-optimizers*`
- `*obey-iterator-optimizer-types*`
- `define-iterator-optimizer`
- `find-iterator-optimizer`
- `get-iterator-optimizer`
- `make-iterator-optimizer-table`
- `map-iterator-optimizer-table`
- `remove-iterator-optimizer`
### `org.tfeb.*`
- `final`
- `final*`
- `for`
- `for*`
- `next`
- `next*`
### `org.tfeb.star/iterators`
- `always`
- `cyclically`
- `cyclically-calling`
- `in-hash-table`
- `in-iterators`
- `in-list`
- `in-naturals`
- `in-package-symbols`
- `in-parallel-iterators`
- `in-sequence`
- `in-vector`
- `on-list`
- `sequentially`
- `sequentially*`
- `sequentially-calling`
- `sequentially-calling*`
- `stepping`
- `stepping*`
- `stepping-values`
### `org.tfeb.star`
This is a conduit for the above four packages.

- `*enable-iterator-optimizers*`
- `*iterator-optimizers*`
- `*obey-iterator-optimizer-types*`
- `always`
- `anonymous-variable-p`
- `cyclically`
- `cyclically-calling`
- `define-iterator-optimizer`
- `final`
- `final*`
- `find-iterator-optimizer`
- `for`
- `for*`
- `get-iterator-optimizer`
- `in-hash-table`
- `in-iterators`
- `in-list`
- `in-naturals`
- `in-package-symbols`
- `in-parallel-iterators`
- `in-sequence`
- `in-vector`
- `make-iterator-optimizer-table`
- `map-iterator-optimizer-table`
- `next`
- `next*`
- `on-list`
- `remove-iterator-optimizer`
- `reporting-star-notes`
- `sequentially`
- `sequentially*`
- `sequentially-calling`
- `sequentially-calling*`
- `star-error`
- `star-note`
- `star-syntax-error`
- `star-syntax-error-form`
- `stepping`
- `stepping*`
- `stepping-values`

## An extension: loop unrolling
There is a somewhat experimental layer above Štar which allows loop unrolling.  This is done by a mechanism analogous to iterator optimizers: you can define an *unroller* for an iterator, which is a function which tells the system how to rewrite the iterator to use bigger steps, and what code to insert into the body of the loop.

As an example, if you defined a loop unroller for [`in-vector`](#vectors-and-general-sequences-in-vector-in-sequence "Vectors and general sequences: in-vector, in-sequence") and ask for the loop to be unrolled 4 times, then

```lisp
(for ((e (in-vector v)))
  (f e))
```

is turned into something like this

```lisp
(let ((<v> v)
      (<i> 0))
  (for ((e (in-vector <v> :by 4)))
    (f e)
    (setf e (aref <v> (incf <i>)))
    (f e)
    (setf e (aref <v> (incf <i>)))
    (f e)
    (setf e (aref <v> (incf <i>)))
    (f e)))
```

where the variables with names in \<angle brackets\> are gensyms of course.

Loop unrolling is *unsafe* in general you are responsible for ensuring that the length of `v` is, in fact, a multiple of 4 in the above code: it won't do that for you.

The idea of loop unrolling is to reduce the number of branches and tests in the code.  In practice it seems that modern processors are really good at branch prediction and also pretty good at making simple tests very quick (`(< a b)` where `a` and `b` are some good numeric type, say).  So loop unrolling, at least as implemented here, doesn't make things much better.

Loop unrolling is, however, an example of just how flexible Lisp is: implementing it entirely in user code is something that would be an interesting challenge in other languages: in Lisp it's 361 lines and took about 3 hours to write.  And Štar was intentionally designed to make this sort of thing easy: implementing loop unrolling for `loop` would not be easy.

Loop unrolling lives in a system & package called `org.tfeb.star/unroll`: this extends `org.tfeb.star`, so it includes everything that package exports, except that `for` is a different symbol.

### The unroller protocol
The unroller protocol is very like the [iterator optimizer protocol](#iterator-optimizers "Iterator optimizers") and consists of the following.

**`make-iterator-unroller-table`**  makes an iterator unroller table analogous to `make-iterator-optimizer-table`.

**`*iterator-unrollers*`** is the stack of unroller tables.  There's no builtin table, unlike with optimizers.

**`get-iterator-unroller`** gets an unroller from a table.  It is an accessor.

**`remove-iterator-unroller`** removes an unroller from a table.

**`find-iterator-unroller`** finds an unroller in the stack, and returns it and the stack from the table where it's found.

**`define-iterator-unroller`** defines iterator unrollers.  It's very like [`define-iterator-optimizer`](#defining-iterator-optimizers-define-iterator-optimizer "Defining iterator optimizers: define-iterator-optimizer"), including the specification of which table to define the unroller in, and the optional environment and stack arguments.  The mandatory arguments are

- `form`, the iterator form to be unrolled;
- `vars`, a list of variables on the left-hand-side of the iterator.  Any element of this list may be `nil` if the variable is anonymous.
- `unroll-by`, how many times to unroll by.

The unroller should return four values:

- whether the iterator is unrollable;
- a list of binding sets for secret variables, if any;
- the new iterator form;
- a list of lists of forms which will be inserted between the stages of the unrolled loop, which should have one less element than `unroll-by` argument.

### Unrolling itself
**`*enable-unrolling*`** should be true to enable unrolling by `for`.  `for/unroll` always unrolls.  This variable matters at macroexpansion time.

**`*unroll-by*`** is how many times to unroll.  The default is `4`, and again this matters at macroexpansion time.

**`for`** is Štar's `for`, but will unroll if possible when `*enable-unrolling*` is true.

**`for/unroll`** is `for`, but it always tries to unroll.

Finally **`rolled`** can be wrapped around any iterator to prevent it being unrolled.

### An example unroller
Here is a rudimentary unroller for [`in-vector`](#vectors-and-general-sequences-in-vector-in-sequence "Vectors and general sequences: in-vector, in-sequence").  This may not be completely correct.

```lisp
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
```

### Notes on unrolling
To unroll a loop all its iterators need to be unrollable.  There is no unrollable equivalent to `for*`.  There are no predefined iterator unrollers.  In practice unrolling does not significantly help performance.  Unrolling is not very heavily tested, and is unsafe by design.

---

## An approximate version history

| version | date              | commentary                                                                                                                                        |
| ------: | :---------------- | ------------------------------------------------------------------------------------------------------------------------------------------------- |
| 5       | 24th April 2025   | Iterator optimizers can provide type information for visible bindings                                                                             |
| 4       | 29th March 2025   | Experimental loop unrolling                                                                                                                       |
| 3       | 30th January 2025 | Abolish `in-range`: `in-naturals` does most of the useful things it did, `stepping` &c have optimizers and can be used for more complicated cases |
| 2       | 9th January 2025  | Much better declaration handling                                                                                                                  |
| 1       | 17th June 2024    | Initial public version                                                                                                                            |

## Notes
Štar itself is, I think, pretty stable.  The iterators are far less stable.

The public git repo for Štar only reflects major changes: the detailed history is not generally visible.

Štar is pronounced roughly 'shtar'.  The sources are UTF-8 encoded: you will need a unicode-competent system to load it.  It's time.

Much of the inspiration for Štar came from my friend Zyni: thanks to her for the ideas behind it, actually making me write it and for many other things.  Štar is dedicated to her, and to Ian Anderson.

---

[^1]:	See *[Something unclear in the Common Lisp standard](https://tfeb.org/fragments/2023/04/18/something-unclear-in-the-common-lisp-standard/ "Something unclear in the Common Lisp standard")* for a case where this same question applies to CL itself.

[^2]:	*Downward macro hygiene* problems happen when a macro binds variables which it should not.  This is avoidable by using gensyms for names.  *Upward macro hygiene* problems happen when a macro makes assumptions that a function or variable is what it thinks it is, and has not been locally rebound above it.  This is not avoidable in CL.  It is almost never a problem (and never a problem for symbols in the `CL` package).

	Upward macro hygiene problems almost never matter: downward ones often do.

[^3]:	I think that *not* doing so is probably closer to correct, but I don't think the standard says.