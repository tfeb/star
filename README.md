# Štar: a simple and extensible iteration construct
Štar is a concise and extensible iteration construct for Common Lisp which aims to be pleasant to use, easy to understand, general, and not to look like Fortran.

For an overview of Štar, see [here](https://www.tfeb.org/fragments/2024/05/15/an-iteration-construct-for-common-lisp/ "Štar: an iteration construct for Common Lisp"): this document is just the reference manual.

At the time of writing, Štar itself is fairly complete.  The predefined iterators are less finished.  Until this paragraph goes away you should use them with some caution: things may change in incompatible ways.  I have tried to indicate the more flaky iterators below.

## Overview
Štar is an iteration construct.  That's all it is: it does not accumulate values, destructure bindings or do your washing.  It doesn't do all those other things because there are better tools which do them already which can be used freely with Štar.  Similarly there are no special magic conditional mechanisms: rather there are local functions which can skip to the next iteration or terminate the iteration, and you use these with normal conditional expressions.

## Iterators
Štar has no built-in knowledge of how to iterate over lists, or over ranges of reals or whatever.  Rather it treats iteration by asking two questions:

- is there more?
- if there is, what it it?

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
## Parallel iteration: `for`
`for` iterates over zero or more iterators.  The syntax is

`(for` `(`*clause*\*`)` *declaration*\* *form*\*`)`

Where each *clause* is either

`(`*var* *iterator*`)`

or

`((`*var*\*`)` *iterator*`)`

and finally a *var* is either a symbol naming a variable, or an expression like

`(`*name* `&key type special ignore ignorable anonymous)`

which allows various declarations to be made about variables.

Variables whose name is `"_"` are anonymous by default: each such variable is unique and is ignored.  You could make such a variable not be anonymous by specifying it as `(_ :anonymous nil)`.

The other options for the complicated variable case exist mostly for the use of `for*`: for `for` declarations at the start of the body do what you expect.

Finally an *iterator* is just a form which evaluates to two functions as specified above.

Within the lexical scope and dynamic extent of the body four local functions are defined:

- **`next`** skips immediatly to the next iteration;
- **`next*`** skips to the next *outer* iteration: this is the same as `next` for `for`.
- **`final`** exits the iteration, returning its arguments as multiple values;
- **`final*`** exits the *outer* iteration and is the same as `final` for `for`.

The body is surrounded by blocks named `for` and `nil`.

Variables are bound in parallel: it is in the nature of the construct that sequential bindings make no sense, since iterators are evaluated before any variables are bound.

Variables are bound for each step (it is possible this might change: don't rely on it).

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

This is a case where the fancy form of variables is useful: it's not possible, in general, to work out which, if any, variables a declaration refers to in CL[^1], so it's not possible to 'raise' declarations so the apply to the right binding.  Rather than doing so but getting it wrong sometimes, Štar doesn't try but allows you to add some declarations with the fancy variable form.  So if you want to declare that `c` is a character, you need to do it like this

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

## Iterator optimizers
The protocol described here is exported by `org.tfeb.star/iop` and `org.tfeb.star`.

Iterator optimizers are functions which tell Štar how to essentially inline iterators.   They are attached to iterators which are named functions.  They are looked up at macroexpansion time in a stack of tables: this is usually just two, with a user table and a builtin table underneath it which is protected, but you can push and pop other entries, which might be useful, for instance, when compiling code where you can make specific assumptions about types which are not generally true.

Because iterator optimizers work on names, they suffer from the unavoidable 'upward macro hygiene' problem that most CL macros suffer from[^2].  Here is an example of an upward macro hygiene problem in štar:

```lisp
(flet ((in-list (...) ...))
  (for ((e (in-list ...)))
    ...))
```

Here the iterator optimzer for `in-list` will assume it is optimizing the global function, not the local one.  There is no general solution to this problem in CL.  But, equally, it  is very seldom a real problem.

An iterator optimizer is usually defined with `define-iterator-optimizer` but it is possible to add entries to the tables by hand.

An iterator optimizer function takes two arguments: the form to be optimized and an environment object (normally this is suppressed when using `define-iterator-optimizer` ).  It should return either `nil` if it declines to optimize the form, or four or five values:

- true to say it can optimize the form;
- a specification for variable bindings;
- a form which says if the iterator is valid;
- a form which produces the values for the next step;
- optionally a function which is called with the whole form and can wrap code around it.

### Specifications of variable bindings
This is a list of what are essentially the first two arguments to `multiple-value-bind`, together with zero or more declarations.  This list can have multiple elements which correspond to nested bindings and in which, therefore, later bindings can refer to earlier ones.

### Valid form
This is just a form which should evaluate to true if the iterator is valid;

### Cursor form
This is a form which should evaluate to the values for the next step.

### Wrapping function
This is either `nil` or a function of one argument, which is called at macroexpansion time with the whole body of the iteration form.  It is useful to wrap code around the whole form.

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

**`get-iterator-optimizer`** finds an iterator optimizer in a table.  It takes two arguments: the optimizer name and the table.  It is an accessor, so you can install new optimizers with it if you wish.

**`remove-iterator-optimizer`** removes an optimizer from a table.  It takes two arguments: the optimizer name and the table.

**`map-iterator-optimizer-table`** maps a function over an optimizer table.  It works like `maphash` (in fact it *is* `maphash` currently) and has the same restrictions.

Finally **`find-iterator-optimizer`**  finds an optimizer in the stack of tables.  It takes two arguments: the optimizer name and the stack of tables, defaultly the value of `*iterator-optimizers*`.  It returns tewo values: the optimizer and the point in the stack where it found it, or `nil` and `nil`.

### Defining iterator optimizers: `define-iterator-optimizer`
This is the normal way to define iterator optimizers.  The first argument is either the name of an optimizer or a list of a name and a table to define it in.  The second argument is an arglist with one or two elements: the first is the name for the form, the second, if given, for the environment.  The rest of the form is a function body.

There is no completely satisfactor answer as to when iterator optimizers should be defined.  Originally I decided that  `define-iterator-optimizer` should *not* wrap its expansion in an `eval-when`, because iterator optimizers should not be defined before their iterators are, ie not before load time.  However this means that a file which says

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

## Some useful things
These sre exported by `org.tfeb.star/utilities` and `org.tfeb.star`.

### Errors
Any error that Štar thinks is your fault will be a condition inheriting from the `star-error` condition type.  Errors that it thinks are bugs will *not* inherit from this type.

### Compilation notes
Štar, and particularly iterator optimizers, can report various notes which might be helpful.  These are done by signalling `star-note`, which is a condition type which is a subtype of `simple-condition`.

**`reporting-star-notes`** is a macro which will report star notes to a specified stream, by default `*debug-io*`.  You want to wrap this around a compilation which is when notes are reported:

```lisp
(reporting-star-notes (my-recording-stream)
  (compile "my-big-file"))
```

for instance.

## Packages
The system is structured so that you can pick and choose which bits you want, starting from something extremely minimal.

- **`org.tfeb.*`** is Štar itself: just `for`, `for*`, `next`, `next*`, `final` and `final*`.
- **`org.tfeb.star/iterator-optimizer-protocol`** *aka* `org.tfeb.star/iop` is the iterator optimizer protocol.
- **`org.tfeb.star/iterators`** is the predefined iterators.
- **`org.tfeb.star/utilities`** is the names of conditions and `reporting-star-notes`.
- **`org.tfeb.star`** combines the above four packages.

---

## Predefined iterators
All of these are exported by `org.tfeb.star/iterators` and `org.tfeb.star`.

These are in a *much* more rudimentary state than Štar itself: many were written before it was finished, as proofs of concept, or both.  The list below may change significantly.

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
`stepping` and `stepping*` are a pair of iterators which make and step bindings.  They take a number of clauses which look like `(var &key initially then type value while until)`.

- `var` is a variable name which will be bound.
- `initially` is the initial value of the variable, default `nil`.
- `then`, if given, is an expression whose value will be the new value of the variable on each step.
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

`stepping-values` is conceptually similar to `stepping` but it steps a single set of multiple values.  Its syntax is `(stepping-values vars &key initially then types values while until)`

- `vars` is a list of variables to step.
- `initially` is a form returning multiple values for them: the default is to bind them all to`nil`.
- `then` is a form returning new values, the default being not to assign new values.
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

These three iterators do not have optimizers: without writing a code walker it's hard to see how to write one.

### Ranges of reals: `in-range`
This iterates over ranges of reals.  It *may or may not* be correct and probably is not final at present.  It has a simple form and a hairy form.

**The simple form** `(in-range r)` where `r` is a real iterates from zero up to or down to `r`, if `r` is negative; `(in-range)` iterates over the natural numbers (where the natural numbers include zero).

**The hairy form** is `(in-range &key from after to before by type)`.

Exactly one of `from` and `after` must be given:

- if `from` is given this is where the iteration starts;
- if `after` is given it starts at `(+ from by)`.

At most one of `to` and `before` must be given:

- if `to` is given the iteration may reach this value but will not exceed it;
- if `before` is given the iteration stops before this value;
- if neither is given the iteration is unbounded.

`by` is the step.  It is defaulted from the start (`from` or `after`) and end (`to` or `before`) limits: if the end is above or equal to the start it defaults to 1, and if it is below it it defaults to -1.  If the iteration is unbounded it defaults to 1.  The defaults are all coerced to the appropriate type.

`type` is the type.

There is fairly complicated behaviour with types, with the aim being to be both consistent but to allow the optimizer to produce fast code.

**Without any user-specified types** things are fairly simple.

- If all the numeric arguments are rationals (or the limit is `*`) then nothing needs done.
- If any of the values are floats, then the widest float type is found, and everything is coerced to that.

The second rule means that the first return from `(range :from 0 :before 10.0)` is `1.0` not `1`.  This means that the type of every returned value is consistent: it doesn't switch from some rational type to some float type, and all the float values will be of the same type

**With user-specified types** things are less clear.

- If the user-specified type is a subtype of `rational` then all of the initial values should be subtypes of this type.
- If the user-specified type is a subtype of `float`, then at least one of the initial values should be a float.  All the values are coerced to the type of the widest float, and everything should then be of the specified type.

**The optimizer.**  The optimizer for `in-range` is pretty hairy: it's almost half of all the iterator code.  It's also just gory, and may well be buggy.  The times when it should be able to produce good code are:

- when all the numerical values are literal numbers;
- when a literal type is given which is friendly to the implementation.

### Meta iterators
These are iterators which iterate over things like other iterators, or functions.

**`sequentially-calling`** iterates over the functions which are its arguments, calling each in turn with no arguments.

**`sequentially`** is a macro version of `sequentially-calling`: it iterates over forms, evaluating each, once, in turn.

**`sequentially-calling*`** and **`sequentially*`** are like their unstarred versions but they 'stick' on the last argument.

**`cyclically-calling`** calls the functions which are its arguments cyclically.  **`cyclically`** is the macro equivalent.

**`in-iterators`** takes arguments which are iterators: it iterates the first until it is exhausted and so on.

**`in-parallel-iterators`** takes arguments which are iterators.  At each step it returns one value from each until one is exhausted.

**`always`** endlessly returns its argument's value.  This may go away.

---

## Notes
Štar itself is, I think, pretty stable.  The iterators are far less stable.  `in-range`, in particular, is always going to exist but might change in significant ways.

There are currently no tests: there really need to be.  Before any real release, there will be tests.

The public git repo for Štar only reflects major changes: the detailed history is not generally visible.

Štar is pronounced roughly 'shtar'.  The sources are UTF-8 encoded: you will need a unicode-competent system to load it.  It's time.

Much of the inspiration for Štar came from my friend Zyni: thanks to her for the inspiration behind it, actually making me write it and for many other things.

Štar is dedicated to her, and to Ian Anderson.

---


[^1]:	This is one of the things that should be repaired in an imaginary future CL standard.

[^2]:	*Downward macro hygiene* problems happen when a macro binds variables which it should not.  This is avoidable by using gensyms for names.  *Upward macro hygiene* problems happen when a macro makes assumptions that a function or variable is what it thinks it is, and has not been locally rebound above it.  This is not avoidable in CL.  It is almost never a problem (and never a problem for symbols in the `CL` package).

	Upward macro hygiene problems almost never matter: downward ones often do.

[^3]:	I think that *not* doing so is probably closer to correct, but I don't think the standard says.