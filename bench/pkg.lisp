;;;; Štar benchmark package
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 :org.tfeb.star
 :org.tfeb.hax.collecting
 :org.tfeb.hax.iterate)

(in-package :cl-user)

(defpackage :org.tfeb.star/bench
  (:use :cl)
  (:use
   :org.tfeb.hax.collecting
   :org.tfeb.hax.iterate)
  (:use :org.tfeb.star)
  (:export
   #:run-benchmarks))
