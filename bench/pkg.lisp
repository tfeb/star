;;;; Štar benchmark package
;;;

(in-package :cl-user)

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.star :compile t))

(defpackage :org.tfeb.star/bench
  (:use :cl)
  (:use
   :org.tfeb.hax.collecting
   :org.tfeb.hax.iterate)
  (:use :org.tfeb.star))
