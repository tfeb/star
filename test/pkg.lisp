;;;; Štar test package
;;;

(in-package :cl-user)

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.star :compile t)
 ("parachute"))

(defpackage :org.tfeb.star/test
  (:use :cl)
  (:use :org.tfeb.star
   :org.shirakumo.parachute))
