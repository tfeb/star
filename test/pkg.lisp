;;;; Štar test package
;;;

(in-package :cl-user)

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 ((:org.tfeb.hax.collecting :org.tfeb.hax.iterate :org.tfeb.hax.utilities
   :org.tfeb.dsm)
  :compile t)
 (:org.tfeb.star :compile t)
 ("parachute"))

(defpackage :org.tfeb.star/test
  (:use :cl)
  (:use
   :org.tfeb.hax.collecting
   :org.tfeb.hax.iterate
   :org.tfeb.hax.utilities
   :org.tfeb.dsm :org.tfeb.dsm/extensions)
  (:use :org.tfeb.star
   :org.shirakumo.parachute))
