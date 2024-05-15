;;;; Loader for Štar
;;;

(in-package :asdf-user)

(load (merge-pathnames (make-pathname :name "org.tfeb.star"
                                      :type "asd")
                       *load-truename*))

(load-system "org.tfeb.star")
