;;;; Test preamble
;;;

(in-package :org.tfeb.star/test)

(unless *test-individually*
  (test "org.tfeb.star" :report *test-report-class*))
