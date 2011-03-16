
(defpackage :literate-lisp
    (:nicknames :lit-lisp)
  (:use :cl :iter :cl-ppcre :toolbox :portable-readtables)
  (:export #:weave #:tangle #:output-documentation) )
