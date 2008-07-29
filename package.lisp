(in-package #:array-operations-asd)

(defpackage :array-operations
  (:use :common-lisp :cl-utilities :bind :iterate)
  (:shadowing-import-from :iterate :collecting :collect)
  (:export 

   ;; ffa

   match-cffi-element-type make-ffa

   ;; displaced-utils

   displace-array flatten-array find-original-array
   find-or-displace-to-flat-array array-copy array-map array-map!
   array-convert

   ;; operations

   array-reduce array-max array-min array-sum array-product
   array-count array-range array-abs-range array-mean dot-product
   outer-product array-elementwise-operation array+ array- array*
   array/ array-scalar+ array-scalar- array-scalar* array-scalar/
   array-reciprocal array-negate array-map-list array-map-values
   index-extrema array-extrema vectorize
   
   ))
