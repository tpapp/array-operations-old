(in-package :array-operations)

(defun find-original-array (array)
  "Find the original parent of a displaced array, return this and the
sum of displaced index offsets."
  (let ((sum-of-offsets 0))
    (tagbody
     check-displacement
       (multiple-value-bind (displaced-to displaced-index-offset)
	   (array-displacement array)
	 (when displaced-to
	   (setf array displaced-to)
	   (incf sum-of-offsets displaced-index-offset)
	   (go check-displacement))))
    (values array sum-of-offsets)))

(defun displace-array (array dimensions index-offset)
  "Make a displaced array from array with the given dimensions and the
index-offset and the same element-type as array.  Tries to displace
from the original array."
  (multiple-value-bind (original-array sum-of-offsets)
      (find-original-array array)
    (make-array dimensions 
		:element-type (array-element-type array)
		:displaced-to original-array
		:displaced-index-offset (+ sum-of-offsets index-offset))))

(defun flatten-array (array)
  "Return a flat (ie rank 1) displaced version of the array."
  (displace-array array (array-total-size array) 0))
  
;; DEPRECATED
;; (defun find-or-displace-to-flat-array (array)
;;   "Find a flat array that array is displaced to, or create one that is
;; displaced to the original array.  Also return the index-offset and
;; length (total size).  Useful for passing to reduce etc."
;;   (bind ((total-size (array-total-size array))
;; 	 ((:values original-array index-offset) (find-original-array array)))
;;     (if (= (array-rank original-array) 1)
;; 	(values original-array index-offset total-size)
;; 	(values (displace-array original-array total-size index-offset)
;; 		0 total-size))))

  
(defparameter *a* #2A((1 2) (3 4)))

(defun array-map (function array 
		  &optional (element-type (array-element-type array)))
  "Map an array into another one elementwise using function.  The
resulting array has the given element-type."
  (bind ((result (make-array (array-dimensions array) :element-type element-type))
	 ((:values original index-offset) (find-original-array array)))
    (iter
      (for result-index :from 0 :below (array-total-size array))
      (for original-index :from index-offset)
      (setf (row-major-aref result result-index)
	    (funcall function (row-major-aref original original-index))))
    result))

(defun array-copy (array)
  "Copy the elements of array.  Does not copy the elements themselves
recursively, if you need that, use array-map."
  (array-map #'identity array))

(defun array-map! (function array)
  "Replace each element 'elt' of an array with (funcall function elt),
and return the modified array."
  (dotimes (i (array-total-size array))
    (setf (row-major-aref array i) (funcall function (row-major-aref array i)))))

(defun array-convert (element-type array)
  "Convert array to desired element type.  Always makes a copy, even
if no conversion is required."
  (let ((element-type (upgraded-array-element-type element-type)))
    (if (equal (array-element-type array) element-type)
	(array-copy array)
	(array-map #'(lambda (x) (coerce x element-type)) array element-type))))
