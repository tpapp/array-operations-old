(in-package :array-operations)

;; !!!! this mapping works with 32-bit SBCL, need to check for other
;; !!!! implementations
(defparameter *cffi-and-lisp-types*
  '((:int8 . (signed-byte 8))
    (:uint8 . (unsigned-byte 8))
    (:int16 . (signed-byte 16))
    (:uint16 . (unsigned-byte 16))
    (:int32 . (signed-byte 32))
    (:uint32 . (unsigned-byte 32))
    (:float . single-float)
    (:double . double-float)))

(defun match-cffi-element-type (cffi-element-type)
  "Return the Lisp array element-type matching cffi-element-type, nil
of not found."
  (cdr (assoc cffi-element-type *cffi-and-lisp-types*)))

(defun make-ffa (dimensions element-type &key
		 (initial-element 0 initial-element-p)
		 (initial-contents nil initial-contents-p))
  "Make an array that is either one-dimensional or displaced to a
one-dimensional array.  element-type can be a cffi type, see
*cffi-and-lisp-types*.  Array is filled with initial-element or
initial-contents, coerced to the given type."
  (assert (or (atom dimensions) (and (listp dimensions) (car dimensions))))
  (let* ((element-type (or (match-cffi-element-type element-type) element-type))
	 (dimensions (if (atom dimensions) (list dimensions) dimensions))
	 (length (reduce #'* dimensions))
	 (array (cond
		  ((and initial-element-p initial-contents-p)
		   (error "you can't supply both initial-element and ~
                           initial-contents"))
		  ;; initial element given
		  (initial-element-p
		   (make-array length :element-type element-type
			       :initial-element (coerce initial-element
							element-type)))
		  ;; contents given, copy or coerce
		  (initial-contents-p
		   (assert (= (length initial-contents) length))
		   (if (typep initial-contents (list 'vector element-type))
		       (copy-seq initial-contents)
		       (map (list 'vector element-type)
			    (lambda (x) (coerce x element-type)) initial-contents)))
		  ;; neither
		  (t (make-array length :element-type element-type)))))
    (if (cdr dimensions)
	(make-array dimensions :element-type element-type 
		    :displaced-to array)
	array)))

