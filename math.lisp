;;;; math.lisp
;;
;;;; Copyright (c) 2022 Gustavo Alves Pacheco


(in-package #:cl-quantum)

(alexandria:define-constant +i+ #C(0 1))

(defun custom-sin (angle)
  (if (zerop (mod angle pi))
      0
      (sin pi)))

(defun custom-cos (angle)
  (if (zerop (mod (+ angle (/ pi 2)) pi))
      0
      (cos pi)))

(defun custom-angle (complex-number)
  (atan (imagpart complex-number) (realpart complex-number)))

(defun dot (a b)
  (mapcar #'(lambda (c)
	      (reduce #'+ (mapcar #'* a c)))
	  b))

(defun scalar (scalar a)
  (labels ((recursive-scalar (list-or-element result)
	     (if list-or-element
		 (let ((first (car list-or-element)))
		   (recursive-scalar (cdr list-or-element)				     
				     (cons (if (consp first)
					       (recursive-scalar first nil)
					       (* scalar first))
					   result)))
		 (nreverse result))))
    (recursive-scalar a nil)))

(defun quadratic-norm (number)
  (expt (abs number) 2))

(defun practical-zerop (number)
  (<= -1d-14 number 1d-14))
