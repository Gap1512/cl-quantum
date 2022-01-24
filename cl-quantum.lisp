;;;; cl-quantum.lisp
;;
;;;; Copyright (c) 2022 Gustavo Alves Pacheco


(in-package #:cl-quantum)

(alexandria:define-constant +i+ #C(0 1))

(alexandria:define-constant +ket-0+ :0)

(alexandria:define-constant +ket-1+ :1)

(alexandria:define-constant +pauli-x+ (list (list 0 1) (list 1 0)) :test #'equal)

(alexandria:define-constant +pauli-y+ (list (list 0 (- +i+)) (list +i+ 0)) :test #'equal)

(alexandria:define-constant +pauli-z+ (list (list 1 0) (list 0 -1)) :test #'equal)

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

(defclass/std qubit ()
  ((a :std 1.0)
   (b :std #C(0.0 0.0))))

(defmethod print-object ((qubit qubit) stream)
  (print-unreadable-object (qubit stream :type t :identity t)
    (format stream "[~{[~a]~^, ~}]" (wave-function qubit))))

(defmethod wave-function ((qubit qubit))
  (with-slots (a b) qubit
    (list a b)))

(defmethod theta ((qubit qubit))
  (* 2 (acos (a qubit))))

(defmethod phi ((qubit qubit))
  (with-slots (b) qubit
    (atan (imagpart b) (realpart b))))

(defmethod cartesian ((qubit qubit))
  (let ((theta (theta qubit))
	(phi (phi qubit)))
    (values (* (custom-sin theta) (custom-cos phi))
	    (* (custom-sin theta) (custom-sin phi))
	    (custom-cos theta))))

(defmethod set-bloch-sphere ((qubit qubit) theta phi)
  (let ((normalized-theta (if (<= .0 theta pi)
			      theta
			      (- pi (mod theta pi))))
	(normalized-phi (if (<= (- pi) phi pi)
			phi
			(- (mod phi (* 2 pi)) (* 2 pi)))))
    (setf (a qubit) (custom-cos (/ normalized-theta 2))
	  (b qubit) (* (custom-sin (/ normalized-theta 2))
		       (exp (* +i+ normalized-phi))))))

(defun quadratic-norm (number)
  (expt (abs number) 2))

(defmethod set-probability-amplitude ((qubit qubit) a b)
  (let* ((quadratic-norm-a (quadratic-norm a))
	 (quadratic-norm-b (quadratic-norm b))
	 (probability (+ quadratic-norm-a quadratic-norm-b)))
    (setf (a qubit) (sqrt (/ quadratic-norm-a probability))
	  (b qubit) (* (sqrt (/ quadratic-norm-b probability))
		       (/ b (abs b))
		       (/ (abs a) a)))))

(defmethod set-wave-function ((qubit qubit) wave-function)
  (set-probability-amplitude qubit
			     (first wave-function)
			     (second wave-function)))

(defmethod measure ((qubit qubit))
  (if (<= (random 1.0) (quadratic-norm (a qubit)))
      +ket-0+
      +ket-1+))

(defmethod simulate ((qubit qubit) &optional (times 1000))
  (let ((measures (loop repeat times collecting (measure qubit))))
    (format t "0: ~a%~%1: ~a%~%"
	    (* 100 (float (/ (count +ket-0+ measures) times)))
	    (* 100 (float (/ (count +ket-1+ measures) times))))))

(defmethod dot (a b)
  (mapcar #'(lambda (c)
	      (reduce #'+ (mapcar #'* a c)))
	  b))
