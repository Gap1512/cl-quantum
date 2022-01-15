;;;; cl-quantum.lisp
;;
;;;; Copyright (c) 2022 Gustavo Alves Pacheco


(in-package #:cl-quantum)

(defvar i #C(0 1))

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
  ((theta :std 0.0)
   (phi :std 0.0)
   (a :std 1.0)
   (b :std #C(0.0 0.0))))

(defmethod print-object ((qubit qubit) stream)
  (print-unreadable-object (qubit stream :type t :identity t)
    (format stream "[~{[~a]~^, ~}]" (wave-function qubit))))

(defmethod wave-function ((qubit qubit))
  (with-slots (a b) qubit
    (list a b)))

(defmethod phi ((qubit qubit))
  (acos (a qubit)))

(defmethod theta ((qubit qubit))
  (with-slots (b) qubit
    (/ (log (/ b (custom-sin (phi qubit)))) i)))

(defmethod set-bloch-sphere ((qubit qubit) theta phi)
  (let ((normalized-theta (if (<= .0 theta pi)
			      theta
			      (- pi (mod theta pi))))
	(normalized-phi (if (<= (- pi) phi pi)
			phi
			(- (mod phi (* 2 pi)) (* 2 pi)))))
    (setf (a qubit) (custom-cos normalized-phi)
	  (b qubit) (* (custom-sin normalized-phi) (exp (* normalized-theta i))))))

(defun quadratic-norm (number)
  (expt (abs number) 2))

(defmethod set-a-b ((qubit qubit) a b)
  (let* ((quadratic-norm-a (quadratic-norm a))
	 (quadratic-norm-b (quadratic-norm b))
	 (calculated-probability (+ quadratic-norm-a quadratic-norm-b))
	 (normalized-a (sqrt (/ quadratic-norm-a calculated-probability)))
	 (normalized-b (sqrt (/ quadratic-norm-b calculated-probability))))
    (setf (a qubit) (abs normalized-a)
	  (b qubit) (* (abs normalized-b)
		       (exp (* i (- (custom-angle normalized-b)
				    (custom-angle normalized-a))))))))

(defmethod measure ((qubit qubit))
  (with-slots (a b) qubit
    (let ((probability (random 1.0))
	  (probability-of-a (quadratic-norm a)))
      (if (<= probability probability-of-a)
	  '0
	  '1))))

(defun test ()
  (let ((qubit (make-instance 'qubit)))
    (set-a-b qubit (/ (sqrt 2) 2) (/ (sqrt 2) 2))
    (measure qubit)))
