;;;; segments.lisp

(in-package #:segments)

;; TODO define parity bit for shape-equal

(defun shape? (s max-points list-length-max)
  (and (<= 2 (count 1 s) max-points)
       (= (elt s 0) 1)
       (<= 2 max-points (length s) list-length-max)))

(defun list-of-bits (integer)
  (let ((bits '()))
    (dotimes (position (integer-length integer) bits)
      (push (ldb (byte 1 position) integer) bits))))

(defun pad-bit-array (int-or-list num-bits)
  (if (integerp int-or-list)
      (setf int-or-list (list-of-bits int-or-list)))
  (let ((result (make-array num-bits :element-type 'bit :initial-element 0)))
    (loop with counter = 0
          for i in int-or-list do
            (setf (sbit result counter) i)
            (incf counter))
    result))

(defun gen-pieces (max-points vertlist-length)
  (let ((results '())
        (working-item nil))
    (loop :for i :from 2 :to (1- (expt 2 vertlist-length))
          do
             (setf working-item (pad-bit-array i vertlist-length))
             (when (shape? working-item max-points vertlist-length)
               (setf results (adjoin working-item results :test #'equal))))
    (sort results #'equal)))


(defun add-shape (canvas tag x y num-sides fillet-radius)
  (let* ((TAU (* 2.0 PI))
		 (inner-angle (/ TAU num-sides))
		 (hypot-radius-angle (/ (- PI inner-angle) 2.0))
		 (distance-to-circle-center (/ fillet-radius (cos hypot-radius-angle))))
  distance-to-circle-center))

		

(defun main ()
  (with-ltk ()
	(let ((canvas (make-instance 'canvas))
		  (button (make-instance 'button
								 :text "Hello"
								 :command (lambda ()
											(format t "clicked")))))
	  (grid date-picker 0 0)
	  (grid canvas 0 1)
	  (grid button 0 2))))


