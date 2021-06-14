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


(defun add-shape (canvas tag bit-array x y fillet-radius)
  (let* ((num-sides (length bit-array))
		 (TAU (* 2.0 PI))
		 (inner-angle (/ TAU num-sides))
		 (hypot-radius-angle (/ (- PI inner-angle) 2.0))
		 (distance-to-circle-center (/ fillet-radius (cos hypot-radius-angle))))
	(dotimes (i num-sides)
	  (let ((pt-x (+ x (* (cos (* i inner-angle)) distance-to-circle-center)))
			(pt-y (+ y (* (sin (* i inner-angle)) distance-to-circle-center)))
			(tex-pt-x (+ x (* (cos (* i inner-angle)) (* distance-to-circle-center 1.45))))
			(tex-pt-y (+ y (* (sin (* i inner-angle)) (* distance-to-circle-center 1.45))))
			(small-fill (if (= (aref bit-array i) 1) "cyan" ""))
			(big-fill (if (= (aref bit-array i) 1) "dark cyan" "")))
		(nodgui:create-text canvas tex-pt-x tex-pt-y i :anchor :center :justify :center)
		(nodgui:make-circle canvas
							pt-x
							pt-y
							fillet-radius
							:fill big-fill
							:outline "black")
		(nodgui:make-circle canvas
							pt-x
							pt-y
							(* fillet-radius .5)
							:fill small-fill
							:outline "black")))))

(defun main ()
  (nodgui:with-nodgui (:title "My progrum" :width 640 :height 480 )
	(let* ((canvas (make-instance 'nodgui:canvas))
		   (exit (make-instance 'nodgui:button
								:text "Exit"
								:command #'nodgui:exit-wish)))
	  (add-shape canvas "shape" #*101101000100  120 128 16)
	  
	  (nodgui:pack canvas :padx 0 :pady 0 :expand t :fill "BOTH")
	  (nodgui:pack exit  :padx 0 :pady 0))))
		  
