#!/usr/bin/sbcl --script

(defun bubble (arr arrX n)

       (do ((i 0 (+ i 1))) ((= i (- n 1)))

         (do ((j 0 (+ j 1))) ((= j (- (- n i) 1)))
              
             (if (> (aref arr j) (aref arr (+ j 1) ) )
		(progn
                 (swap j (+ j 1) arr)
		 (swap j (+ j 1) arrX)
		)
             )
                           
         )
       ) 
)

(defun swap(x y arr)

       (let ((temp 0))
       (setf temp (aref arr x)) 

       (setf (aref arr x) (aref arr y)) 

       (setf (aref arr y) temp)
       ) 
)

(defun recursiveCollatz(collatzNum)
	(let ((recSteps 0))
	  (cond ((eq collatzNum 1) (return-from recursiveCollatz 0))
	  ((eq (mod collatzNum 2) 1) (setf recSteps (recursiveCollatz(+ (* collatzNum 3) 1))))
	  (t (setf recSteps (recursiveCollatz(/ collatzNum 2)))))
	(setf recSteps (+ 1 recSteps))
	(return-from recursiveCollatz recSteps))
	)




(defvar num)
(defvar collatzNum)
(defvar steps)
(defvar minNum)
(defvar minSteps)
(defvar stepArray)
(defvar magnitudeArray)
(defvar j)
(defvar i)
(defvar k)
(defvar newIndex)
(defvar isSame)

(setf stepArray (make-array'(10)))
(setf magnitudeArray (make-array'(10)))

(setf num 5000000000)

(loop while(/= num 0) do 

	(setf collatzNum num)
	(setf isSame 0)
	(setf newIndex 0)
	(setf steps (recursiveCollatz collatzNum))
	(setf minNum 0)
	(setf minSteps (aref stepArray 0))

	(loop for i from 0 to 9 do
		(if (> minSteps (aref stepArray i))
			(progn
				(setf minSteps (aref stepArray i))
				(setf minNum i)
			))
		(if (eq (aref stepArray i) steps)
			(progn
				(setf isSame 1)
				(setf newIndex i)
			)))
	(setf j minNum)
	(if (and (> steps (aref stepArray j)) (eq isSame 0))
		(progn
			(setf (aref magnitudeArray j) num)
			(setf (aref stepArray j) steps)
		))
	(if (and (< num (aref stepArray newIndex)) (eq isSame 1))
		(progn
			(setf (aref magnitudeArray newIndex) num)
		))
	(setf num (- num 1)))
	
	(format t "Sequence Length Array (Magnitude -> Steps)")
	(terpri)
	(bubble stepArray magnitudeArray 10)
	(loop for i from 0 to 9 do
		(format t "~d"(aref magnitudeArray i)) 
		(format t " ")
		(format t "~d"(aref stepArray i))
		(terpri))

	(format t "Magnitude Array (Magnitude -> Steps)")
	(terpri)
        (bubble magnitudeArray stepArray 10)
        (loop for i from 0 to 9 do
                (format t "~d" (aref magnitudeArray i))
                (format t " ")
                (format t "~d" (aref stepArray i))
		(terpri))
	(terpri)
