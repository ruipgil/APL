
;;; Tensor type definition
;;; (make-tensor '( (((1 2)(3 4)) ((5 1)(2 3))) (((1 2)(3 4)) ((5 1)(2 3)))   ))


(defclass tensor ()
  ((lista :type list 
     :initarg :lista
     :reader tensor-read
     :writer tensor-set 
     :documentation "Defines the tensor type")))

(defmethod tensor-read ((obj tensor))
  "Method to acess tensor value. Receives tensor as argument"
  (slot-value obj 'lista))
    
(defmethod tensor-set ((obj tensor) new-value)
  "Method to change tensor value. Receives tensor and new value as arguments"
  (setf (slot-value obj 'lista) new-value))

(defun make-tensor (lista)
  "Method to create a tensor. Receives array as arguments"
  (make-instance 'tensor :lista lista))

(defmethod print-object-aux1 ((lista list) stream)
  "Auxiliar function to print-object. Parses tensor type to print"
  (let ((size (- (length lista) 1)))
    (cond ((null lista) nil)
      ((atom (first lista)) (print-object-aux2 lista stream))
      ((atom (first (first lista))) (progn (print-object-aux1 (first lista) stream)
           (if (not (= size 0)) 
               (progn (format stream "~%")
                 (- size 1)))
                                        (print-object-aux1 (rest lista) stream)))
      (t (progn 
           (progn (print-object-aux1 (first lista) stream)
           (if (not (= size 0))
	    (loop for i from 0 to (count-dimensions  lista) do
               (progn (format stream "~%")
                 (- size 1))))
             (print-object-aux1 (rest lista) stream)))))))


(defun count-dimensions (lista)
  "Counts the dimensions present in the given list"
    (let ((counter 0)
	  (temp lista))
      (loop while (not (null temp)) do
	  (if (not (atom (first temp)))
	      (progn
		  (incf counter)
		  (setf temp (first temp)))
	      (return counter)))))
           
(defmethod print-object-aux2 ((lista list) stream)
  "Prints a list with atom's"
  (loop for val in lista do
        (format stream "~A " val)))

(defmethod print-object ((obj tensor) stream)
  "Overrides the print-objct function for the tensor type."
  (let ((lista (tensor-read obj)))
    (print-object-aux1 lista stream)))

;;; Scalars and Vectors

(defun s (arg)
  "Creates a scalar tensor."
  (make-tensor (list arg)))

(defun v (&rest args)
  "Creates a vector tensor."
  (make-tensor args))

;;; Monadic Functions 
  
(defmethod monadicmap ((obj tensor) op)
  "Maps a tensor to a single argument operation"
  (let ((tensor (tensor-read obj)))
         (cond ((null tensor) nil)
           ((atom (first tensor)) (make-tensor (map 'list op tensor))))))

;;; .- function defined in dyadic functions

;;; ./ defined in dyadic functions

(defmethod .! ((obj tensor))
  "Factorial function"
  (monadicmap obj #'(lambda (num) (reduce #'* (loop for i from 1 to num collect i)))))

(defmethod .sin ((obj tensor))
  "Sin function"
  (monadicmap obj #'sin))

(defmethod .cos ((obj tensor))
  "Cos function"
  (monadicmap obj #'cos))

(defmethod .not ((obj tensor))
  "Negation function"
  (monadicmap obj #'(lambda (num) (if (= num 0) 1 0))))

;;; Confirmar 
(defmethod shape ((obj tensor))
  "Returns a vector with size of the tensor"
  (let* ((temp '())
         (lista (tensor-read obj))
         (dim (count-dimensions lista)))
    (loop for i from 0 to dim do 
          (progn (setf temp (append temp (list (length lista))))
            (if (not(atom(first lista)))
                (setf lista (first lista)))))
  (make-tensor temp)))
                

(defun number-sequence (min max &optional (step 1))
  "Returns a list within the min and max interval"
  (when (<= min max)
    (cons min (number-sequence (+ min step) max step))))

;;; ALLEGRO PROBLEM, IN CLISP interval is accepted
(defun interval1 (num)
  "Interval of numbers between 0 and the given number"
  (v (number-sequence 0 num)))


;;; DYADIC FUNCTIONS

  
(defmethod dyadicmap ((obj1 tensor)(obj2 tensor) op)
  "Maps two tensors to a double argument operation"
  (let ((tensor1 (tensor-read obj1))
        (tensor2 (tensor-read obj2)))
    (cond ((= (length tensor1) 1) (make-tensor (dyadicmap-aux2 (first tensor1) tensor2 op)))
      ((= (length tensor2) 1) (make-tensor (dyadicmap-aux2 (first tensor2) tensor1 op)))
      ((= (length tensor1) (length tensor2)) (make-tensor (dyadicmap-aux1 tensor1 tensor2 op)))
      (t "Error: Tensor's size must be equal or one of them scalar"))))

(defmethod dyadicmap-aux1 (lista1 lista2 op)
  "Applies a operation between the elements of 2 lists. Returns the corresponding list."
  (let ((return-value '()))
    (loop for i from 0 to (- (length lista1) 1)
        do (setf return-value (cons (funcall op (nth i lista1) (nth i lista2)) return-value)))
    (reverse return-value)))

(defmethod dyadicmap-aux2 (num lista op)
  "Applies a operation to a list with num value. Returns corresponding list"
  (let ((return-value '()))
    (loop for i from 0 to (- (length lista) 1)
        do (setf return-value (cons (funcall op (nth i lista) num) return-value)))
    (reverse return-value)))
  
 
(defmethod .+ ((obj1 tensor) (obj2 tensor))
  (dyadicmap obj1 obj2 #'+ ))


;;;Monadic & Dyadic
(defmethod .- ((obj1 tensor) &optional obj2)
  (if (null obj2)
      (monadicmap obj1 #'-)
      (dyadicmap obj1 obj2 #'- )))

(defmethod .* ((obj1 tensor) (obj2 tensor))
  (dyadicmap obj1 obj2 #'* ))

;;;Monadic & Dyadic
(defmethod ./ ((obj1 tensor) &optional obj2)
    (if (null obj2)
      (monadicmap obj1 #'/)
      (dyadicmap obj1 obj2 #'/ )))

(defmethod .// ((obj1 tensor) (obj2 tensor))
  (dyadicmap obj1 obj2 #'floor ))

(defmethod .% ((obj1 tensor) (obj2 tensor))
  (dyadicmap obj1 obj2 #'mod ))
 
(defmethod .< ((obj1 tensor) (obj2 tensor))
  (dyadicmap obj1 obj2 #'< ))

(defmethod .> ((obj1 tensor) (obj2 tensor))
  (dyadicmap obj1 obj2 #'> ))

(defmethod .<= ((obj1 tensor) (obj2 tensor))
 (dyadicmap obj1 obj2 #'<= ))

(defmethod .>= ((obj1 tensor) (obj2 tensor))
 (dyadicmap obj1 obj2 #'>= ))

(defmethod .= ((obj1 tensor) (obj2 tensor))
 (dyadicmap obj1 obj2 #'= ))

(defmethod .or ((obj1 tensor) (obj2 tensor))
  (dyadicmap obj1 obj2 #'or ))

(defmethod .and ((obj1 tensor) (obj2 tensor))
  (dyadicmap obj1 obj2 #'and ))


;;;not working
(defmethod reshape ((obj1 tensor)(obj2 tensor))
  "Reshapes a tensor with the dimensions of the first tensor and values of second"
  (let* ((tensor1 (tensor-read obj1))
         (tensor2 (tensor-read obj2))
         (temp '()))
    (cond ((= (length tensor1) 1) (progn (loop for i from 0 to (- (first tensor1) 1) do
                                               (setf temp (append temp (list (nth i tensor2)))))
                                    (make-tensor temp)))
      (t (loop for i from 0 to (- (first tensor1) 1) do
               (setf temp (append temp (list (reshape (make-tensor (rest tensor1)) obj2)))))))
    temp))

;;;count numbers needed

;;; working always same numbers
(defmethod reshape2 ((obj1 tensor)(obj2 tensor))
  "Reshapes a tensor with the dimensions of the first tensor and values of second"
  (let* ((tensor1 (tensor-read obj1))
         (tensor2 (tensor-read obj2))
         (temp '()))
    (cond ((= (length tensor1) 1) (progn (loop for i from 0 to (- (first tensor1) 1) do
                                               (setf temp (append temp (list (nth i tensor2)))))
                                    (make-tensor temp)))
      (t (loop for i from 0 to (- (first tensor1) 1) do
               (setf temp (append temp (list (reshape (make-tensor (rest tensor1)) obj2)))))))
    (make-tensor temp)))
           
           
           
           
  
  













          
    
    
    
  


  
  