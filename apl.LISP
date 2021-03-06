
;;; Tensor type definition

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
	    (loop for i from 0 to (- (count-dimensions  lista) 1) do
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
  
(defun monadicmap (tensor op)
  "Maps a tensor to a single argument operation"
  (let ((solution '()))
    (cond 
     ((atom (first tensor)) (setf solution (map 'list op tensor)))
     (t 
      (loop for i from 0 to (- (length tensor) 1) do
            (setf solution (append solution (list (monadicmap (nth i tensor) op)))))))
    solution))

;;; .- function defined in dyadic functions

;;; ./ defined in dyadic functions

(defmethod .! ((obj tensor))
  "Factorial function"
  (make-tensor (monadicmap (tensor-read obj) #'(lambda (num) (reduce #'* (loop for i from 1 to num collect i))))))

(defmethod .sin ((obj tensor))
  "Sin function"
  (make-tensor (monadicmap (tensor-read obj) #'sin)))

(defmethod .cos ((obj tensor))
  "Cos function"
  (make-tensor (monadicmap (tensor-read obj) #'cos)))

(defmethod .not ((obj tensor))
  "Negation function"
  (make-tensor (monadicmap (tensor-read obj) #'(lambda (num) (if (= num 0) 1 0)))))

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

(defun interval1 (num)
  "Interval of numbers between 0 and the given number"
  (make-tensor (number-sequence 1 num)))


;;; DYADIC FUNCTIONS

(defmethod dyadicmap ((obj1 tensor) (obj2 tensor) op)
  (let ((tensor1 (tensor-read obj1))
        (tensor2 (tensor-read obj2))
        (solution '()))
    (cond 
     ((atom (first tensor1))
      (setf solution (dyadicmap2 tensor1 tensor2 op)))
     (t 
      (loop for i from 0 to (- (length tensor1) 1) do
            (setf solution (append solution (list (dyadicmap (make-tensor (nth i tensor1)) (make-tensor (nth i tensor2)) op)))))))
    solution))

  
(defmethod dyadicmap2 (tensor1 tensor2 op)
  "Maps two tensors to a double argument operation"
    (cond ((= (length tensor1) 1) (dyadicmap-aux2 (first tensor1) tensor2 op))
      ((= (length tensor2) 1) (dyadicmap-aux2 (first tensor2) tensor1 op))
      ((= (length tensor1) (length tensor2)) (dyadicmap-aux1 tensor1 tensor2 op))
      (t "Error: Tensor's size must be equal or one of them scalar")))

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
  (make-tensor (dyadicmap obj1 obj2 #'+ )))



;;;Monadic & Dyadic
(defmethod .- ((obj1 tensor) &optional obj2)
  "Subtraction function."
  (if (null obj2)
      (make-tensor (monadicmap (tensor-read obj1) #'-))
      (dyadicmap obj1 obj2 #'- )))

(defmethod .* ((obj1 tensor) (obj2 tensor))
  "Multiplication function."
  (dyadicmap obj1 obj2 #'* ))

;;;Monadic & Dyadic
(defmethod ./ ((obj1 tensor) &optional obj2)
  "Division function."
    (if (null obj2)
      (make-tensor (monadicmap (tensor-read obj1) #'/))
      (dyadicmap obj1 obj2 #'/ )))

(defmethod .// ((obj1 tensor) (obj2 tensor))
  "Integer division function."
  (dyadicmap obj1 obj2 #'floor ))

(defmethod .% ((obj1 tensor) (obj2 tensor))
  "Remainder integer division function."
  (dyadicmap obj1 obj2 #'mod ))
 
 (defmethod .< ((obj1 tensor) (obj2 tensor))
"Less function."
(if (= (length (tensor-read obj1) ) 1)
  (make-tensor (dyadicmap obj1 obj2 #'(lambda (tensor1 tensor2) (if (> tensor1 tensor2) 1 0))))
  (make-tensor (dyadicmap obj1 obj2 #'(lambda (tensor1 tensor2) (if (< tensor1 tensor2) 1 0)))))) 

(defmethod .> ((obj1 tensor) (obj2 tensor))
"Bigger function."
(if (= (length (tensor-read obj1) ) 1)
  (make-tensor (dyadicmap obj1 obj2 #'(lambda (tensor1 tensor2) (if (< tensor1 tensor2) 1 0))))
  (make-tensor (dyadicmap obj1 obj2 #'(lambda (tensor1 tensor2) (if (> tensor1 tensor2) 1 0))))))   

(defmethod .<= ((obj1 tensor) (obj2 tensor))
"Less or equal function."
 (if (= (length (tensor-read obj1) ) 1)
  (dyadicmap obj1 obj2 #'(lambda (tensor1 tensor2) (if (>= tensor1 tensor2) 1 0)))
  (dyadicmap obj1 obj2 #'(lambda (tensor1 tensor2) (if (<= tensor1 tensor2) 1 0)))))   

(defmethod .>= ((obj1 tensor) (obj2 tensor))
"Bigger or equal function."
(if (= (length (tensor-read obj1) ) 1)
  (make-tensor (dyadicmap obj1 obj2 #'(lambda (tensor1 tensor2) (if (<= tensor1 tensor2) 1 0))))
  (make-tensor (dyadicmap obj1 obj2 #'(lambda (tensor1 tensor2) (if (>= tensor1 tensor2) 1 0))))))   



(defmethod .= ((obj1 tensor) (obj2 tensor))
  "Equal function."
 (dyadicmap obj1 obj2 #'(lambda (tensor1 tensor2) (if (= tensor1 tensor2) 1 0))))    

(defmethod .or ((obj1 tensor) (obj2 tensor))
  "Or function."
  (dyadicmap obj1 obj2 #'(lambda (tensor1 tensor2) (if (or (= tensor1 1) (= tensor2 1)) 1 0))))  

(defmethod .and ((obj1 tensor) (obj2 tensor))
  "And function."
  (dyadicmap obj1 obj2 #'(lambda (tensor1 tensor2) (if (and (= tensor1 1) (= tensor2 1)) 1 0))))  


(defun reshape-aux (tensor1 tensor2)
  (let ((indice 0))
    (labels ((recursao  (tensor1 tensor2)
               (let ((temp '()))
                 (cond ((null (cdr tensor1))
                        (progn 
                          (loop for i from 0 to (- (first tensor1) 1) do 
                                (setf temp (append temp (list (nth (mod indice (length tensor2)) tensor2))))
                                (incf indice))
                           temp))
                   (t (loop for i from 0 to (- (first tensor1) 1) do
                            (setf temp (append temp (list (recursao (rest tensor1) tensor2)))))
                     temp)))))
      (recursao tensor1 tensor2)))) 
  
(defmethod reshape ((obj1 tensor)(obj2 tensor))
  "Reshapes a tensor with the dimensions of the first tensor and values of second"
  (make-tensor (reshape-aux (tensor-read obj1) (flatten (tensor-read obj2)))))

(defun flatten (structure)
  "Auxiliar function to remove parentisis from a list, keeping values."
  (cond ((null structure) nil)
        ((atom structure) (list structure))
    (t (mapcan #'flatten structure))))

(defun drop-aux (tensor1 tensor2)
  (let* ((num tensor1)
         (tensor tensor2)
         (size (- (length tensor) 1))
         (temp '()))
    (cond ((atom (first tensor))
                 (if (< (first num) 0)
                     (loop for i from 0 to (+ size (first num)) do
                           (setf temp (append temp (list (nth i tensor)))))
                   (loop for i from (first num) to size do
                         (setf temp (append temp (list (nth i tensor)))))))
      (t (if (< (first num) 0)
               (loop for i from 0 to (+ size (first num)) do
                     (setf temp (append temp (list (nth i tensor)))))
           (loop for i from (first num) to size do
                  (setf temp (append temp (list (nth i tensor))))))
        (loop for i from 0 to (- (length temp) 1) do
              (setf (nth i temp) (drop-aux (rest num) (nth i temp)))))) 
    temp))

(defmethod drop ((obj1 tensor) (obj2 tensor))
  "Deletes number of columns/rows specified in tensor obj1 from tensor obj2"
  (make-tensor (drop-aux (tensor-read obj1) (tensor-read obj2))))



(defmethod member? ((obj1 tensor)(obj2 tensor))
  "Checks if obj1 tensor contains any number from obj2 tensor."
  (make-tensor (member?-aux (tensor-read obj1) (tensor-read obj2))))

(defun member?-aux (tensor1 tensor2)
  (let ((result (monadicmap tensor1 #'identity)))
    (cond 
          ((atom (first tensor1)) 
           (loop for i from 0 to (- (length tensor1) 1) do
                (setf (nth i result) 0))
           (loop for i from 0 to (- (length tensor1) 1) do
                 (loop for j from 0 to (- (length tensor2) 1) do
                       (if (= (nth i tensor1) (nth j tensor2))
                           (setf (nth i result) 1)))))
      (t (if (not(null (first tensor1)))(setf result (append result (list (member?-aux (first tensor1) tensor2)))))
        (if (not(null (rest tensor1)))(setf result (append result (list (member?-aux (rest tensor1) tensor2)))))))
    result))


(defmethod select ((obj1 tensor)(obj2 tensor))
  "Returns a tensor of booleans with the same shape and dimension of 1st arg."
  (make-tensor (select-aux (tensor-read obj1) (tensor-read obj2))))

(defun select-aux (tensor1 tensor2)
  (let ((result '()))
    (cond ((atom (first tensor2))
           (loop for i from 0 to (- (length tensor2) 1) do
                 (if (= (nth i tensor1) 1)
                     (setf result (append result (list (nth i tensor2)))))))
      (t (setf result (append result (list(select-aux tensor1 (first tensor2)))))
        (setf result (append result (list (select-aux tensor1 (first(rest tensor2))))))))
    result))




;;;; MISSING CATENATE

;;; MONADIC OPERATORS

(defmethod fold (op)
  "Accepts a function and returns another funtion which receives one arg. 
Computes sucessive op's to list elements and retuns the result"
  (lambda (tensor)
    (let* ((vec (tensor-read tensor))
           (result (first vec)))
      (loop for i from 1 to (- (length vec) 1) do
            (setf result (first (tensor-read (funcall op (s result) (s (nth i vec)))))))
      result)))

(defmethod scan (op)
  "Applies op the increasinglt large subsets of the elements of the vector."
  (lambda (tensor)
    (let* ((vec (tensor-read tensor))
           (result (first vec)))
      (loop for i from 1 to (- (length vec) 1) do
            (setf (nth i vec) (first (tensor-read (funcall op (s result) (s (nth i vec))))))
            (setf result (nth i vec)))
      vec)))

(defmethod outer-product (op)
  "Receives a function and returns the outer product of two tensors."
  (lambda (obj1 obj2) 
    (let* ((tensor1 (tensor-read obj1))
           (tensor2 (tensor-read obj2))
           (result '()))
      (labels ((calc-outer (tensor1 tensor2 op)
        (let ((result '())
              (temp '())) 
          (loop for i from 0 to (- (length tensor1) 1) do 
                (loop for j from 0 to (- (length tensor2) 1) do
                      (setf temp (append temp (list (funcall op (s (nth i tensor1)) (s (nth j tensor2)))))))
                (setf result (append result (list temp)))
                (setf temp '()))
           (format t "ONE ~A ~%" result)
          result))
        (outer-iter-right (tensor1 tensor2 op)
          (let ((result '())
                 (temp '()))
             (cond ((and 
                     (atom (first tensor1))
                     (atom (first tensor2)))
                    (setf result (calc-outer tensor1 tensor2 op)))
               ((atom (first tensor1)) 
                (loop for i from 0 to (- (length tensor1) 1) do
                      (loop for j from 0 to (- (length tensor2) 1) do
                            (if (atom (nth j tensor2))
                                (setf temp (append temp (list (calc-outer (list (nth i tensor1)) (list (nth j tensor2)) op))))
                              (setf temp (append temp (list (outer-iter-right (list (nth i tensor1)) (nth j tensor2) op))))))
                      ;;; works for vector x matrix but not matrix x vector (remove list)
                      (setf result (append result (list (list temp))))
                      (setf temp '()))))
             (format t "TWO ~A ~%" result)
             result)))
         (if (atom (first tensor1))
             (setf result (outer-iter-right tensor1 tensor2 op))
           (loop for i from 0 to (- (length tensor1) 1) do
                 (setf result (append result (list (outer-iter-right (nth i tensor1) tensor2 op))))))
        (format t "ONE ~A ~%" result)
        (make-tensor result)))))

;;;; EXERCISES

(defmethod tally ((obj tensor))
"Given a tensor retunrs a scalar with the number of elements."
  (s (funcall (fold #'.*) (shape obj))))

(defmethod rank ((obj tensor))
 "Given a tensor returns a scalar with the dimension of elements."
  (s (shape (shape obj))))

(defmethod within ((obj tensor) (scalar1 tensor) (scalar2 tensor))
  "Given a vector and two scalars returns the values in interval defined by the scalars."
  (select (funcall #'.<= (select (funcall #'.>= obj scalar1) obj) scalar2) (select (funcall #'.>= obj scalar1) obj)))
  
(defmethod ravel ((obj tensor))
  "Given a tensor, returns a vector with all elements of tensor."
  (reshape (v (first (tensor-read (tally obj)))) obj))

(defun primes (indice)
    (let ((numbers (drop (s 1) (interval1 (car (tensor-read indice))))))
        (select (.not (member? numbers (funcall (outer-product #'.*) numbers numbers)))
                numbers)))




            



                        
      
      


  








           
           
           
  
  













          
    
    
    
  


  
  