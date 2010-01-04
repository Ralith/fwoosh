(in-package :fwoosh)

;;; TODO: Derivation of name, class?, weight, rate of fire?, accuracy, recoil
;;; Should light/heavy guns take the same ammo?

(defun normal-random (mean standard-deviation &optional (state *random-state*))
  "Performs a Box-Muller transform on CL:RANDOM"
  (+ mean
     (* (sqrt (* -2 (log (random 1.0 state))))
        (cos (* 2 pi (random 1.0 state)))
        standard-deviation)))

;;; (modifier weight (attribute #'(lambda (old change) ...) #'make-change)*)
(defparameter *gun-modifiers*
  (macrolet ((modifier (name probability &body params)
               (declare (ignore name))
               `(list ,probability ,@(mapcar (fun (cons 'list _))
                                             params)))
             (dist (&rest distributions)
               (let ((literal `(list ,@(mapcar (fun (cons 'list _))
                                               distributions))))
                 (if (> (length distributions) 1)
                     `(let ((selector (make-discrete-random-var ,(apply #'vector (mapcar #'first distributions)))))
                        #'(lambda () (apply #'normal-random
                                            (rest (nth (funcall selector)
                                                       ,literal)))))
                     `#'(lambda () (apply #'normal-random (rest (first ,literal))))))))
    (list (modifier "Base/Pistol" 1
                    (:caliber #'+ (dist (1 6 1)))
                    (:magazine-size #'+ (dist (2/3 7 2) (1/3 10 3)))
                    (:barrel-length #'+ (dist (1 100 10))))
          (modifier "Magazine" 1/2
                    (:magazine-size #'+ (dist (1 25 5))))
          (modifier "Rifle" 1/4
                    (:barrel-length #'+ (dist (4/5 400 100)
                                              (1/5 800 200))))
          (modifier "Automatic" 1/6
                    (:magazine-size #'+ (dist (2/3 100 25)
                                              (1/3 200 50)))
                    (:barrel-length #'+ (dist (1 200 50))))
          (modifier "Heavy" 1/6
                    (:caliber #'+ (dist (1 12 2)))
                    (:magazine-size #'/ (dist (1 2 1)))))) ;TODO: use a logarithm instead
  "A list of variously probable sets of probability distribution samplers each associated with one or more gun attributes.") 

(defparameter *gun-postprocessor*
  (flet ((round-multiple (value factor)
           (* factor (round value factor))))
   `((:caliber . ,(fun (max 3 _)))
     (:barrel-length . ,(fun (max 40 _)))
     (:magazine-size . ,(fun (cond
                               ((< _ 30) (max 1 (round _)))
                               ((< _ 80) (round-multiple _ 5))
                               (t (round-multiple _ 10)))))))
  "An alist associating gun attributes with post-processor functions.")

(defun make-gun (modifiers post-processor &aux gun)
  ;; Build gun by sequential application of modifiers
  (loop for (probability . clauses) in modifiers
     when (<= (random 1.0) probability)
     do (loop for (attrib operator randomizer) in clauses
           ;; All parameters start at 0
           do (pushnew (cons attrib 0) gun
                       :key #'car)
             (let ((cell (assoc attrib gun)))
               (setf (cdr cell)
                     (funcall operator
                              (cdr cell) (funcall randomizer))))))
  ;; Sanify gun
  (loop for cell in gun
     do (setf (cdr cell) (funcall (cdr (assoc (car cell) post-processor)) (cdr cell)))
     finally (return gun)))

(defun caliber (gun)
  (cdr (assoc :caliber gun)))
(defun magazine-size (gun)
  (cdr (assoc :magazine-size gun)))
(defun barrel-length (gun)
  (cdr (assoc :barrel-length gun)))

;;; TODO: with-slots
(defun classify-gun (gun &aux class)
  (push (cond ((> (caliber gun) 10) 'heavy)
              ((> (caliber gun) 6) 'medium)
              (t 'light))
        class))