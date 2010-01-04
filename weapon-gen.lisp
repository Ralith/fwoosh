(in-package :fwoosh)

(defclass gun ()
  (
   ;; Randomized values
   (caliber :initform 0 :initarg :caliber :accessor caliber)
   (barrel-length :initform 0 :initarg :barrel-length :accessor barrel-length)
   (magazine-size :initform 0 :initarg :magazine-size :accessor magazine-size)
   (firing-rate :initform 0 :initarg :firing-rate :accessor firing-rate)
   ;; Derived values
   (muzzle-velocity :initform 0 :initarg :muzzle-velocity :accessor muzzle-velocity)
   (accuracy :initform 0 :initarg :accuracy :accessor accuracy) ;radians
   (mass :initform 0 :initarg :mass :accessor mass) ;grams
   (recoil :initform 0 :initarg :recoil :accessor recoil))) ;radians

(define-print-object (gun)
  (with-slots (caliber barrel-length magazine-size firing-rate) gun
        (format t "~a" (tag-gun gun))))

;;; TODO: Derivation of weight, accuracy, recoil
;;; Should light/heavy guns take the same ammo?

(defun normal-random (mean standard-deviation &optional (state *random-state*))
  "Performs a Box-Muller transform on CL:RANDOM"
  (+ mean
     (* (sqrt (* -2 (log (random 1.0 state))))
        (cos (* 2 pi (random 1.0 state)))
        standard-deviation)))

;;; TODO: Make these pure data.
(defparameter *gun-modifiers*
  (macrolet ((modifier (name probability &body params)
               (declare (ignore name))
               `(list ,probability ,@(mapcar (fun `(list (quote ,(first _)) ,@(rest _)))
                                             params)))
             (dist (&rest distributions)
               (let ((literal `(list ,@(mapcar (fun `(list ,@_))
                                               distributions))))
                 (if (> (length distributions) 1)
                     `(let ((selector (make-discrete-random-var ,(apply #'vector (mapcar #'first distributions)))))
                        #'(lambda () (apply #'normal-random
                                            (rest (nth (funcall selector)
                                                       ,literal)))))
                     `#'(lambda () (apply #'normal-random (rest (first ,literal))))))))
    (list (modifier "Base/Pistol" 1
                    (caliber #'+ (dist (1 6 1)))
                    (magazine-size #'+ (dist (2/3 7 2) (1/3 10 3)))
                    (barrel-length #'+ (dist (1 80 10)))
                    (firing-rate #'+ (dist (2/3 5 2) (1/3 10 3))))
          (modifier "Magazine" 1/2
                    (magazine-size #'+ (dist (1 20 5))))
          (modifier "Rifle" 1/2
                    (barrel-length #'+ (dist (1/4 100 50)
                                             (2/4 500 100)
                                             (1/4 700 200))))
          (modifier "Automatic" 1/3
                    (magazine-size #'+ (dist (2/3 100 25)
                                             (1/3 200 50)))
                    (barrel-length #'+ (dist (1 200 50)))
                    (firing-rate #'+ (dist (1 5 2))))
          (modifier "Heavy" 1/6
                    (caliber #'+ (dist (1 12 2)))
                    (magazine-size #'/ (dist (1 2 1))) ;TODO: use a logarithm instead
                    (firing-rate #'/ (dist (1 3 1))))))
  "A list of variously probable sets of probability distribution samplers each associated with one or more gun attributes.") 

(defparameter *gun-postprocessor*
  `((caliber . ,(fun (max 3 _)))
    (barrel-length . ,(fun (max 40 _)))
    (magazine-size . ,(fun (flet ((round-multiple (value factor)
                                    (* factor (round value factor))))
                             (cond
                               ((< _ 30) (max 1 (round _)))
                               ((< _ 80) (round-multiple _ 5))
                               (t (round-multiple _ 10))))))
    (firing-rate . ,(fun (max 1 _))))
  "An alist associating gun attributes with post-processor functions.")

(defun generate-gun (modifiers post-processor &aux (gun (make-instance 'gun)))
  (with-slots (caliber barrel-length magazine-size muzzle-velocity accuracy mass recoil)
      gun
    ;; Build gun by sequential application of modifiers
    (loop for (probability . clauses) in modifiers
       when (<= (random 1.0) probability)
       do (loop for (slot operator randomizer) in clauses
             ;; All parameters start at 0
             do (setf (slot-value gun slot)
                      (funcall operator
                               (slot-value gun slot) (funcall randomizer)))))
    ;; Sanify gun
    (loop for (slot . function) in post-processor
       do (setf (slot-value gun slot) (funcall function (slot-value gun slot))))
    ;; Calculate derived values.  TODO: Randomness?
    (setf muzzle-velocity (+ 700 (/ barrel-length 2)))
    (setf accuracy (/ pi 8 (/ barrel-length 100)))
    (setf mass (+ (* caliber magazine-size)
                  (/ barrel-length 3)))
    (setf recoil (/ (* caliber muzzle-velocity) mass 100))
    gun))

(defun tag-gun (gun &aux tags)
  (with-slots (caliber barrel-length magazine-size firing-rate) gun
    (macrolet ((tag (&rest clauses)
                 `(let ((new-tag
                         (cond ,@(loop
                                    for (condition tag) on clauses by #'cddr
                                    collecting (list condition tag)))))
                    (when new-tag
                      (push new-tag tags)))))
      (tag (> barrel-length 800) :long-rifle
           (> barrel-length 400) :rifle
           (> barrel-length 150) :carbine
           t                     :pistol)
      (tag (and (> magazine-size 15)
                (> firing-rate 10)) :machine
           (> magazine-size 15) :automatic)
      (tag (> caliber 10) :heavy
           (> caliber 6)  :medium
           t              :light))))
