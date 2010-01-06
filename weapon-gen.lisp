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
   (mass :initform 0 :initarg :mass :accessor mass) ;kilograms
   (recoil :initform 0 :initarg :recoil :accessor recoil))) ;kilonewton-seconds

(define-print-object (gun)
  (with-slots (caliber barrel-length magazine-size firing-rate) gun
        (format t "~a" (tag-gun gun))))

(defun normal-random (mean standard-deviation &optional (state *random-state*))
  "Performs a Box-Muller transform on CL:RANDOM"
  (+ mean
     (* (sqrt (* -2 (log (random 1.0 state))))
        (cos (* 2 pi (random 1.0 state)))
        standard-deviation)))

(defmacro with-dists ((&rest clauses) &body body)
  (let ((names (loop repeat (length clauses) collecting (gensym))))
   `(let (,@(loop for (name . distributions) in clauses
               for i from 0
               collecting `(,(nth i names) (make-discrete-random-var ,(apply #'vector (mapcar #'first distributions))))))
      (symbol-macrolet (,@(loop for name in (mapcar #'first clauses)
                               for i from 0
                               collecting `(,name (apply #'normal-random (rest (nth (funcall ,(nth i names)) ',(rest (assoc name clauses))))))))
        ,@body))))

;;; TODO: Make these pure data.
(defparameter *gun-modifiers*
  (macrolet ((modifier (name probability &body effects)
               (declare (ignore name))
               `(list ,probability
                      ,@(loop for (slot dists function) in effects
                           collecting `(cons ',slot (with-dists ,dists
                                                      (fun ,function)))))))
    (list (modifier "Base" 1
                    (caliber ((d (1 6 2)))
                             (+ _ d))
                    (magazine-size ((d (2/3 7 2) (1/3 10 3)))
                                   (+ _ d))
                    (barrel-length ((d (1 70 20)))
                                   (+ _ d))
                    (firing-rate ((d (2/3 3 1) (1/3 7 2)))
                                 (+ _ d)))
          (modifier "Magazine" 1/2
                    (magazine-size ((d (1 20 5)))
                                   (+ _ d))
                    (firing-rate ((d (1 5 2)))
                                 (+ _ d)))
          (modifier "Carbine" 1/4
                    (barrel-length ((d (1 200 50)))
                                   (+ _ d))
                    (magazine-size ((d (1 20 5)))
                                   (+ _ d))
                    (firing-rate ((d (1 5 2)))
                                 (+ _ d)))
          (modifier "Automatic" 1/3
                    (magazine-size ((d (1 4 2)))
                                   (* _ (max 1 d)))
                    (barrel-length ((d (1 200 50)))
                                   (+ _ d))
                    (firing-rate ((d (1 5 2)))
                                 (+ _ d)))
          (modifier "Rifle" 1/4
                    (barrel-length ((d (1/2 400 100)
                                       (1/2 600 50)))
                                   (+ _ d))
                    (magazine-size ((d (2/3 5 2)
                                       (1/3 10 4)))
                                   (+ _ d)))
          (modifier "Sniper" 1/6
                    (barrel-length ((d (1 700 200)))
                                   (+ (/ _ 2) d))
                    (magazine-size ((d (1 2 1)))
                                   (log _ (max 1.5 d)))
                    (firing-rate ((d (1 2 1)))
                                 (log _ (max 1.5 d))))
          (modifier "Heavy" 1/6
                    (caliber ((d (1 8 2)))
                             (+ _ d))
                    (magazine-size ((d (1 2 1)))
                                   (log _ (max 1.5 d)))
                    (firing-rate ((d (1 2 1)))
                                 (log _ (max 1.5 d)))))))

(defparameter *gun-postprocessor*
  `((caliber . ,(fun (max 3 _)))
    (barrel-length . ,(fun (max 40 _)))
    (magazine-size . ,(fun (flet ((round-multiple (value factor)
                                    (* factor (round value factor))))
                             (cond
                               ((< _ 30) (max 0 (round _)))
                               ((< _ 80) (round-multiple _ 5))
                               (t (round-multiple _ 10))))))
    (firing-rate . ,(fun (max 1/2 _))))
  "An alist associating gun attributes with post-processor functions.")

(defun generate-gun (modifiers post-processor &aux (gun (make-instance 'gun)))
  (with-slots (caliber barrel-length magazine-size muzzle-velocity accuracy mass recoil)
      gun
    ;; Build gun by sequential application of modifiers
    (loop for (probability . clauses) in modifiers
       when (<= (random 1.0) probability)
       do (loop for (slot . function) in clauses
             ;; All parameters start at 0
             do (setf (slot-value gun slot)
                      (funcall function (slot-value gun slot)))))
    ;; Sanify gun
    (loop for (slot . function) in post-processor
       do (setf (slot-value gun slot) (funcall function (slot-value gun slot))))
    ;; Calculate derived values.  TODO: Randomness?
    ;; TODO: Data-driven
    (macrolet ((twiddle (slot)
                 `(setf ,slot (max 0 (normal-random ,slot (/ ,slot 10))))))
      (setf muzzle-velocity (+ 700 (/ barrel-length 2)))
      (twiddle muzzle-velocity)
      (setf accuracy (/ pi 16 (/ barrel-length 200) (/ caliber 4)))
      (twiddle accuracy)
      (setf mass (+ (/ (* caliber (/ barrel-length 100)) 10)
                    (/ magazine-size 50)))
      (twiddle mass)
      (setf recoil (/ (* caliber muzzle-velocity) 1000))
      (twiddle recoil))
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
      (tag (> barrel-length 800) :sniper
           (> barrel-length 400) :rifle
           (> barrel-length 150) :carbine
           (> barrel-length 50)  :pistol
           t                     :derringer)
      (when (> magazine-size 15)
        (tag (> firing-rate 15) :spammy
             (> firing-rate 10) :machine
             (> firing-rate 5)  :automatic))
      (tag (> caliber 15) :cannon
           (> caliber 10) :heavy
           (> caliber 6)  :medium
           (> caliber 3)  :light
           t              :toothpick))))

(defun describe-gun (gun &optional (stream *standard-output*))
  (with-slots (caliber barrel-length magazine-size firing-rate muzzle-velocity accuracy mass recoil)
      gun
    (format stream "A ~{~a ~}having caliber ~4,2f mm, a barrel ~4,2f mm long, firing ~a rounds per reload at ~4,2f rounds per second, with a muzzle velocity of ~4,2f m/s, standard divergence of ~4,3f radians, massing ~4,2f kg, and recoiling with ~4,2f kilonewton-seconds." (tag-gun gun) caliber barrel-length magazine-size firing-rate muzzle-velocity accuracy mass recoil)))
