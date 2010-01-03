(in-package :fwoosh)

;;; TODO: Modifier-based generator, classification system, naming system
;;; Should light/heavy guns take the same ammo?

(defun normal-random (mean standard-deviation &optional (state *random-state*))
  "Performs a Box-Muller transform on CL:RANDOM"
  (+ mean
     (* (sqrt (* -2 (log (random 1.0 state))))
        (cos (* 2 pi (random 1.0 state)))
        standard-deviation)))

(defparameter *gun-parts*
  '(:caliber :barrel-length :magazine-size)
  (flet ((dist (gaussians &rest post-processors)
           "Takes a list of (weight mean std-dev)s for normal distributions"
           (let ((selector (make-discrete-random-var
                            (apply #'vector (mapcar #'first gaussians)))))
             #'(lambda ()
                 (reduce #'funcall post-processors
                         :initial-value (apply #'normal-random (rest (nth (funcall selector) gaussians)))
                         :from-end t)))))
    `((:caliber ,(dist '((3/4 6 1) (1/4 12 2)) ; units: mm
                       (fun (max 2 _))))
      (:barrel-length ,(dist '((1/3 100 30) (1/3 300 100) (1/3 500 200)) ; units: mm
                             (fun (max 30 _))))
      (:magazine-size ,(dist '((1/3 6 3) (1/3 30 10) (1/6 100 25) (1/6 200 50)) ; units: bullets
                             (fun (max 1 _))
                             #'truncate)))))

(defun sanify-gun (gun)
  (when (> (getf gun :magazine-size) 20)
    (incf (getf gun :barrel-length) (max 0 (normal-random 100 30))))
  gun)

(defun make-gun ()
  (sanify-gun (apply #'append
                     (mapcar (lambda (attrib)
                               (list (first attrib)
                                     (funcall (second attrib))))
                             *gun-parts*))))