(in-package :fwoosh)

(cffi:defcallback resize-handler :void ((width :int)
                                        (height :int))
  (format t "Resizing to ~ax~a~%" width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 45 (/ width height) 1 200)
  (gl:matrix-mode :modelview))

(defun main ()
  (glfw:with-init
    (glfw:with-open-window ("fwoosh" 0 0 0 0 0 0 16)
      (glfw:set-window-size-callback (cffi:callback resize-handler))
      (init-resources)
      (main-loop))))

(defun init-resources ()
  (format t "OpenGL version ~a~%" (gl:get-string :version))
  (gl:clear-color 0 0 0 0))

(defun main-loop ()
  (loop while (/= 0 (glfw:get-window-param glfw:+opened+)) do
       (gl:clear :color-buffer :depth-buffer)
       (gl:with-pushed-matrix
         (gl:load-identity)
         (gl:with-primitive :quad-strip
           (mapc (lambda (args) (apply #'gl:vertex args))
                 ;; front/back top/bottom left/right
                 '((-10 -10 -10)        ;BBL
                   (-10  10 -10)        ;BTL
                   ( 10 -10 -10)        ;BBR
                   ( 10  10 -10)        ;BTR
                   ( 10 -10  10)        ;FBR
                   ( 10  10  10)        ;FTR
                   (-10 -10  10)        ;FBL
                   (-10  10  10)        ;FTL
                   (-10 -10 -10)        ;BBL
                   (-10  10 -10)))))    ;BTL
       (glfw:swap-buffers)
       (sleep (/ 60))))
