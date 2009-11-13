(in-package :fwoosh)

(cffi:defcallback resize-handler :void ((width :int)
                                        (height :int))
  (format t "Resizing to ~ax~a~%" width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((x (/ width 2))
        (y (/ height 2)))
    (gl:viewport 0 0 width height)
    (gl:ortho (- x) x (- y) y -1 1))
  (gl:matrix-mode :modelview))

(defun fwoosh ()
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
       (gl:with-primitive :polygon
         (gl:color 0 1 0)
         (gl:vertex -100 -100 0)
         (gl:vertex 100 -100 0)
         (gl:vertex 100 100 0)
         (gl:vertex -100 100 0))
       (glfw:swap-buffers)
       (sleep (/ 60))))
