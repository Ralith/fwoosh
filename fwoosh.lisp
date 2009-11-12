(in-package :fwoosh)

(defun fwoosh ()
  (glfw:init)
  (unwind-protect (progn
                    (glfw:open-window 0 0 0 0 0 0 16 0)
                    (init-resources)
                    (main-loop))
    (glfw:terminate)))

(defun init-resources ()
  (format t "OpenGL version ~a~%" (gl:get-string :version))
  (gl:clear-color 0 0 0 0)
  (gl:ortho 0 1 0 1 -1 1))

(defun main-loop ()
  (loop while (/= 0 (glfw:get-window-param glfw:+opened+)) do
       (gl:clear :color-buffer :depth-buffer)
       (gl:with-primitive :polygon
         (gl:color 0 1 0)
         (gl:vertex 1/4 1/4 0)
         (gl:vertex 3/4 1/4 0)
         (gl:vertex 3/4 3/4 0)
         (gl:vertex 1/4 3/4 0))
       (glfw:swap-buffers)
       (sleep (/ 60))))
