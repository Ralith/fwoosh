(in-package :fwoosh)

(defun fwoosh ()
  (glfw:init)
  (unwind-protect (progn
                    (glfw:open-window 0 0 0 0 0 0 16 0)
                    (main-loop))
    (glfw:terminate)))

(defun main-loop ()
  (loop while (/= 0 (glfw:get-window-param glfw:+opened+)) do
       (gl:clear :color-buffer :depth-buffer)
       (glfw:swap-buffers)
       (sleep (/ 60))))
