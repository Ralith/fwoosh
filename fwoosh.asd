(asdf:defsystem fwoosh
  :maintainer "Benjamin Saunders"
  :depends-on (#:cl-opengl #:cl-glfw #:cffi)
  :components
  ((:file "package")
   (:file "fwoosh" :depends-on ("package"))
   (:file "marching-cubes" :depends-on ("package"))))
