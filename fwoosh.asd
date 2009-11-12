(asdf:defsystem fwoosh
  :maintainer "Benjamin Saunders"
  :depends-on (#:cl-opengl #:cl-glfw #:cffi)
  :components
  ((:file "package")
   (:file "fwoosh")))
