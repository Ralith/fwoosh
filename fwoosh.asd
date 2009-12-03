(asdf:defsystem fwoosh
  :maintainer "Benjamin Saunders"
  :depends-on (#:cl-opengl #:cl-glfw #:cffi)
  :components
  ((:file "package")
   (:file "utils" :depends-on ("package"))
   (:file "fwoosh" :depends-on ("package"))))
