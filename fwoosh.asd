(asdf:defsystem fwoosh
  :maintainer "Benjamin Saunders"
  :depends-on (:cl-opengl :cl-glu :cl-glfw :cffi)
  :components
  ((:file "package")
   (:file "utils" :depends-on ("package"))
   (:file "alias-method" :depends-on ("package"))
   (:file "weapon-gen" :depends-on ("package" "alias-method" "utils"))
   (:file "fwoosh" :depends-on ("package"))))
