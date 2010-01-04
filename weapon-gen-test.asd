(asdf:defsystem weapon-gen-test
  :maintainer "Benjamin Saunders"
  :components
  ((:file "package")
   (:file "utils" :depends-on ("package"))
   (:file "alias-method" :depends-on ("package"))
   (:file "weapon-gen" :depends-on ("package" "alias-method" "utils"))))