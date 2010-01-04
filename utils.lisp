(in-package :fwoosh)

(defmacro with-gensyms ((&rest vars) &body body)
  `(let ,(loop for var in vars collect `(,var (gensym ,(symbol-name var))))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(defmacro fun (&rest body)
  `#'(lambda (&optional _) ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbolicate (&rest things)
    "Concatenate together the names of some strings and symbols,
producing a symbol in the current package."
    (let ((name (make-string (reduce #'+ things :key (fun (length (string _)))))))
      (let ((index 0))
        (dolist (thing things (values (intern name)))
          (let ((x (string thing)))
            (replace name x :start1 index)
            (incf index (length x))))))))

(macrolet ((define-ensure-foo (place) ; Lisp macros are nice
             `(defun ,(symbolicate "ENSURE-" place) (place &optional (default place))
                (if (atom place) default (,place place)))))
  (define-ensure-foo car)
  (define-ensure-foo cadr))

(defmacro define-print-object ((class &key (identity t) (type t)) &body body)
  "Defines a `print-object' method on class CLASS, using the standard macro
`print-unreadable-object'. The IDENTITY and TYPE keyword arguments are passed
through to `print-unreadable-object', although they default to T if not supplied.

CLASS can be a list of the form (VARIABLE CLASS-NAME), in which case
the `print-object' method will be specialized on class CLASS-NAME and VARIABLE
will be used as the parameter name. Alternatively, as shorthand, CLASS can be a
single symbol, which will be used for both the variable and the class name."
  (let ((object (ensure-car class))
        (class-name (ensure-cadr class)))
    (with-gensyms (stream)
      `(defmethod print-object ((,object ,class-name) ,stream)
         (print-unreadable-object (,object ,stream :type ,type :identity ,identity)
           (let ((*standard-output* ,stream)) ,@body))))))
