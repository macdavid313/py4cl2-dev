(in-package :py4cl2)

(defun numeric-char-p (ch) (find ch "0123456789"))

(defun split-point-p (pch ch)
  (or (and (upper-case-p ch) (lower-case-p pch))
      (and (numeric-char-p ch) (alpha-char-p pch))
      (char= pch #\_)))

(defun collect-first-word (char-list) ; can this be simplified?
  "Returns ((h e l l o) W o r l d), given (h e l l o W o r l d)."
  (iter (for ch-list initially char-list
             then (cdr ch-list))
    (while ch-list)
    (for ch = (first ch-list))
    (for pch previous ch)
    (for word initially ()
         then (cons ch word))
    (unless (first-iteration-p) (until (split-point-p pch ch)))
    (finally (return (if ch-list
                         (cons (nreverse word) ch-list)
                         (list char-list))))))

(defun break-into-words (char-list)
  "Returns ((h e l l o) (W o r l d)), given (h e l l o W o r l d)."
  (when char-list
    (destructuring-bind (word . rem-chars) (collect-first-word char-list)
      (cons word (break-into-words rem-chars)))))

(declaim (ftype (function (string) string) lispify-name))
(defun lispify-name (name)
  "Converts NAME to a lisp-like name. Specifically:
  1. Replaces underscores with hyphens.
  2. CamelCase is converted to CAMEL-CASE"
  (let* ((words (mapcar (lambda (word)
                          (coerce word 'string))
                        (remove-if #'null
                                   (break-into-words (coerce name 'list)))))
         (prefinal-string (if (eq (readtable-case *readtable*) :UPCASE) ; only upcase the words when *readtable* in in :UPCASE mode
                              (string-upcase (format nil "~{~A~^-~}" words))
                              (format nil "~{~a~^-~}" words))))
    (remove-if (lambda (ch)
                 (char= ch #\_))
               prefinal-string
               :end (1- (length prefinal-string)))))

(defun get-unique-symbol (symbol-name package-name)
  (declare (type string symbol-name))
  (multiple-value-bind (symbol location)
      (intern symbol-name package-name)
    (declare (ignore symbol))
    (if location
        (concatenate 'string
                     symbol-name "/1")
        symbol-name)))

(defun pythonize-kwargs (arg-plist)
  (nconc (iter (generate elt in arg-plist)
           (collect (%pythonize (next elt)))
           (collect (%pythonize (next elt)))
           (collect ","))
         '(")")))

;;; If anyone wants to generate something so as to allow this, feel free to :)
;;; (pyfun 1 2 3 :a 'a :b 'b)
;;; args '(1 2 3)
;;; kwargs '(:a a :b b)

(defun get-arg-list (fullname lisp-package)
  "Returns a list of two lists: PARAMETER-LIST and PASS_LIST"
  (%get-arg-list (intern (raw-pyeval "str(type(" fullname "))[8:-2]")
                         :keyword)
                 fullname
                 lisp-package))

(defmethod %get-arg-list ((package (eql :|numpy.ufunc|)) fullname lisp-package)
  (let* ((n (pyeval fullname ".nin"))
         (arg-list-without-keys
           (iter (for ch in-string "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
             (for i below n)
             (collect (intern (string ch) lisp-package)))))
    `(,(append arg-list-without-keys
               '(&rest keys &key out (where t) &allow-other-keys))
      ((declare (ignore out where))
       (apply #'pycall ,fullname ,@arg-list-without-keys keys)))))

(defmethod %get-arg-list ((package (eql :|builtin_function_or_method|)) fullname lisp-package)
  `((&rest args)
    (() (apply #'pycall ,fullname args))))

(defmethod %get-arg-list ((package (eql :|type|)) fullname lisp-package)
  `((&rest args)
    (() (apply #'pycall ,fullname args))))

(define-condition default-arg-list-necessary (condition) ())
(define-condition default-is-python-object (default-arg-list-necessary) ())
(define-condition both-positional-and-keyword-rest (default-arg-list-necessary) ())

;; https://stackoverflow.com/questions/2677185/how-can-i-read-a-functions-signature-including-default-argument-values
(defmethod %get-arg-list (package fullname lisp-package)

  (flet ((ensure-tuple->list (object)
           (if (and (stringp object) (string= object "()"))
               nil
               (coerce object 'list))))
    
    (handler-case      
        (let* ((parameters         (handler-case (raw-pyeval "tuple(inspect.signature("
                                                             fullname ").parameters.values())")
                                     ;; math.hypot does not have a signature; math.acos does
                                     (pyerror (e)
                                       (when (search "no signature found"
                                                     (slot-value e 'text))
                                         (signal 'default-arg-list-necessary)))
                                     (t nil)))
               (parameter-kinds    (raw-pyeval "tuple(map(lambda p: p.kind.name, "
                                               (%pythonize parameters) "))"))
               ;; names do not contain * or ** for var args
               (parameter-names    (raw-pyeval "tuple(map(lambda p: p.name, "
                                               (%pythonize parameters) "))"))
               (parameter-defaults (raw-pyeval "tuple(map(lambda p: p.default, "
                                               (%pythonize parameters) "))"))
               (parameter-empty-p  (raw-pyeval "tuple(map(lambda p: p.default == inspect._empty, "
                                               (%pythonize parameters) "))"))
               rest keyword-rest)
          
          (iter
            
            (initially

             (setq parameters         (ensure-tuple->list parameters)
                   parameter-kinds    (ensure-tuple->list parameter-kinds)
                   parameter-names    (ensure-tuple->list parameter-names)
                   parameter-defaults (ensure-tuple->list parameter-defaults)
                   parameter-empty-p  (ensure-tuple->list parameter-empty-p))
             
             (loop :for default :in parameter-defaults
                   ;; Needs that True translates to T; False translates to NIL
                   :if (and (typep default 'python-object)
                            (not (raw-pyeval "inspect._empty == " (%pythonize default))))
                     :do (signal 'default-is-python-object)))
            
            (for kind      in parameter-kinds)
            (for name      in parameter-names)
            (for empty-p   in parameter-empty-p)
            (for default   in parameter-defaults)
            (when empty-p (setq default nil))
            
            (for arg-symbol = (intern (lispify-name name) lisp-package))
            (setq arg-symbol
                  (intern (cond ((member arg-symbol arg-symbols)
                                 (get-unique-symbol (symbol-name arg-symbol) lisp-package))
                                ((eq t arg-symbol)
                                 (concatenate 'string "." (lispify-name name)))
                                ((eq nil arg-symbol)
                                 (concatenate 'string "." (lispify-name name)))
                                (t (symbol-name arg-symbol)))
                          lisp-package))
            (collect arg-symbol into arg-symbols)


            (alexandria:eswitch (kind :test 'string=)
              ("VAR_POSITIONAL"(setq rest         arg-symbol))
              ("VAR_KEYWORD"   (setq keyword-rest arg-symbol))
              ("POSITIONAL_ONLY"
               (if empty-p
                   (collect arg-symbol               into positional)
                   (collect `(,arg-symbol ',default) into optional)))
              ("KEYWORD_ONLY"
               (collect `(,arg-symbol ',default) into keyword))
              ("POSITIONAL_OR_KEYWORD"
               (collect `(,arg-symbol ',default) into keyword)))

            (alexandria:switch (kind :test 'string=)
              ("POSITIONAL_ONLY"
               (appending `((pythonize ,arg-symbol) ",")           into positional-pass))
              ("KEYWORD_ONLY"
               (appending `(,name "=" (pythonize ,arg-symbol) ",") into keyword-pass))
              ("POSITIONAL_OR_KEYWORD"
               (appending `(,name "=" (pythonize ,arg-symbol) ",") into keyword-pass)))

            (finally
             (return-from %get-arg-list
               (cond
                 ((and rest
                       (or keyword keyword-rest))
                  (signal 'both-positional-and-keyword-rest))
                 (keyword-rest
                  `((,@positional ,@(when optional `(&optional ,@optional))
                                  &rest ,keyword-rest
                                  ,@(when keyword `(&key ,@keyword &allow-other-keys)))
                    (() (apply #'raw-pyeval ,fullname "("
                               ,@positional-pass
                               ,@keyword-pass
                               (pythonize-kwargs
                                (loop :for (symbol default) :in ',keyword
                                      :do (remf ,keyword-rest
                                                (find-symbol (symbol-name symbol)
                                                             :keyword))
                                      :finally (return ,keyword-rest)))))))
                 (keyword
                  `((,@positional ,@(when optional `(&optional ,@optional))
                                  ,@(when keyword `(&key ,@keyword)))
                    (() (raw-pyeval ,fullname "("
                                    ,@positional-pass
                                    ,@keyword-pass
                                    ")"))))
                 (rest
                  `((,@positional ,@(when optional `(&optional ,@optional)) &rest ,rest)
                    (() (raw-pyeval ,fullname "("
                                    ,@positional-pass "*" (pythonize ,rest) ")"))))
                 (t
                  `((,@positional ,@(when optional `(&optional ,@optional)))
                    (() (raw-pyeval ,fullname "("
                                    ,@positional-pass
                                    ")")))))))))
      
      (default-arg-list-necessary (c)
        (declare (ignore c))
        `((&rest args)
          (() (apply #'pycall ,fullname args)))))))
