(in-package :cl-user)
(ql-dist:install-dist "http://beta.quicklisp.org/dist/quicklisp/2020-10-16/distinfo.txt"
                      :replace t
                      :prompt nil)
(push :travis *features*)
(push (print (pathname (uiop:getenv "EXOTIC_DIR")))
      ql:*local-project-directories*)
(push #P"~/" ql:*local-project-directories*)
(print (ql:where-is-system "py4cl2"))
(ql:quickload "py4cl2-tests")
(setf (py4cl2:config-var 'py4cl2:pycmd)
      (namestring (uiop:getenv "EXOTIC_DIR_PYTHON")))
(py4cl2:save-config)
(uiop:quit 0)
