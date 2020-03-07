(in-package :cl-user)
(defpackage com.ahungry
  (:use :cl
        :com.ahungry.model)
  (:import-from :com.ahungry.config
                :config)
  (:import-from :clack
                :clackup)
  (:export :start
           :stop))
(in-package :com.ahungry)

(defvar *appfile-path*
  (asdf:system-relative-pathname :com.ahungry #P"app.lisp"))

(defvar *handler* nil)

(defun start (&rest args &key server port debug &allow-other-keys)
  (declare (ignore server port debug))
  (when *handler*
    (restart-case (error "Server is already running.")
      (restart-server ()
        :report "Restart the server"
        (stop))))
  (com.ahungry.model:populate-pages)
  (setf *handler*
        (apply #'clackup *appfile-path* args)))

(defun stop ()
  (prog1
      (clack:stop *handler*)
    (setf *handler* nil)))
