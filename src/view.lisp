(in-package :cl-user)
(defpackage com.ahungry.view
  (:use :cl)
  (:import-from :com.ahungry.config
                :*template-directory*)
  (:import-from :caveman2
                :*response*)
  ;; (:import-from :clack.response
  ;;               :headers)
  (:import-from :cl-emb
                :*escape-type*
                ;;:*case-sensitivity*
                :*function-package*
                :execute-emb)
  (:import-from :datafly
                :encode-json)
  (:export :*default-layout-path*
           :*default-layout-env*
           :render
           :render-json
           :with-layout))
(in-package :com.ahungry.view)

(defvar *default-layout-directory* #P"layouts/")
(defvar *default-layout-path* #P"default.tmpl")

(defvar *default-layout-env* '())

(defun render (template-path &optional env)
  (let ((emb:*escape-type* :html))
        ;;(emb:*case-sensitivity* nil))
    (emb:execute-emb
     (merge-pathnames template-path
                      *template-directory*)
     :env env)))

(defun render-json (object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (encode-json object))

(defmacro with-layout ((&rest env-for-layout) &body body)
  (let ((layout-path (merge-pathnames *default-layout-path*
                                      *default-layout-directory*)))
    (when (pathnamep (car env-for-layout))
      (setf layout-path (pop env-for-layout)))

    `(let ((emb:*escape-type* :html))
           ;;(emb:*case-sensitivity* nil))
       (emb:execute-emb
        (merge-pathnames ,layout-path
                         *template-directory*)
        :env (list :content (progn ,@body)
                   ,@env-for-layout
                   *default-layout-env*)))))

;; Define functions that are available in templates.
(import '(com.ahungry.config:config
          com.ahungry.config:appenv
          com.ahungry.config:developmentp
          com.ahungry.config:productionp
          caveman2:url-for)
        emb:*function-package*)
