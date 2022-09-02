(in-package :cl-user)
(defpackage com.ahungry.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*application-root*
           :*static-directory*
           :*template-directory*
           :appenv
           :developmentp
           :productionp))
(in-package :com.ahungry.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :com.ahungry))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))

(defparameter *env-based-database-info*
  (list (list :maindb :mysql
              :database-name "ahungry"
              :host (or (sb-ext:posix-getenv "MYSQL_AHUNGRY_HOST") "0.0.0.0")
              :username (or (sb-ext:posix-getenv "MYSQL_AHUNGRY_USERNAME") "ahungry")
              :password (or (sb-ext:posix-getenv "MYSQL_AHUNGRY_PASSWORD") "ahungry"))))

(defconfig :common
    (list
     :error-log #P"error.log"
     :databases *env-based-database-info*))

(defconfig |development|
    (list :databases *env-based-database-info*
          :debug t))

(defconfig |production|
    (list :databases *env-based-database-info*
          :debug nil))

(defconfig |test|
    (list :databases *env-based-database-info*
          :debug nil))

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (asdf::getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))
