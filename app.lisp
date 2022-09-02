(ql:quickload :com.ahungry)

(defpackage com.ahungry.app
  (:use :cl)
  ;; (:import-from :lack
  ;;               :call)
  (:import-from :lack.builder
                :builder)
  (:import-from :lack.middleware.static
                :*lack-middleware-static*)
  (:import-from :lack.middleware.session
                :*lack-middleware-session*)
  (:import-from :lack.session.state.cookie
                :make-cookie-state)
  ;; (:import-from :lack.middleware.accesslog
  ;;               :*lack-middleware-accesslog*)
  ;; (:import-from :lack.middleware.backtrace
  ;;               :*lack-middleware-backtrace*)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :com.ahungry.web
                :*web*)
  (:import-from :com.ahungry.config
                :config
                :productionp
                :*static-directory*))
(in-package :com.ahungry.app)

(builder
 (:static
  :path (lambda (path)
          (if (ppcre:scan "^(?:/assets/|/projects/|/iosevka/|/blog/|/images/|/css/|/js/|/robot\\.txt$|/favicon.ico$)" path)
              path
              nil))
  :root *static-directory*)
 :accesslog
 (:backtrace :output #P"/tmp/com.ahungry.errors.log")
 ;; (if (productionp)
 ;;     nil
 ;;     (make-instance '*lack-middleware-accesslog*))
 ;; (if (getf (config) :error-log)
 ;;     (make-instance '*lack-middleware-backtrace*
 ;;                    :output (getf (config) :error-log))
 ;;     nil)

 ;; :session
 ;; (*lack-middleware-session*
 ;;  :state (make-instance
 ;;          'lack.session.state.cookie:make-cookie-state ;; *lack-session-state-cookie*
 ;;          :expires 36000))
 ;; Expire cookie in 10 days

 (if (productionp)
     nil
     (lambda (app)
       (lambda (env)
         (let ((datafly:*trace-sql* t))
           (funcall app env)))))
 *web*)
