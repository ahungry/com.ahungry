(ql:quickload :com.ahungry)

(defpackage com.ahungry.app
  (:use :cl)
  (:import-from :clack
                :call)
  (:import-from :clack.builder
                :builder)
  (:import-from :clack.middleware.static
                :<clack-middleware-static>)
  (:import-from :clack.middleware.session
                :<clack-middleware-session>)
  (:import-from :clack.middleware.accesslog
                :<clack-middleware-accesslog>)
  (:import-from :clack.middleware.backtrace
                :<clack-middleware-backtrace>)
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
 (<clack-middleware-static>
  :path (lambda (path)
          (if (ppcre:scan "^(?:/assets/|/blog/|/images/|/css/|/js/|/robot\\.txt$|/favicon.ico$)" path)
              path
              nil))
  :root *static-directory*)
 (if (productionp)
     nil
     (make-instance '<clack-middleware-accesslog>))
 (if (getf (config) :error-log)
     (make-instance '<clack-middleware-backtrace>
                    :output (getf (config) :error-log))
     nil)

 (<clack-middleware-session>
  :state (make-instance
          'clack.session.state.cookie:<clack-session-state-cookie>
          :expires 36000)) ;; Expire cookie in 10 days

 (if (productionp)
     nil
     (lambda (app)
       (lambda (env)
         (let ((datafly:*trace-sql* t))
           (call app env)))))
 *web*)
