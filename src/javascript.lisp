(in-package :cl-user)
(defpackage com.ahungry.javascript
  (:use :cl
        :caveman2
        :com.ahungry.config
        :com.ahungry.view
        :com.ahungry.db
        :datafly
        :sxql
        ;;:defjs
        :glyphs))
(in-package :com.ahungry.javascript)

;;
;; Application

;;(defjs hello-world (name)
  ;;(alert (+ "Hello " name)))

;;(defjs init ()
  ;;nil)
