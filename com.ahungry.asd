(in-package :cl-user)
(defpackage com.ahungry-asd
  (:use :cl :asdf))
(in-package :com.ahungry-asd)

(defsystem com.ahungry
  :version "0.1"
  :author "Matthew Carter"
  :license "AGPLv3"
  :depends-on (:clack
               :caveman
               :caveman2
               :envy
               :cl-ppcre

               ;; HTML Template
               :cl-emb

               ;; for CL-DBI
               :datafly
               :sxql

               ;; For my own things
               :bordeaux-threads
               :local-time
               ;;:defjs
               :cl-json
               :ahungry-fleece
               :glyphs)
  :components ((
                :module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view" "model" "javascript"))
                 (:file "javascript" :depends-on ("config"))
                 (:file "model" :depends-on ("config"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description ""
  :in-order-to ((test-op (load-op com.ahungry-test))))
