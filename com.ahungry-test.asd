(in-package :cl-user)
(defpackage com.ahungry-test-asd
  (:use :cl :asdf))
(in-package :com.ahungry-test-asd)

(defsystem com.ahungry-test
  :author "Matthew Carter"
  :license "AGPLv3"
  :depends-on (:com.ahungry
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "com.ahungry"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
