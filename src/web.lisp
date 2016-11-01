(in-package :cl-user)
(defpackage com.ahungry.web
  (:use :cl
        :caveman2
        :com.ahungry.config
        :com.ahungry.view
        :com.ahungry.db
        :com.ahungry.model
        :com.ahungry.javascript
        :datafly
        :cl-annot ;; Needed to add this to use @route syntax
        :sxql
        ;;:defjs
        :glyphs)
  (:export :*web*))
(in-package :com.ahungry.web)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

(annot:enable-annot-syntax)

;;
;; Routing rules

;;(defjs:start-websocket-server
 ;;:port 54321
 ;;:server-name (or (sb-ext:posix-getenv "COM_AHUNGRY_SERVER_NAME") "localhost"))

(defroute "/" ()
  (let ((cms-content (get-page-content "/")))
    (with-layout (:title "Welcome to Ahungry"
                         ;;:defjs (defjs:get-loader)
                         :analytics (get-ad "analytics")
                         :pages (get-matching-pages "/"))
      (render #P"index.tmpl"
              (list :cms-content cms-content)))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))

(defroute ("/blog[/]*" :regexp t) ()
  (redirect "/blog/index.html"))

;; Redirect all trailing slash addresses to one sans-slash
(defroute "/*/" (&key splat)
  (redirect (concatenate 'string "/" (car splat))))

@route GET "/about-eqauctions"
(defun about-eqauctions ()
  (with-layout (:title "EQ Auction Logger"
                       ;;:defjs (defjs:get-loader)
                       :analytics nil
                       :pages (get-matching-pages "/about-auctions/"))
    (render #P"about-auctions.tmpl")))

@route GET "/eqauctions"
(defun eqauctions (&key (|limit| 100))
  (let ((auctions (get-auctions :limit |limit|)))
    (with-layout (:title "EQ Auction Logger"
                         ;;:defjs (defjs:get-loader)
                         :analytics nil
                         :pages (get-matching-pages "/eqauctions/"))
      (render #P"auctions.tmpl"
              (list :auctions auctions
                    :ad-one nil
                    :ad-two nil
                    :ad-three nil)))))

@route POST "/eqauctions"
(defun eqauctions-post (&key (|action| "searchAuctions")
                          (|limit| 10)
                          (|regex| nil))
  (let ((auctions (get-auctions :limit |limit|)))
    (render #P"auction-stub.tmpl"
            (list :auctions auctions))))

;;@route GET "/defjs.js"
;;(defun defjs-js ()
  ;;(defjs:page-js))
@route GET "/options.js"
(defun eqauctions-options ()
  (setf (getf (response-headers *response*) :content-type) "text/javascript")
  (render #P"auction-items.tmpl"
          (list :items (get-items))))

(defroute "/action/eq/getItem/*" (&key splat)
  (render #P"item-stub.tmpl"
          (list :item (list (get-item-loosely-by-name (car splat))))))

(defroute "/action/eq/getItems/*/*" (&key splat)
  (let ((auctions (get-auctions :limit 30 :type (car splat) :regex (cadr splat))))
    (render #P"auction-stub.tmpl"
            (list :auctions auctions))))

(defroute "/action/eq/getItems/*" (&key splat)
  (let ((auctions (get-auctions :limit 100 :type (car splat) :regex nil)))
    (render #P"auction-stub.tmpl"
            (list :auctions auctions))))

(defroute "/*" (&key splat)
  (let* ((uri (format nil "/~a" (car splat)))
         (cms-content (get-page-content uri)))
    (unless cms-content (throw-code 404))
    (with-layout (:title (car splat)
                         ;;:defjs (defjs:get-loader)
                         :analytics nil
                         :pages (get-matching-pages (format nil "~a/" uri)))
      (render #P"index.tmpl"
              (list :cms-content cms-content)))))
