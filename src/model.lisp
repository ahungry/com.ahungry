(in-package :cl-user)
(defpackage com.ahungry.model
  (:use :cl
        :caveman2
        :com.ahungry.config
        :com.ahungry.view
        :com.ahungry.db
        :datafly
        :sxql
        :split-sequence
        :bordeaux-threads
        :local-time
        :glyphs)
  (:export
   :*pages*
   :get-auctions
   :get-items
   :get-matching-pages
   :get-page-content
   :get-item-loosely-by-name
   :get-ad
   :populate-eq-item-index
   :populate-all-eq-items
   :populate-pages
   :price-all-eq-items
   ))
(in-package :com.ahungry.model)

;;
;; Application

(in-readtable glyphs:syntax)

;; Pull out the actual numeric for our passed in item
;; Expect input like: listing | item
(ƒ price-extract ~(format nil "(?i).*(~a.*?)([0-9kp]+?)([^0-9kp]|$).*" αb)~ → |"\\2"|)

;; turn 1k into 1000
(ƒ expand-ks ~"(?i)k+"~ → |"000"| α → α)

(ƒ strip-non-numbers ~"\\D"~ → |""| α → α)

;; Pull out a nicely formatted price
(ƒ price-parse (price-extract α αb) → (strip-non-numbers (expand-ks (price-extract α αb))))

(defparameter *pages* nil)

(defun populate-pages ()
  "Set up the various site links - called once in web"
  (unless *pages*
    (setf *pages*
          (with-connection (db)
            (retrieve-all
             (select :title
                     (from :page)))))))

(ƒ first-nest
   ~"(.*?)/.*"~ → |"\\1"|
   α → α)

(defun get-matching-pages (uri)
  "Pull out pages that match the start of the uri"
  (ψ
   (remove
    ""
    (remove-duplicates
     (loop for page in *pages*
        when (string= uri (getf page :title)
                      :end2 (min (length (getf page :title))
                                 (length uri)))
        collect (first-nest (subseq (getf page :title) (length uri))))
     :test #'string=)
    :test #'string=) α → (list :title α :uri (format nil "~a~a" uri α))))

(defun get-page-content (uri)
  "Pull out page content for a URI"
  (getf (with-connection (db)
          (retrieve-one
           (select :*
             (from :page)
             (where (:= :title uri))))) :content))

(in-readtable :common-lisp)

(defun build-like-clause (type regex)
  "Given a type and a regex, build an appropriate LIKE statement"
  (let ((clause "%")) ;; start with something that will always work
    (when (member type '("wtb" "wts") :test #'string=)
      (setf clause (format nil "~a~a%" clause type)))
    (when regex
      (setf clause (format nil "~a~a%" clause regex)))
    clause))

(defparameter *query-auctions-cache* (make-hash-table :test #'equal))

(defun make-cache-key (regex server-color)
  "Create a unique enough cache key reference."
  (format nil "~a-~a" server-color regex))

(defun query-auctions-with-cache (&key (limit 100) (type nil) (regex nil) (server-color nil))
  "Provide a fast way to query out repeat queries and avoid the DB"
  (let ((epoch (or (gethash "epoch" *query-auctions-cache*) 0)))
    (when (> (- (get-universal-time) epoch) 60)
      (setf *query-auctions-cache* (make-hash-table :test #'equal)
            (gethash "epoch" *query-auctions-cache*) (get-universal-time)))
    (let* ((table (or (gethash type *query-auctions-cache*)
                      (make-hash-table :test #'equal)))
           (key (make-cache-key regex server-color))
           (value (gethash key table)))
      (if value value
          (let ((value (query-auctions :limit limit :type type :regex regex :server-color server-color)))
            (setf (gethash key table) value
                  (gethash type *query-auctions-cache*) table)
            value)))))

(defparameter *index-update-list* (list)
  "Keep track of auctions that need their indexes updated")

(defun take-ids (ids &key (count 25))
  "Given a string of ids: `1,2,3,4,5', return just a segment of them
in descending order, in a list."
  (let ((id-list (sort (mapcar #'parse-integer (split-sequence #\, ids)) #'>)))
    (subseq id-list 0 (min count (length id-list)))))

(defun get-server-color (s)
  "Turn a server color into one that matches our db values.
The database should only ever have the values 'green', 'blue', or 'teal'."
  (if s
      (string-downcase s)
      "blue"))

(defun query-auctions-with-index (name &key type server-color)
  "Query via strict item names using our index lookup.  This ends up being
faster than the cache option when querying many different name types at
a frequent rate (similar to how the site is used)."
  (with-connection (db)
    (let ((ids (retrieve-one (select :ids (from :eq_item_index)
                                     (where (:= :name name))))))
      (when (stringp (getf ids :ids))
        ;; For now, lets not do this until we see whats killing the server
        ;;(unless (member name *index-update-list* :test #'equal)
        ;;(push name *index-update-list*))
        ;;(print (getf ids :ids))
        (if type
            (retrieve-all
             (select :* (from :|eqAuction|)
                     (where
                      (:and (:in :id (take-ids (getf ids :ids)))
                            (:= :server (get-server-color server-color))
                            (:like :listing (build-like-clause type name))))
                     (order-by (:desc :date))))
            (retrieve-all
             (select :* (from :|eqAuction|)
                     (where
                      (:and
                       (:= :server (get-server-color server-color))
                       (:in :id (take-ids (getf ids :ids)))))
                     (order-by (:desc :date)))))))))

(defun yyyy-mm-dd (ut)
  "Change a universal-time into the mysql format"
  (with-decoded-timestamp (:year y :month m :day d)
      (universal-to-timestamp ut)
    (format nil "~a-~2,'0d-~2,'0d" y m d)))

(defun query-auctions (&key (limit 100) (type nil) (regex nil) (max-days 2) (server-color nil))
  "Pull out the listings, limit is the total to show"
  (with-connection (db)
    (let ((clause (build-like-clause type regex)))
      (if (not (equal "%" clause))
          (retrieve-all
           (select :*
             (from :|eqAuction|)
             (where
              (:and (:like :listing clause)
                    (:= :server (get-server-color server-color))
                    (:> :date (yyyy-mm-dd (- (get-universal-time) (* 60 60 24 max-days))))))
             (order-by (:desc :date))
             (limit limit)))
          (retrieve-all
           (select :*
             (from :|eqAuction|)
             (where (:= :server (get-server-color server-color)))
             (order-by (:desc :date))
             (limit limit)))))))

(defun query-item-loosely-by-name (name)
  "Pull a single item out of the database"
  (with-connection (db)
    (retrieve-one
     (select (:|eqItems|.name :itemclass :weight :norent :nodrop :size :slots :price :icon
                    :cr :dr :pr :mr :fr :astr :asta :aagi :adex :acha :aint :awis :hp
                    :mana :endur :ac :classes :races :deity :bardtype :bardvalue :magic
                    :lore :delay :range :damage :bagtype :bagslots :bagsize :attack :regen
                    :haste :clickeffect :clicktype :clicklevel2 :clicklevel :maxcharges :casttime
                    :proceffect :proctype :proclevel2 :proclevel
                    :price_week :price_month :price_all)
       (from :|eqItems|)
       (left-join :eq_item_index :on (:= :|eqItems|.name :eq_item_index.name))
       (where (:and (:like :|eqItems|.name (format nil "%~a%" name))
                    (:= :p99 1)))
       (limit 1)))))

(defun time-ago-format (seconds)
  "Display a readable format for how long ago it occurred"
  (let* ((minutes (round (/ seconds 60)))
         (hours (round (/ minutes 60)))
         (days (round (/ hours 60))))
    (cond ((>= days 1) (format nil "~a days ago" days))
          ((>= hours 1) (format nil "~a hours ago" hours))
          ((>= minutes 1) (format nil "~a minutes ago" minutes))
          (t (format nil "~a seconds ago" (max seconds 0))))))

;; Daylight savings time can mess up our app, so, still, update twice a year.
;; In spring, set to 4, in fall, set to 5.
(defparameter *dst-offset* 5)

(defun get-auctions (&key (limit 100) (type nil) (regex nil) (server-color nil))
  "Query the auctions and apply some additional fields to them"
  (let ((limit (if (stringp limit) (parse-integer limit) limit)))
    (ψ (remove-duplicates
        (if regex
            ;; Try to use the index, use the cache if not
            (append
             (query-auctions-with-cache :regex regex :type type :server-color server-color)
             (query-auctions-with-index regex :type type :server-color server-color))
            ;; Unless we had no regex, then default to the cache
            (query-auctions-with-cache :limit limit
                                       :type type
                                       :server-color server-color
                                       :regex regex))
        :test #'string= :key (lambda (r) (getf r :listing)))
       α → (append
            (list :time-ago
                  (time-ago-format
                   (- (- (get-universal-time) (* 3600 *dst-offset*)) ;; EDT offset
                      (getf α :date))))
            α))))

(defun get-items ()
  "Pull out a list of all p99 items"
  (with-connection (db)
    (retrieve-all
     (select (:name)
       (from :|eqItems|)
       (where (:or (:= :p99 1)
                   (:like :name "Spell:%")))
       (order-by :name)))))

(defun get-auction-listing-ids (name &key (max-days 2))
  "For a given item name, pull out the auction listing ids"
  (let* ((matches (query-auctions :regex name :max-days max-days))
         (ids (mapcar (lambda (match) (getf match :id)) matches)))
    ids))

(defun empty (i)
  "If the item is either nil or 0."
  (or (null i)
      (eq 0 i)))

(defun remove-by-date (matches seconds)
  "Takes in a list of plists that has the date column, and filters by
the time."
  (remove-if (lambda (m)
               (let ((seconds-ago (- (get-universal-time) (getf m :date))))
                 (> seconds-ago seconds))) matches))

(defun get-price-average (name &key (max-days 7))
  "Build a price average for the given item."
  (let* ((matches (query-auctions-with-index name))
         (seconds (* max-days 60 60 24))
         (filtered (remove-by-date matches seconds))
         (listings (mapcar (lambda (match) (getf match :listing)) filtered))
         (sum-list (remove-if #'empty (mapcar (lambda (p) (parse-integer p :junk-allowed t))
                                             (remove-if #'null (ψ listings α → (price-parse α name)))))))
    (when (> (length sum-list) 0)
      (float (/ (reduce #'+ sum-list) (length sum-list))))))

(defun eq-item-index-exists-p (name)
  "See if we have an index entry for the name or not"
  (with-connection (db)
    (retrieve-one
     (select :name
       (from :eq_item_index)
       (where (:= :name name))))))

(defun populate-eq-item-index (name)
  "In our database, populate item names/indexes for all available p99 items."
  (let* ((item-name name)
         (item-ids (get-auction-listing-ids item-name :max-days 3600)))
    (if (eq-item-index-exists-p item-name)
        (with-connection (db)
          (execute
           (update :eq_item_index
             (set= :ids (format nil "~{~a~^,~}" item-ids))
             (where (:= :name item-name)))))
        (with-connection (db)
          (execute
           (insert-into :eq_item_index
             (set= :name item-name
                   :ids (format nil "~{~a~^,~}" item-ids))))))
    (print item-name)))

(defun price-eq-item-index (name)
  "Set the prices for an item that exist in the index."
    (let* ((price-all (or (get-price-average name :max-days 3600) 0))
           (price-month (or (get-price-average name :max-days 30) price-all))
           (price-week (or (get-price-average name :max-days 7) price-month)))
      (with-connection (db)
        (execute
         (update :eq_item_index
           (set= :price_week price-week
                 :price_month price-month
                 :price_all price-all)
           (where (:= :name name)))))))

(defun populate-eq-item-index-via-queue ()
  "Run the index updates based on queued item lookups"
  (mapcar #'populate-eq-item-index
          (remove-duplicates *index-update-list* :test #'equal))
  (setf *index-update-list* (list)))

(defun populate-threader ()
  "Run the population stuff in an isolated thread"
  (populate-eq-item-index-via-queue)
  (sleep 30)
  (populate-threader))

;;(make-thread (lambda () (populate-threader)) :name "Population Thread")

(defun populate-all-eq-items ()
  "Do the population for all items."
  (let ((items (get-items)))
    (mapcar (lambda (item)
              (populate-eq-item-index (getf item :name))
              (price-eq-item-index (getf item :name)))
            items)))

(defun price-all-eq-items ()
  "Do the pricing for all items."
  (let ((items (get-items)))
    (mapcar (lambda (item)
              (print item)
              (price-eq-item-index (getf item :name)))
            items)))

(defmacro c. (var string)
  "Concatenates string onto the end of var"
  `(setf ,var (format nil "~a~a" ,var ,string)))

(defun get-spell (spell-id)
  "Return the spell queried via spell-id"
  (let ((rows (with-connection (db)
                (retrieve-all
                 (select (:name)
                   (from :|eqSpells|)
                   (where (:= :id spell-id))
                   (order-by :name))))))
    (when rows (getf (car rows) :name))))

(defparameter *masks-class*
  '(:war :clr :pal :rng :shd :dru :mnk :brd
    :rog :shm :nec :wiz :mag :enc))

(defparameter *masks-race*
  '(:hum :bar :eru :elf :hie :def :hef :dwf
    :trl :ogr :hfl :gnm :iks))

(defparameter *masks-slot*
  '(:hum :ear :head :face :ear :neck :shoulders
    :arms :back :wrist :wrist :range :hands :primary
    :secondary :fingers :fingers :chest :legs :feet
    :waist))

(defun get-matching-bit-masks (bit-mask masks)
  "Iterate across some masks and collect matching ones"
  (loop for mask in masks
     for i from 0
     for b = (expt 2 i) ;; Base 2 to an exponent goes 1, 2, 4, 8...
     when (> (logand b bit-mask) 0)
     collect mask))

(defun get-formatted-item-tooltip (item)
  "Requires an item (plist of a database row) and formats it as necessary"
  (let ((tooltip "")
        (loretip "")
        (hastetip "")
        (regentip "")
        (delay "")
        (dps "")
        (stats "")
        (resists "")
        (proc "")
        (click "")
        (weight "")
        (size "")
        (class "")
        (race "")
        (slot "")
        (pweektip "")
        (pmonthtip "")
        (palltip ""))

    ;; Clean up the item to make sure nils are 0s for comparison
    (setf item (loop for slot in item collect (if (equal nil slot) 0 slot)))

    (when (eql (getf item :lore) 0) (setf (getf item :lore) ""))

    ;; Set up general display items (MAGIC, LORE, REGEN, HASTE)
    (when (> (getf item :magic) 0) (c. tooltip "MAGIC ITEM "))
    (when (> (length (getf item :lore)) 0) (c. tooltip "LORE ITEM "))
    (unless (string= (getf item :name)
                     (getf item :lore))
      (c. loretip (format nil "~%Lore: ~a" (getf item :lore))))
    (when (> (getf item :haste) 0) (c. hastetip (format nil "Haste: ~a%" (getf item :haste))))
    (when (> (getf item :regen) 0) (c. regentip (format nil "Regen: +~a" (getf item :regen))))

    ;; Set up stats
    (when (> (getf item :delay) 0) (c. delay (format nil "Atk Delay: ~a " (getf item :delay))))
    (when (> (getf item :damage) 0) (c. dps (format nil "DMG: ~a " (getf item :damage))))
    (when (> (getf item :ac) 0) (c. dps (format nil "AC: ~a " (getf item :ac))))
    (when (> (getf item :range) 0) (c. dps (format nil "Range: ~a " (getf item :range))))

    ;; Basic stats
    (when (> (getf item :astr) 0) (c. stats (format nil "STR: +~a " (getf item :astr))))
    (when (> (getf item :asta) 0) (c. stats (format nil "STA: +~a " (getf item :asta))))
    (when (> (getf item :aagi) 0) (c. stats (format nil "AGI: +~a " (getf item :aagi))))
    (when (> (getf item :adex) 0) (c. stats (format nil "DEX: +~a " (getf item :adex))))
    (when (> (getf item :awis) 0) (c. stats (format nil "WIS: +~a " (getf item :awis))))
    (when (> (getf item :aint) 0) (c. stats (format nil "INT: +~a " (getf item :aint))))
    (when (> (getf item :acha) 0) (c. stats (format nil "CHA: +~a " (getf item :acha))))
    (when (> (getf item :hp) 0) (c. stats (format nil "HP: ~a " (getf item :hp))))
    (when (> (getf item :mana) 0) (c. stats (format nil "MANA: ~a " (getf item :mana))))

    ;; Now do resists
    (when (> (getf item :cr) 0) (c. resists (format nil "SV COLD: ~a " (getf item :cr))))
    (when (> (getf item :dr) 0) (c. resists (format nil "SV DISEASE: ~a " (getf item :dr))))
    (when (> (getf item :pr) 0) (c. resists (format nil "SV POISON: ~a " (getf item :pr))))
    (when (> (getf item :mr) 0) (c. resists (format nil "SV MAGIC: ~a " (getf item :mr))))
    (when (> (getf item :fr) 0) (c. resists (format nil "SV FIRE: ~a " (getf item :fr))))

    ;; Now do proc
    (when (> (getf item :proceffect) 0) (c. proc (format nil "Proc: ~a " (get-spell (getf item :proceffect)))))

    ;; Now clicky
    (when (> (getf item :clickeffect) 0) (c. click (format nil "Clicky: ~a " (get-spell (getf item :clickeffect)))))
    (when (> (getf item :casttime) 0) (c. click (format nil "(~a seconds) " (round (/ (getf item :casttime) 1000)))))

    ;; Weight and size
    (when (> (getf item :weight) 0) (c. weight (format nil "~a  " (round (/ (getf item :weight) 10)))))
    (setf size (funcall (λ 0 → "SMALL" 1 → "MEDIUM" 2 → "LARGE" 3 → "GIANT" α → nil) (getf item :size)))

    ;; Set up the slot type values
    (let* ((matches (get-matching-bit-masks (getf item :classes) *masks-class*))
           (match-string (mapcar #'string-upcase matches)))
      (c. class (format nil "~{~a~^ ~}" match-string)))

    (let* ((matches (get-matching-bit-masks (getf item :races) *masks-race*))
           (match-string (mapcar #'string-upcase matches)))
      (c. race (format nil "~{~a~^ ~}" match-string)))

    (let* ((matches (get-matching-bit-masks (getf item :slots) *masks-slot*))
           (match-string (mapcar #'string-upcase matches)))
      (c. slot (format nil "~{~a~^ ~}" match-string)))

    ;; Set the price tooltips
    (when (> (getf item :price-week) 0) (c. pweektip (format nil "~,2f" (getf item :price-week))))
    (when (> (getf item :price-month) 0) (c. pmonthtip (format nil "~,2f" (getf item :price-month))))
    (when (> (getf item :price-all) 0) (c. palltip (format nil "~,2f" (getf item :price-all))))

    ;; Assign values to the item row
    (setf (getf item :tooltip) tooltip
          (getf item :loretip) loretip
          (getf item :hastetip) hastetip
          (getf item :regentip) regentip
          (getf item :delaytip) delay
          (getf item :dpstip) dps
          (getf item :statstip) stats
          (getf item :resiststip) resists
          (getf item :proctip) proc
          (getf item :clicktip) click
          (getf item :weighttip) weight
          (getf item :sizetip) size
          (getf item :classtip) class
          (getf item :racetip) race
          (getf item :slottip) slot
          (getf item :pweektip) pweektip
          (getf item :pmonthtip) pmonthtip
          (getf item :palltip) palltip
          )

    ;; Lastly, change any empty strings to nil so we can check in template
    (setf item (loop for val in item
                  collect (if (stringp val)
                              (if (> (length val) 0) val nil)
                              val)))

    item))

(defun name-segment (name)
  "Given a name, drop the first and last fifth of it"
  (let* ((length (length name))
         (fifth (round (/ length 5))))
    (subseq name fifth (- length fifth))))

(defparameter *queried-items-cache* (make-hash-table :test #'equal))

(defun get-item-loosely-by-name (name)
  "Get an item by it's name, format for display on the item tool tip"
  (when (> (hash-table-count *queried-items-cache*) 1000)
    (setf *queried-items-cache* (make-hash-table :test #'equal)))
  (let ((cached-item (gethash name *queried-items-cache*)))
    (if cached-item cached-item
        (let ((item (query-item-loosely-by-name name)))
          (unless item
            (setf item (query-item-loosely-by-name (name-segment name))))
          (when item
            (let ((formatted-item (get-formatted-item-tooltip item)))
              (setf (gethash name *queried-items-cache*) formatted-item)))))))

(defun get-ad (ad-id)
  "Gotta eat somehow ... put your ads in ads/<int> type filename
and you can use this to load them up and serve in your pages"
  (let* ((file-path (format nil "~a~a/" (user-homedir-pathname) "src/lisp/com.ahungry/ads"))
         (file-name (merge-pathnames file-path ad-id)))
    (with-open-file (s file-name)
      (let ((lines
             (loop for line = (read-line s nil 'eof)
                until (eq line 'eof)
                collect line)))
        (format nil "~{~a~%~}" lines)))))
