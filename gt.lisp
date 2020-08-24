;;;; gt.lisp --- GrammaTech Common Lisp utilities core package
;;;;
;;;; Copyright (C) 2020 GrammaTech, Inc.
;;;;
;;;; This code is licensed under the MIT license. See the LICENSE.txt
;;;; file in the project root for license terms.
;;;;
;;;; This project is sponsored by the Office of Naval Research, One
;;;; Liberty Center, 875 N. Randolph Street, Arlington, VA 22203 under
;;;; contract # N68335-17-C-0700.  The content of the information does
;;;; not necessarily reflect the position or policy of the Government
;;;; and no official endorsement should be inferred.
(uiop/package:define-package :gt
    (:nicknames :gt/gt)
  (:use-reexport :common-lisp :alexandria :serapeum :closer-mop
                 :trivia :cl-ppcre :trivia.ppcre
                 :iterate :gt/fset :fset :gmap :split-sequence
                 :bordeaux-threads
                 :functional-trees
                 :named-readtables :curry-compose-reader-macros)
  (:shadow :mapconcat :~> :~~>)  ; Shadow serapeum arrows & mapconcat.
  (:shadow :lines)                      ; We wrap in a defgeneric.
  (:shadow :exe)                        ; This isn't generically useful.
  (:shadow :equal?)                     ; We'd rather this be generic.
  (:shadowing-import-from :common-lisp
                          :map)         ; Shadow fset:map.
  (:shadowing-import-from :serapeum
                          :tuple)       ; Shadow fset:tuple.
  (:shadowing-import-from :alexandria
                          :compose)     ; Shadow fset:compose.
  (:shadowing-import-from :trivia
                          :alist)       ; Shadow fset:alist.
  (:shadowing-import-from :iterate
                          ;; Shadow serapeum macros.
                          :summing :collecting :sum :in)
  (:shadowing-import-from :closer-mop
                          :standard-method
                          :standard-class
                          :standard-generic-function
                          :defmethod
                          :defgeneric)
  (:shadowing-import-from :functional-trees :mapcar :mapc)
  (:shadowing-import-from :fset
                          :@
                          :unionf :appendf :with :removef :size :complement
                          ;; Shadowed type/constructor names.
                          :set
                          ;; Shadowed sequence operations.
                          :first :subseq :reverse :sort :stable-sort
                          :reduce
                          :find :find-if :find-if-not
                          :count :count-if :count-if-not
                          :position :position-if :position-if-not
                          :remove :remove-if :remove-if-not
                          :substitute :substitute-if :substitute-if-not
                          :some :every :notany :notevery
                          ;; Shadowed sequence operations from serapeum.
                          :concat :range :partition :filter)
  (:shadowing-import-from :gt/fset
                          :last :lastcar
                          :union :intersection :set-difference)
  (:shadowing-import-from :cl-ppcre
                          :scan)        ; Shadow serapeum:scan.
  (:export :mapconcat
           :negative-infinity
           :positive-infinity
           :infinity
           :parse-number
           :parse-numbers
           :equal?
           :withf :lessf
           :in-seq :index-of-seq
           :in-set
           :in-map
           :in-iterator
           :in-map-iterator
           :in-tree
           :set-collect
           :seq-collect
           :map-collect))
;;; NOTE: *Consider* including Generic-cl less its new seq. stuff.
(in-package :gt)

(define-modify-macro withf (&rest item-or-tuple) with
  "Modify macro for `fset:with'.")
(define-modify-macro lessf (&rest item-or-tuple) less
  "Modify macro for `fset:less'.")

;;; A poor approximation of `serapium:@'.
(defmethod lookup ((table hash-table) key)
  (gethash key table))

(defgeneric mapconcat (function collection separator &key stream)
  (:documentation "Build a string by mapping FUNCTION over COLLECTION.
Separate each value with SEPARATOR.
An extension of `serapeum:mapconcat' to include fset collections.")
  (:method (function (sequence sequence) separator &key stream)
    (serapeum:mapconcat function sequence separator :stream stream))
  (:method (function (seq seq) separator &key stream)
    (let ((function (coerce function 'function))
          (i 0)
          (ult (1- (size seq))))
      (with-string (stream stream)
        (do-seq (element seq)
          (write-string (funcall function element) stream)
          (unless (= (prog1 i (incf i)) ult)
            (write-string separator stream)))))))

(defvar negative-infinity
  #+sbcl
  sb-ext:double-float-negative-infinity
  #+ccl
  ccl::double-float-negative-infinity
  #+allegro
  excl:*infinity-double*
  #+ecl
  ext:long-float-negative-infinity
  #-(or ecl sbcl ccl allegro)
  (error "")
  "Most negative number.")

(defvar positive-infinity
  #+sbcl
  sb-ext:double-float-positive-infinity
  #+ccl
  ccl::double-float-positive-infinity
  #+allegro
  excl:*infinity-double*
  #+ecl
  ext:long-float-positive-infinity
  #-(or ecl sbcl ccl allegro)
  (error "")
  "Most positive number.")

(defvar infinity positive-infinity
  "Most positive number.")

(defmethod print-object ((obj (eql negative-infinity)) stream)
  (if *print-readably* (call-next-method) (format stream "negative-infinity")))

(defmethod print-object ((obj (eql positive-infinity)) stream)
  (if *print-readably* (call-next-method) (format stream "positive-infinity")))

(defmethod print-object ((obj (eql infinity)) stream)
  (if *print-readably* (call-next-method) (format stream "positive-infinity")))

(define-condition parse-number (error)
  ((text :initarg :text :initform nil :reader text))
  (:report (lambda (condition stream)
             (format stream "Can't parse ~a as a number" (text condition)))))

(defun parse-number (string)
  "Parse the number located at the front of STRING or return an error."
  (let ((number-str
          (or (multiple-value-bind (whole matches)
                  (scan-to-strings
                   "^(-?.?[0-9]+(/[-e0-9]+|\\.[-e0-9]+)?)([^\\./A-Xa-x_-]$|$)"
                   string)
                (declare (ignorable whole))
                (when matches (aref matches 0)))
              (multiple-value-bind (whole matches)
                  (scan-to-strings "0([xX][0-9A-Fa-f]+)([^./]|$)"
                                   string)
                (declare (ignorable whole))
                (when matches (concatenate 'string "#" (aref matches 0)))))))
    (unless number-str
      (make-condition 'parse-number :text string))
    (read-from-string number-str)))

(defun parse-numbers (string &key (radix 10) (delim #\Space))
  (mapcar #'(lambda (num) (parse-integer num :radix radix))
          (split-sequence delim string :remove-empty-subseqs t)))


;;; Some functions become generic functions for extensibility.
(defgeneric lines (thing)
  (:documentation "A list of lines in THING.")
  (:method ((string string)) (serapeum:lines string)))

(defun compare/iterator (col1 col2)
  "Compare two FSet collections, known to be of the same size, using FSet's iterator protocol."
  (fbind ((iterator (iterator col1)))
    (iter (while (iterator :more?))
          (mvlet* ((key1 val1 (iterator :get))
                   (val2 val2? (lookup col2 key1)))
            (always (and val2? (equal? val1 val2)))))))

(defgeneric equal? (a b)
  (:documentation "Generic equality designed to descend into structures.")
  (:method :around ((a t) (b t)) (or (eql a b) (call-next-method)))
  (:method ((a t) (b t)) (equalp a b))
  (:method ((a number) (b number)) (= a b))
  (:method ((a character) (b character)) (char= a b))
  (:method ((a string) (b string)) (string= a b))
  (:method :around ((a collection) (b collection))
    (and (= (fset:size a) (fset:size b))
         (call-next-method)))
  (:method ((a collection) (b collection))
    nil)
  (:method ((a seq) (b seq))
    (every #'equal? a b))
  (:method ((a set) (b set))
    (every #'equal? a b))
  (:method ((a fset:map) (b fset:map))
    (compare/iterator a b))
  (:method ((a bag) (b bag))
    (compare/iterator a b))
  (:method ((a fset:tuple) (b fset:tuple))
    (compare/iterator a b))
  (:method ((a sequence) (b sequence))
    (and (length= a b)
         (every #'equal? a b)))
  (:method ((a cons) (b cons)) (tree-equal a b :test #'equal?))
  (:method ((a array) (b array))
    (and (= (array-total-size a)
            (array-total-size b))
         (equal (array-dimensions a)
                (array-dimensions b))
         (iter (for i below (array-total-size a))
           (always (equal? (row-major-aref a i)
                           (row-major-aref b i))))))
  (:method ((a hash-table) (b hash-table))
    (and (= (hash-table-count a) (hash-table-count b))
         (eql (hash-table-test a) (hash-table-test b))
         (every (lambda (key) (equal? (gethash key a) (gethash key b)))
                (hash-table-keys a))))
  (:method ((a standard-object) (b standard-object))
    (and (eq (class-of a) (class-of b))
         (every (lambda (slot)
                  (let ((name (slot-definition-name slot)))
                    (equal? (slot-value a name) (slot-value b name))))
                (class-slots (class-of a))))))


;;; Iterate drivers for FSet and functional-trees.

(defclause-sequence in-seq index-of-seq
  :access-fn 'fset:lookup
  :size-fn 'fset:size
  :element-type t
  :element-doc-string "Elements of anything that implements fset:size and fset:lookup."
  :index-doc-string "Indices of anything that implements fset:size and fset:lookup.")

(defmacro-driver (for node in-iterator it)
  "Driver for FSet iterators."
  (let ((for (if generate 'generate 'for)))
    (with-unique-names (next v present?)
      `(progn
         (iterate:with ,next = (ensure-function ,it))
         (,for ,node next
               (multiple-value-bind (,v ,present?)
                   (funcall ,next :get)
                 (if ,present?
                     ,v
                     (terminate))))))))

(defmacro-driver (for node in-map-iterator it)
  "Driver for FSet map iterators."
  (let ((for (if generate 'generate 'for)))
    (with-unique-names (next k v present?)
      `(progn
         (iterate:with ,next = (ensure-function ,it))
         (,for ,node next
               (multiple-value-bind (,k ,v ,present?)
                   (funcall ,next :get)
                 (if ,present?
                     (list ,k ,v)
                     (terminate))))))))

(defmacro-driver (for x in-set set)
  "Driver for FSet sets."
  (let ((kwd (if generate 'generate 'for)))
    `(,kwd ,x in-iterator (iterator ,set))))

(defmacro-driver (for node in-tree tree)
  "Driver for functional trees."
  (let ((kwd (if generate 'generate 'for)))
    `(,kwd ,node in-iterator (iterator ,tree))))

(defmacro-driver (for node in-map map)
  "Driver for FSet maps. Each key-value pair is returned as
a two-element list."
  (let ((kwd (if generate 'generate 'for)))
    `(,kwd ,node in-map-iterator (iterator ,map))))

(defmacro-driver (for elt in-bag bag)
  "Driver for FSet bags."
  (let ((kwd (if generate 'generate 'for)))
    `(,kwd ,elt in-iterator (iterator ,bag))))

(defmacro-driver (for elt in-bag-pairs bag)
  "Driver for FSet bags."
  (let ((kwd (if generate 'generate 'for)))
    `(,kwd ,elt in-map-iterator (iterator ,bag :pairs? t))))

(defmethod iterator (node &key)
  "An FSet iterator for functional trees."
  (let ((stack (list node)))
    (lambda (arg)
      (ecase arg
        (:done? (null stack))
        (:more? (not (null stack)))
        (:get
         (if (null stack)
             (values nil nil)
             (let* ((node (pop stack))
                    (children
                     (if (typep node 'node)
                         (cl:remove-if-not (of-type 'node)
                                           (children node))
                         nil)))
               (setf stack (append children stack))
               (values node t))))))))

(defmacro-clause (set-collect x &optional into var)
    `(reducing ,x by #'fset:with into ,var initial-value (empty-set)))

(defmacro-clause (seq-collect x &optional into var)
    `(reducing ,x by #'fset:with-last into ,var initial-value (empty-seq)))

(defsubst apply-with (map kv)
  (apply #'fset:with map kv))

(defmacro-clause (%map-collect kv &optional into var)
    `(reducing ,kv by #'apply-with into ,var initial-value (empty-map)))

(defmacro map-collect (k v &rest args)
  `(%map-collect (list ,k ,v) ,@args))
