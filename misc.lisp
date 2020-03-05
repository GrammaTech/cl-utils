;;;; full.lisp --- generic forensic functions over arbitrary objects
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
(uiop/package:define-package :gt/misc
  (:use :common-lisp :alexandria
        :metabang-bind
        :closer-mop
        :iterate
        :named-readtables
        :curry-compose-reader-macros)
  (:import-from :serapeum :mapconcat :drop-while :take-while :plist-keys)
  (:import-from :uiop/utility :with-muffled-conditions)
  #+sbcl
  (:import-from :sb-introspect :function-lambda-list)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export ;; compilation and introspection
           :*uninteresting-conditions*
           :with-muffled-conditions
           :with-quiet-compilation
           :without-compiler-notes
           :arglist
           ;; macros and macro-related functions
           :if-let*
           :multiple-value-or
           :symbol-cat
           :symbol-cat-in-package
           ;; trees
           :mapt
           :tree-right-length
           :tree-right-walk
           :filter-subtrees
           ;; plists
           :plist-get
           :plist-drop-if
           :plist-drop
           :plist-merge
           ;; alists
           :aget
           :areplace
           :adrop
           :alist-filter
           :getter
           :counts
           ;; probability and statistics
           :position-extremum
           :position-extremum-rand
           :random-bool
           :random-elt-with-decay
           :proportional-pick
           :uniform-probability
           :normalize-probabilities
           :cumulative-distribution
           :un-cumulative-distribution
           :random-pick
           :random-subseq
           :random-sample-with-replacement
           :random-sample-without-replacement
           ;; string replacement
           :apply-replacements
           :replace-all
           ;; lists
           :repeatedly
           :indexed
           :equal-it
           :different-it
           :transpose
           :interleave
           :drop-until
           :take-until
           :pad
           :chunks
           :cartesian
           :cartesian-without-duplicates
           :binary-search
           :levenshtein-distance
           :tails
           :pairs
           :mapcar-improper-list
           ;; hash tables
           :make-thread-safe-hash-table
           :random-hash-table-key))
(in-package :gt/misc)
(in-readtable :curry-compose-reader-macros)


;;;; Compilation and introspection
(defvar *uninteresting-conditions* nil
  "Additional uninteresting conditions for `with-quiet-compilation' to stifle.")

(defmacro with-quiet-compilation (&body body)
  `(let ((*load-verbose* nil)
         (*compile-verbose* nil)
         (*load-print* nil)
         (*compile-print* nil)
         (uiop/lisp-build:*uninteresting-conditions*
          (append *uninteresting-conditions*
                  uiop/lisp-build:*usual-uninteresting-conditions*)))
     ,@body))

(defmacro without-compiler-notes (&body body)
  "Suppress compiler notes from BODY"
  #+sbcl
  `(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun arglist (fname)
  "Return the argument list of FNAME."
  ;; Taken from swank/backend:arglist.
  #+sbcl
  (function-lambda-list fname)
  ;; NOTE: The following is similar, but may return 0 for nil args.
  ;; (sb-kernel:%simple-fun-arglist fname)
  #+ecl
  (multiple-value-bind (arglist foundp)
      (ext:function-lambda-list name)
    (if foundp arglist :not-available))
  #+ccl
  (multiple-value-bind (arglist binding) (let ((*break-on-signals* nil))
                                           (ccl:arglist fname))
    (if binding
        arglist
        :not-available))
  #-(or ecl sbcl ccl)
  (error "Only ECL, SBCL, and CCL."))


;;;; Macros and macro-related functions
(defmacro if-let* (bindings &body (then-form &optional else-form))
  "Creates new bindings, and conditionally executes THEN-FORM or ELSE-FORM.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

Each INITIAL-FORM is executed in turn, and the variable bound to the
corresponding value. INITIAL-FORM expressions can refer to variables
previously bound by the IF-LET*.

Execution of IF-LET* transitions to ELSE-FORM immediately if any
INITIAL-FORM evaluates to NIL.  No bindings are present if ELSE-FORM
is evaluated.  If all INITIAL-FORMs evaluate to true, then THEN-BODY
is executed."
  ;; NOTE: Largely adapted form Alexandria's `when-let*'.
  (with-gensyms  (if-block)
    (let ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                            (list bindings)
                            bindings)))
      (labels ((bind (bindings body)
                 (if bindings
                     `((let (,(car bindings))
                         (when ,(caar bindings)
                           ,@(bind (cdr bindings) body))))
                     `((return-from ,if-block ,body)))))
        `(block ,if-block
           (let (,(car binding-list))
             (when ,(caar binding-list)
               ,@(bind (cdr binding-list) then-form)))
           ,else-form)))))

(defmacro multiple-value-or (&body forms)
  "Evaluates FORM arguments one at time, until the first value returned
by one of the forms is true.  It then returns all the values returned
by evaluating that form.  If none of the forms return a true first
value, we return the values returned by the last form."
  (with-gensyms (values)
    `(let ((,values (multiple-value-list ,(first forms))))
       (if (car ,values)
           (values-list ,values)
           ,(if (rest forms)
                `(multiple-value-or ,@(rest forms))
                `(values-list ,values))))))

(defun symbol-cat (&rest symbols)
  "Return a symbol concatenation of SYMBOLS."
  (intern (string-upcase (mapconcat #'symbol-name symbols "-"))))

(defun symbol-cat-in-package (package &rest symbols)
  "Return a symbol concatenation of SYMBOLS in PACKAGE."
  (intern (string-upcase (mapconcat #'symbol-name symbols "-"))
          package))


;;;; Tree-related functions
(defun tree-right-length (tree &aux (size 1))
  "Return the length of the right spine of TREE."
  (declare (optimize speed))
  (iter (while (consp tree))
        (setf tree (cdr tree))
        (incf (the fixnum size)))
  (the fixnum size))

(defun tree-right-walk (tree)
  "Return the right spine of TREE as a list."
  (declare (optimize speed))
  (if tree
      (if (consp tree)
          (cons (car tree) (tree-right-walk (cdr tree)))
          (list tree))
      nil))

(defun mapt (function tree)
  "Like `mapcar' but TREE is a cons tree instead of a proper list."
  (if (consp tree)
      (cons (mapt function (car tree))
            (mapt function (cdr tree)))
      (funcall function tree)))

(defgeneric filter-subtrees (predicate tree)
  (:documentation "Return a list of subtrees of TREE satisfying PREDICATE.")
  (:method filter-subtrees (predicate (tree list))
    (when (and tree (listp tree))
      (append
       (when (funcall predicate tree) (list tree))
       (when (listp (car tree))
         (filter-subtrees predicate (car tree)))
       (when (listp (cdr tree))
         (filter-subtrees predicate (cdr tree)))))))


;;;; plist functions
(defun plist-get (item list &key (test #'eql) &aux last)
  (loop :for element :in list :do
     (cond
       (last (return element))
       ((funcall test item element) (setf last t)))))

(defun plist-drop-if (predicate list &aux last)
  (nreverse (reduce (lambda (acc element)
                      (cond
                        (last (setf last nil) acc)
                        ((funcall predicate element) (setf last t) acc)
                        (t (cons element acc))))
                    list :initial-value '())))

(defun plist-drop (item list &key (test #'eql))
  (plist-drop-if {funcall test item} list))

(defun plist-merge (plist-1 plist-2)
  "Merge arguments into a single plist with unique keys, prefer PLIST-1 items."
  (append plist-1 (plist-drop-if {member _ (plist-keys plist-1)} plist-2)))


;;;; alist functions
(defun aget (item list &key (test #'eql))
  "Get KEY from association list LIST."
  (cdr (assoc item list :test test)))

(define-compiler-macro aget (&whole whole item list &key (test '#'eql test-p))
  (if (constantp item)
      (if test-p
          `(cdr (assoc ,item ,list :test ,test))
          `(cdr (assoc ,item ,list)))
      whole))

(define-setf-expander aget (item list &key (test ''eql) &environment env)
  (multiple-value-bind (dummies vals stores store-form access-form)
      (get-setf-expansion list env)
    (declare (ignorable stores store-form))
    (let ((store (gensym))
          (cons-sym (gensym)))
      (values dummies
              vals
              `(,store)
              `(let ((,cons-sym (assoc ,item ,access-form :test ,test)))
                 (if ,cons-sym
                     (setf (cdr ,cons-sym) ,store)
                     (prog1 ,store
                       (setf ,access-form (acons ,item ,store ,access-form)))))
              `(aget ,item ,access-form :test ,test)))))

(defun areplace (key val alist &key (test #'eql))
  "Replace the value of KEY in the association list ALIST with VAL."
  (cons (cons key val) (remove key alist :key #'car :test test)))

(defun adrop (drop-keys alist)
  "Remove all keys in DROP-KEYS from alist."
  (remove-if [{member _ drop-keys} #'car] alist))

(defun alist-filter (keep-keys alist)
  "Remove all keys from ALIST except those in KEEP-KEYS."
  (remove-if-not [{member _ keep-keys} #'car] alist))

(defun getter (key)
  "Return a function which gets KEY from an association list."
  (lambda (it) (aget key it)))

(defun counts (list &key (test #'eql) key frac &aux totals)
  "Return an alist keyed by the unique elements of list holding their counts.
Keyword argument FRAC will return fractions instead of raw counts."
  (mapc (lambda (el)
          (if-let (place (assoc el totals :key key :test test))
            (incf (cdr place))
            (push (cons el 1) totals)))
        list)
  (if frac
      (let ((total (reduce #'+ (mapcar #'cdr totals))))
        (mapcar (lambda-bind ((obj . cnt)) (cons obj (/ cnt total))) totals))
      totals))


;;;; Probablity and statistics functions
(defun proportional-pick (list key)
  (let ((raw (reduce (lambda (acc el) (cons (+ el (car acc)) acc))
                     (mapcar key list) :initial-value '(0))))
    (position-if {<= (random (first raw))} (cdr (reverse raw)))))

(defun position-extremum (list predicate key)
  "Returns the position in LIST of the element maximizing KEY."
  (car (extremum (indexed list) predicate :key [key #'second])))

(defun position-extremum-rand (list predicate key)
  "Randomly returns one of position in LIST maximizing KEY."
  (declare (ignorable predicate))
  (warn "`position-extremum-rand' not finished: doesn't use all parameters")
  (let ((scores (mapcar key list)))
    (random-elt (mapcar #'car (remove-if-not [{= (apply #'max scores)} #'second]
                                             (indexed scores))))))

(defun random-bool (&optional bias)
  (> (or bias 0.5) (random 1.0)))

(defun uniform-probability (list)
  (mapcar {cons _ (/ 1.0 (length list))} list))

(defun normalize-probabilities (alist)
  "Normalize ALIST so sum of second elements is equal to 1."
  (let ((total-prob (reduce #'+ (mapcar #'cdr alist))))
    (mapcar (lambda-bind ((key . prob)) (cons key (/ prob total-prob))) alist)))

(defun cumulative-distribution (alist)
  "Cumulative distribution function.
Return an updated version of ALIST in which the cdr of each element is
transformed from an instant to a cumulative probability."
  (nreverse
   (reduce (lambda-bind (acc (value . prob)) (acons value (+ (cdar acc) prob) acc))
           (cdr alist) :initial-value (list (car alist)))))

(defun un-cumulative-distribution (alist)
  "Undo the `cumulative-distribution' function."
  (let ((last 0))
    (mapcar (lambda-bind ((value . prob))
              (prog1 (cons value (- prob last)) (setf last prob)))
            alist)))

(defun random-pick (cdf)
  (car (find-if {<= (random 1.0)} cdf :key #'cdr)))

(defun random-elt-with-decay (orig-list decay-rate)
  (if (null orig-list)
      nil
      (labels ((pick-from (list)
                 (if (null list)
                     (pick-from orig-list)
                     (if (< (random 1.0) decay-rate)
                         (car list)
                         (pick-from (cdr list))))))
        (pick-from orig-list))))

(defun random-subseq (list &optional (size (1+ (if (null list) 0
                                                   (random (length list))))))
  (if (null list)
      nil
      (subseq (shuffle list) 0 size)))

(declaim (inline random-sample-with-replacement))
(defun random-sample-with-replacement
    (range size &aux (result (make-array size :element-type 'fixnum)))
  "Return a random sample of SIZE numbers in RANGE with replacement."
  (declare (optimize speed))
  (declare (type fixnum size))
  (declare (type fixnum range))
  (dotimes (n size (coerce result 'list))
    (setf (aref result n) (random range))))

(declaim (inline random-sample-without-replacement))
(defun random-sample-without-replacement (range size)
  (declare (optimize speed))
  (declare (type fixnum size))
  (declare (type fixnum range))
  "Return a random sample of SIZE numbers in RANGE without replacement."
  (cond
    ((> size range)
     (error "Can't sample ~a numbers from [0,~a] without replacement"
            size range))
    ((= size range)
     (let ((result (make-array size :element-type 'fixnum)))
       (dotimes (n range (coerce result 'list))
         (setf (aref result n) n))))
    (t
     ;; TODO: For faster collection implement a skip-list which
     ;;       increments the value being stored as it passes might be
     ;;       a better data structure.
     (labels ((sorted-insert (list value)
                (declare (type fixnum value))
                (cond
                  ((null list) (cons value nil))
                  ((< value (the fixnum (car list))) (cons value list))
                  (t (cons (car list) (sorted-insert (cdr list) (1+ value)))))))
       (let (sorted)
         (dotimes (n size sorted)
           (setf sorted (sorted-insert sorted (random (- range n))))))))))


;;;; String replacement functions

;; From the Common Lisp Cookbook
(defgeneric replace-all (string part replacement &key test)
  (:documentation "Returns a new string in which all the
occurences of the part is replaced with replacement."))

(defmethod replace-all ((string string) (part string)
                        (replacement string) &key (test #'char=))
  (with-output-to-string (out)
    (loop :with part-length := (length part)
       :for old-pos := 0 :then (+ pos part-length)
       :for pos := (search part string
                           :start2 old-pos
                           :test test)
       :do (write-string string out
                         :start old-pos
                         :end (or pos (length string)))
       :when pos :do (write-string replacement out)
       :while pos)))

;; Specialization to base strings, which are more space
;; efficient
(defmethod replace-all ((string base-string) (part base-string)
                        (replacement base-string) &key (test #'char=))
  (coerce
   (with-output-to-string (out)
     (loop :with part-length := (length part)
        :for old-pos := 0 :then (+ pos part-length)
        :for pos := (search part string
                            :start2 old-pos
                            :test test)
        :do (write-string string out
                          :start old-pos
                          :end (or pos (length string)))
        :when pos :do (write-string replacement out)
        :while pos))
   'base-string))

(defun apply-replacements (list str)
  (if (null list)
      str
      (let ((new-str
             ;; If (caar list) is null then `replace-all' can fall
             ;; into an infinite loop.
             (if (and (caar list) (cdar list))
                 (replace-all str (caar list) (cdar list))
                 str)))
        (apply-replacements (cdr list) new-str))))


;;;; General list-related utilities
(defmacro repeatedly (times &rest body)
  (let ((ignored (gensym)))
    `(loop :for ,ignored :below ,times :collect ,@body)))

(defun indexed (list)
  (loop :for element :in list :as i :from 0 :collect (list i element)))

(defun equal-it (obj1 obj2 &optional trace inhibit-slots)
  "Equal over objects and lists."
  (let ((trace1 (concatenate 'list (list obj1 obj2) trace)))
    (cond
      ((or (member obj1 trace) (member obj2 trace)) t)
      ((and (listp obj1) (not (listp (cdr obj1)))
            (listp obj2) (not (listp (cdr obj2))))
       (and (equal-it (car obj1) (car obj2))
            (equal-it (cdr obj1) (cdr obj2))))
      ((or (and (listp obj1) (listp obj2)) (and (vectorp obj1) (vectorp obj2)))
       (and (equal (length obj1) (length obj2))
            (reduce (lambda (acc pair)
                      (and acc (equal-it (car pair) (cdr pair) trace1)))
                    (if (vectorp obj1)
                        (mapcar #'cons (coerce obj1 'list) (coerce obj2 'list))
                        (mapcar #'cons obj1 obj2))
                    :initial-value t)))
      ((class-slots (class-of obj1))
       (reduce
        (lambda (acc slot)
          (and acc (equal-it (slot-value obj1 slot) (slot-value obj2 slot)
                             trace1)))
        (remove-if [{member _ inhibit-slots :test #'string= :key #'symbol-name}
                    #'symbol-name]
                   (mapcar #'slot-definition-name
                           (class-slots (class-of obj1))))
        :initial-value t))
      (t (equal obj1 obj2)))))

(defun different-it (obj1 obj2 &optional trace)
  (let ((trace1 (concatenate 'list (list obj1 obj2) trace)))
    (cond
      ((or (member obj1 trace) (member obj2 trace)) t)
      ((or (and (vectorp obj1) (vectorp obj2))
           (and (proper-list-p obj1) (proper-list-p obj2)))
       (and (or (equal (length obj1) (length obj2))
                (format t "~&different lengths ~a!=~a"
                        (length obj1) (length obj2)))
            (reduce (lambda-bind (acc (i (a b)))
                      (and acc (or (different-it a b trace1)
                                   (format t "~& at ~d ~a!=~a" i a b))))
                    (indexed
                     (if (vectorp obj1)
                         (mapcar #'list (coerce obj1 'list) (coerce obj2 'list))
                         (mapcar #'list obj1 obj2)))
                    :initial-value t)))
      ((and (consp obj1) (consp obj2))
       (and (different-it (car obj1) (car obj2))
            (different-it (cdr obj1) (cdr obj2))))
      ((class-slots (class-of obj1))
       (reduce (lambda (acc slot)
                 (and acc (or (different-it
                               (slot-value obj1 slot) (slot-value obj2 slot)
                               trace1)
                              (format t "~&  ~a" slot))))
               (mapcar #'slot-definition-name
                       (class-slots (class-of obj1)))
               :initial-value t))
      (t (or (equal obj1 obj2) (format t "~&~a!=~a" obj1 obj2))))))

(defun transpose (matrix)
  "Simple matrix transposition."
  (apply #'map 'list #'list matrix))

(defun interleave (list sep &optional rest)
  (cond
    ((cdr list) (interleave (cdr list) sep (cons sep (cons (car list) rest))))
    (list (reverse (cons (car list) rest)))
    (t nil)))

(defun drop-until (pred seq)
  (drop-while (complement pred) seq))

(defun take-until (pred seq)
  (take-while (complement pred) seq))

(defun pad (list n &optional (elem nil))
  "Pad LIST to a length of N with ELEM"
  (if (>= (length list) n)
      list
      (append list (make-list (- n (length list))
                              :initial-element elem))))

(defun chunks (list size &optional include-remainder-p)
  "Return subsequent chunks of LIST of size SIZE."
  (loop :for i :to (if include-remainder-p
                       (length list)
                       (- (length list) size))
     :by size :collect (subseq list i (min (+ i size) (length list)))))

(defun cartesian (lists)
  "Cartesian product of a set of lists."
  (cartesian-without-duplicates lists :test (constantly nil)))

(defun cartesian-without-duplicates (lists &key (test #'eql))
  "Cartesian product of a set of lists, without sets containing duplicates."
  (labels ((cartesian-nil-duplicates (lists)
             (if (car lists)
                 (mappend (lambda (inner)
                            (mapcar (lambda (outer)
                                      (if (not (member outer inner :test test))
                                          (cons outer inner)
                                          nil))
                                    (car lists)))
                          (cartesian-nil-duplicates (cdr lists)))
                 (list nil))))
    (remove-if [{> (length lists)} #'length] (cartesian-nil-duplicates lists))))

(defun binary-search (value array &key (low 0)
                                       (high (1- (length array)))
                                       (test (lambda (v)
                                                (cond ((< v value) -1)
                                                      ((> v value) 1)
                                                      (t 0)))))
  "Perform a binary search for VALUE on a sorted ARRAY.
Optional keyword parameters:
LOW:  Lower bound
HIGH: Higher bound
TEST: Test for the binary search algorithm taking on arg.
Return -1 if arg is less than value, 1 if arg is greater than value,
and 0 otherwise."
  (if (< high low)
      nil
      (let ((middle (floor (/ (+ low high) 2))))

        (cond ((< 0 (funcall test (aref array middle)))
               (binary-search value array :low low
                                          :high (1- middle)
                                          :test test))

              ((> 0 (funcall test (aref array middle)))
               (binary-search value array :low (1+ middle)
                                          :high high
                                          :test test))

              (t middle)))))

;; scheme implementation given at
;; http://en.wikipedia.org/wiki/Levenshtein_distance
(defun levenshtein-distance (s1 s2 &key (test #'char=) (key #'identity))
  (let* ((width (1+ (length s1)))
         (height (1+ (length s2)))
         (d (make-array (list height width))))
    (dotimes (x width)
      (setf (aref d 0 x) x))
    (dotimes (y height)
      (setf (aref d y 0) y))
    (dotimes (x (length s1))
      (dotimes (y (length s2))
        (setf (aref d (1+ y) (1+ x))
              (min (1+ (aref d y (1+ x)))
                   (1+ (aref d (1+ y) x))
                   (+ (aref d y x)
                      (if (funcall test
                                   (funcall key (aref s1 x))
                                   (funcall key (aref s2 y)))
                          0
                          1))))))
    (aref d (1- height) (1- width))))

(defun tails (lst)
  "Return all final segments of the LST, longest first.

For example (tails '(a b c)) => ('(a b c) '(b c) '(c))
"
  (when lst (cons lst (tails (cdr lst)))))

(defun pairs (lst)
  "Return all pairs of elements in LST.

For example (pairs '(a b c)) => ('(a . b) '(a . c) '(b . c))
"
  (iter (for (a . rest) in (tails lst))
        (appending (iter (for b in rest)
                         (collecting (cons a b))))))

(defun mapcar-improper-list (fn list)
  "Apply FN to the elements of a possibly improper list LIST,
including the final non-nil tail element, if any.  Return a fresh
list composed of the value returned by each application.  Does
not work on circular lists."
  (let* ((head (list nil))
         (tail head))
    (iter (while (consp list))
          (setf tail (setf (cdr tail)
                           (list (funcall fn (pop list))))))
    (when list
      (setf (cdr tail) (funcall fn list)))
    (cdr head)))


;;; Hash-table related functions
(defun make-thread-safe-hash-table (&rest args)
  "Create a thread safe hash table with the given ARGS"
  #+(or sbcl ecl)
  (apply #'make-hash-table :synchronized t args)
  #+ccl
  (apply #'make-hash-table :shared :lock-free args)
  #-(or ccl sbcl ecl)
  (error "unsupported implementation for thread-safe hashtables"))

(defun find-hash-table-element (hash-tbl n)
  (maphash
   (lambda (k v)
     (declare (ignore v))
     (when (= n 0) (return-from find-hash-table-element k))
     (decf n))
   hash-tbl))

(defun random-hash-table-key (hash-tbl)
  "Return a random key in a hash table"
  (let ((size (hash-table-count hash-tbl)))
    (unless (zerop size)
      (find-hash-table-element hash-tbl (random size)))))
