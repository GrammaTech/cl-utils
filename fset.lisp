;;;; fset.lisp - Override and re-export common lisp operations with
;;;; different semantics or arguments in the fset library.
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
(uiop/package:define-package :gt/fset
  (:import-from :cl :defgeneric :defmethod :&key :&optional)
  (:import-from :fset :collection)
  (:shadow :last :union :intersection :set-difference)
  (:export :last :union :intersection :set-difference))
(in-package :gt/fset)

(defgeneric last (list &optional n)
  (:method ((list cons) &optional (n 1))
    (cl:last list n))
  (:method ((list collection) &optional (n 1)
            &aux (len (fset:size list)))
    (fset:subseq list (if (<= len n) 0 (- len n)))))

(defgeneric union (list-1 list-2 &key key test test-not)
  (:method ((list-1 list) (list-2 list) &rest args &key key test test-not)
    (declare (ignorable key test test-not))
    (apply #'cl:union list-1 list-2 args))
  (:method ((list-1 collection) (list-2 collection) &key key test test-not)
    (declare (ignorable key test test-not))
    (fset:union list-1 list-2)))

(defgeneric intersection (list-1 list-2 &key key test test-not)
  (:method ((list-1 list) (list-2 list) &rest args &key key test test-not)
    (declare (ignorable key test test-not))
    (apply #'cl:intersection list-1 list-2 args))
  (:method ((list-1 collection) (list-2 collection) &key key test test-not)
    (declare (ignorable key test test-not))
    (fset:intersection list-1 list-2)))

(defgeneric set-difference (list-1 list-2 &key key test test-not)
  (:method ((list-1 list) (list-2 list) &rest args &key key test test-not)
    (declare (ignorable key test test-not))
    (apply #'cl:set-difference list-1 list-2 args))
  (:method ((list-1 collection) (list-2 collection) &key key test test-not)
    (declare (ignorable key test test-not))
    (fset:set-difference list-1 list-2)))
