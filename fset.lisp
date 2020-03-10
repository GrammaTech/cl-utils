;;;; fset.lisp - Override some FSet generics with CL-consistent alternatives
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

(defgeneric last (collection &optional n)
  (:documentation #.(documentation 'cl:last 'function))
  (:method ((collection cons) &optional (n 1))
    (cl:last collection n))
  (:method ((collection collection) &optional (n 1)
            &aux (len (fset:size collection)))
    (fset:subseq collection (if (<= len n) 0 (- len n)))))

(defgeneric union (collection-1 collection-2 &key key test test-not)
  (:documentation #.(documentation 'cl:union 'function))
  (:method ((collection-1 list) (collection-2 list)
            &rest args &key key test test-not)
    (declare (ignorable key test test-not))
    (apply #'cl:union collection-1 collection-2 args))
  (:method ((collection-1 collection) (collection-2 collection)
            &key key test test-not)
    (declare (ignorable key test test-not))
    (fset:union collection-1 collection-2)))

(defgeneric intersection (collection-1 collection-2 &key key test test-not)
  (:documentation #.(documentation 'cl:intersection 'function))
  (:method ((collection-1 list) (collection-2 list)
            &rest args &key key test test-not)
    (declare (ignorable key test test-not))
    (apply #'cl:intersection collection-1 collection-2 args))
  (:method ((collection-1 collection) (collection-2 collection)
            &key key test test-not)
    (declare (ignorable key test test-not))
    (fset:intersection collection-1 collection-2)))

(defgeneric set-difference (collection-1 collection-2 &key key test test-not)
  (:documentation #.(documentation 'cl:set-difference 'function))
  (:method ((collection-1 list) (collection-2 list)
            &rest args &key key test test-not)
    (declare (ignorable key test test-not))
    (apply #'cl:set-difference collection-1 collection-2 args))
  (:method ((collection-1 collection) (collection-2 collection)
            &key key test test-not)
    (declare (ignorable key test test-not))
    (fset:set-difference collection-1 collection-2)))
