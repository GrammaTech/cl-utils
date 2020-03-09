;;;; full.lisp --- Omnibus package to include all gt/* packages
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
(uiop/package:define-package :gt/full
    (:use-reexport :gt/gt :gt/filesystem :gt/misc :gt/profile :gt/shell))
