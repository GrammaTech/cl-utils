;;;; profile.lisp --- Tools for profiling lisp code
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
;;;;
;;;;
;;;; Tools for profiling lisp code.  Specifically functionality for
;;;; dumping profiling information into a format usably for the
;;;; generation of flame graphs (see http://oprofile.sourceforge.net/news/)
(uiop/package:define-package :gt/profile
  (:use :common-lisp :alexandria :iterate :gt/misc :arrow-macros
        :cl-ppcre :split-sequence
        :named-readtables :curry-compose-reader-macros)
  (:import-from :cl-dot)
  (:import-from :uiop/run-program :run-program)
  (:import-from :uiop/os :getenv)
  (:export :*profile-dot-min-ratio*
           :*profile-flame-graph*
           :profile-to-dot-graph
           :profile-to-flame-graph
           :with-prof))
(in-package :gt/profile)
(in-readtable :curry-compose-reader-macros)

;; Dot implementation from
;; https://techfak.uni-bielefeld.de/~jmoringe/call-graph.html.
(defvar *profile-dot-min-ratio* 1/200
  "Minimum percentage ratio to include a node in the profile dot graph.")

#+sbcl
(defmethod cl-dot:graph-object-node
    ((graph sb-sprof::call-graph) (object sb-sprof::node))
  (flet ((ratio->color (ratio)
           (let ((red   (floor 255))
                 (green (floor (alexandria:lerp ratio 255 0)))
                 (blue  (floor (alexandria:lerp ratio 255 0))))
             (logior (ash red 16) (ash green 8) (ash blue 0)))))
    (let ((ratio (/ (sb-sprof::node-count object)
                    (sb-sprof::call-graph-nsamples graph))))
      (make-instance 'cl-dot:node
        :attributes `(:label ,(format nil "~A\\n~,2,2F %"
                                      (sb-sprof::node-name object) ratio)
                             :shape     :box
                             :style     :filled
                             :fillcolor ,(format nil "#~6,'0X"
                                                 (ratio->color ratio)))))))

#+sbcl
(defmethod cl-dot:graph-object-pointed-to-by
    ((graph sb-sprof::call-graph) (object sb-sprof::node))
  (sb-sprof::node-callers object))

#+sbcl
(defun profile-to-dot-graph (stream)
  "Write profile to STREAM."
  (progn
    (unless sb-sprof::*samples*
      (warn "; `profile-to-dot-graph': No samples to report.")
      (return-from profile-to-dot-graph))
    (let ((call-graph (sb-sprof::make-call-graph most-positive-fixnum)))
      (cl-dot:print-graph
       (cl-dot:generate-graph-from-roots
        call-graph
        (remove-if [{> *profile-dot-min-ratio*}
                    {/ _ (sb-sprof::call-graph-nsamples call-graph)}
                    #'sb-sprof::node-count]
                   (sb-sprof::call-graph-vertices call-graph)))
       :stream stream))))

#-sbcl
(defun profile-to-dot-graph (&rest args)
  (declare (ignorable args))
  (error "`PROFILE-TO-DOT-GRAPH' unimplemented for non-SBCL lisps."))

;; FlameGraph implementation from
;; http://paste.lisp.org/display/326901.
;;
;; NOTE: Related package. https://github.com/40ants/cl-flamegraph
#+sbcl
(defun profile-to-flame-graph (stream)
  "Write FlameGraph profile data to STREAM.
The resulting file may be fed directly to the flamegraph tool as follows.

    REPL> (sb-sprof:start-profiling)

       ...do some work...

    REPL> (with-open-file (out \"profile.data\"
                               :direction :output
                               :if-exists :supersede)
            (profile-to-flame-graph out))

    shell$ cat profile.data|flamegraph > profile.svg

See http://www.brendangregg.com/FlameGraphs/cpuflamegraphs.html."
  (progn
    (unless sb-sprof::*samples*
      (warn "; `profile-to-flame-graph': No samples to report.")
      (return-from profile-to-flame-graph))
    (let ((samples (sb-sprof::samples-vector sb-sprof::*samples*))
          (counts (make-hash-table :test #'equal)))

      (sb-sprof::with-lookup-tables ()
        (loop :for start = 0 :then end
           :while (< start (length samples))
           :for end = (or (position 'sb-sprof::trace-start samples
                                    :start (1+ start)
                                    :key (lambda (it) (and (listp it) (car it))))
                          (return))
           :do (let ((key
                      (sb-sprof::with-output-to-string (stream)
                        (loop :for i :from (- end 2) :downto (+ start 2) :by 2
                           :for node = (sb-sprof::lookup-node
                                        (aref samples i))
                           :when node
                           :do (let ((*print-pretty* nil))
                                 (format stream "~A;"
                                         (sb-sprof::node-name node)))))))
                 (incf (gethash key counts 0)))))

      (maphash (lambda (trace count)
                 (format stream "~A ~D~%" trace count))
               counts))))

#-sbcl
(defun profile-to-flame-graph (&rest args)
  (declare (ignorable args))
  (error "`PROFILE-TO-FLAME-GRAPH' unimplemented for non-SBCL lisps."))

;;; Profiling

(defvar *profile-flame-graph* nil
  "Write report with `profile-to-flame-graph' from `with-prof'.")

(defmacro with-prof (profile-path &rest body)
  "Execute BODY with profiling enables.  Write the profile report
to a file at PROFILE-PATH.  Currently only works in SBCL."
  #+sbcl
  ;; Profiler settings
  `(if ,profile-path
       (sb-sprof:with-profiling (:sample-interval .01
                                                  :max-samples 1000000
                                                  :mode :cpu
                                                  :loop nil)
         ,@body
         (if gt/profile:*profile-flame-graph*
             (with-open-file (out ,profile-path
                                  :if-exists :supersede
                                  :if-does-not-exist :create
                                  :direction :output)
               (gt/profile:profile-to-flame-graph out))
             (with-output-to-file (out ,profile-path
                                       :if-exists :overwrite
                                       :if-does-not-exist :create)
               (sb-sprof:report :stream out))))
       (progn ,@body))
  #-sbcl
  `(progn
     (when ,profile-path
       (with-output-to-file (out ,profile-path)
         (format out "Not profiling.~%")))
     ,@body))
