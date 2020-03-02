;;; test.lisp --- tests for the `gt' package
(defpackage :gt/test
  (:nicknames :gt/test)
  (:use :common-lisp
        :curry-compose-reader-macros
        :named-readtables
        :stefil
        :gt/filesystem
        :gt/misc
        :gt/shell
        #+gt :testbot)
  (:import-from :serapeum :trim-whitespace)
  (:export :test :batch-test :testbot-test))
(in-package :gt/test)
(in-readtable :curry-compose-reader-macros)

#-gt
(progn
  ;;; External replacement for GT-specific test submission helpers
  (defvar *success* nil "Variable indicating test success or failure.")
  (defun batch-test (test project branch &optional args)
    "Run tests in 'batch' mode, printing results as a string."
    (declare (ignorable project branch args))

    (let* ((stefil::*test-progress-print-right-margin* (expt 2 20))
           (failures (coerce (stefil::failure-descriptions-of
                              (without-debugging (funcall test)))
                             'list)))
      (setf *success*
            (if failures
                (prog1 nil
                  (format *error-output* "FAILURES~%")
                  (mapc [{format *error-output* "  ~a~%"}
                         #'stefil::name-of
                         #'stefil::test-of
                         #'car #'stefil::test-context-backtrace-of]
                        failures))
                (prog1 t
                  (format *error-output* "SUCCESS~%")))))))

(defun run-batch (&rest a)
  (declare (ignorable a))
  #+ccl (setf ccl::*interactive-streams-initialized* nil)
  (batch-test #'test "gt" "master"))

(defparameter +etc-dir+ (append (pathname-directory
                                 #.(or *compile-file-truename*
                                       *load-truename*
                                       *default-pathname-defaults*))
                                (list "test" "etc"))
  "Path to directory holding testing artifacts.")

(defsuite test)
(in-suite test)

(deftest file-to-string-empty-file ()
  (with-temp-file-of (tmp) ""
    (is (equal "" (file-to-string tmp)))))

(deftest file-to-string-non-utf8-encoding ()
  #-ccl       ; CCL silently reads w/o warning despite bad encoding...
  (is (string= (file-to-string (make-pathname :directory +etc-dir+
                                              :defaults "latin-1.c"))
               "/* Here is a non-ASCII character: § */")))

(deftest which-test ()
  (is (null (which "dsjafpoarue")))
  (is (not (null (which #-windows "ls" #+windows "cmd.exe")))))

#-windows
(deftest shell-directory-test ()
  (multiple-value-bind (stdout stderr errno)
      (shell "pwd" :directory "/tmp/")
    (is (equal "/tmp" (trim-whitespace stdout)))
    (is (equal "" stderr))
    (is (zerop errno))))

#+windows
(deftest shell-directory-test ()
  (let* ((temp-file (pathname (sel/utility:temp-file-name)))
         (temp-dir (make-pathname :directory (pathname-directory temp-file)
                                  :device (pathname-device temp-file))))
    (multiple-value-bind (stdout stderr errno)
        (shell "chdir" :directory (namestring temp-dir))
      (let ((dir (trim-whitespace stdout)))
        ;; append slash at end if necessary to make it a directory
        (unless (or (char= (char dir (1- (length dir))) #\\)
                    (char= (char dir (1- (length dir))) #\/))
          (setf dir (concatenate 'string dir "/")))
        (is (equal temp-dir (pathname dir))) ; compare pathnames
        (is (equal "" stderr))
        (is (zerop errno))))))

#-windows ; IO-SHELL not yet supported on Windows
(deftest read-and-write-shell-files ()
  (let ((test-string "Hello world. Hello world. Hello world."))
    (is (string= test-string
                 (handler-case
                     (with-temp-file (temp.xz)
                       (write-shell-file (out temp.xz "xz")
                                         (write-line test-string out))
                       ;; NOTE: sleep one second to account for rare stochastic
                       ;; end-of-file errors in which I think we're going from
                       ;; writing to reading too quickly for the file system.
                       (sleep 1)
                       (read-shell-file (in temp.xz "xzcat")
                                        (read-line in)))
                   (error (c) (declare (ignorable c)) nil))))))

#-windows
(deftest read-and-write-bytes-shell-files ()
  (let ((byte #x25))
    (is (equal byte
               (handler-case
                   (with-temp-file (temp.xz)
                     (write-shell-file (out temp.xz "xz")
                                       (write-byte byte out))
                     ;; NOTE: sleep one second to account for rare stochastic
                     ;; end-of-file errors in which I think we're going from
                     ;; writing to reading too quickly for the file system.
                     (sleep 1)
                     (read-shell-file (in temp.xz "xzcat")
                                      (read-byte in)))
                 (error (c) (declare (ignorable c)) nil))))))


(deftest pad-list-expand-to-requisite-length ()
  (is (equal '(1 2 3 3) (pad '(1 2) 4 3))))

(deftest pad-list-already-of-requisite-length ()
  (is (equal '(1 2 3) (pad '(1 2 3) 3))))

(deftest cartesian-test ()
  (is (equal '(()) (cartesian nil)))
  (is (equal '((1)) (cartesian '((1)))))
  (is (equal '((1 2) (2 2) (1 3) (2 3))
             (cartesian '((1 2) (2 3))))))

(deftest cartesian-without-duplicates-test ()
  (is (equal '(()) (cartesian-without-duplicates nil)))
  (is (equal '((1)) (cartesian-without-duplicates '((1)))))
  (is (equal '((1 2) (1 3) (2 3))
             (cartesian-without-duplicates '((1 2) (2 3))))))
