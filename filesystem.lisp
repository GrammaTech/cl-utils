;;;; filesystem.lisp --- Files and Directories
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
;;;; Functions for working with file and directories.  Includes
;;;; functions for efficient serialization to/from files as well as
;;;; functions for creating and using temporary files and directories.
;;;;
(uiop/package:define-package :gt/filesystem
  (:use-reexport :uiop/filesystem :uiop/pathname)
  (:use :common-lisp :alexandria :serapeum :iterate)
  (:local-nicknames
   (:file :org.shirakumo.file-attributes))
  (:shadowing-import-from :iterate
                          ;; Shadow serapeum macros.
                          :summing :collecting :sum :in)
  (:import-from :split-sequence :split-sequence)
  (:import-from :uiop/run-program :run-program)
  (:import-from :uiop/os :getcwd)
  (:import-from :uiop/filesystem :with-current-directory)
  (:import-from :uiop/stream
                :default-temporary-directory)
  #-ecl
  (:import-from :asdf-encodings
                :detect-file-encoding
                :encoding-external-format)
  (:export
   :file-mime-type
   :reasonable-external-format
   :file-to-string
   :file-to-bytes
   :with-character-output-to-file
   :string-to-file
   :bytes-to-file
   :file-access
   :file-access-path
   :file-access-operation
   ;; Temporary files
   :*temp-dir*
   :with-temporary-file
   :with-temporary-file-of
   :with-temporary-directory
   :with-temporary-directory-of
   :with-temporary-fifo
   :temp-file-name
   :delete-path
   ;; cl-fad
   :merge-pathnames-as-file
   :merge-pathnames-as-directory
   :canonical-pathname
   :directory-wildcard
   :list-directory
   :walk-directory
   ;; Other filename/pathname.
   :pathname-relativize
   :directory-p
   :getcwd
   :chdir
   :with-current-directory
   :file-permissions
   :pathname-as-directory
   :copy-file-with-attributes))
(in-package :gt/filesystem)

(defun file-mime-type (path)
  "Return the mime type of PATH as a list of two keywords.
The Unix `file' command is used, specifically \"file -b --mime-type PATH\"."
  (assert (probe-file path) (path) "No file or directory at ~S" path)
  (flet ((file-mime-type (path)
           (nest (mapcar #'make-keyword)
                 (split-sequence #\/)
                 (string-upcase)
                 (trim-whitespace)
                 (run-program
                  `("file" "-b" "--mime-type"
                           ,(uiop:native-namestring path))
                  :output :string))))
    (let ((type (pathname-type path)))
      (if (null type) (file-mime-type path)
          ;; Handle the most common extensions inline. These are not
          ;; "correct" (e.g. the correct mime type for a .json file is
          ;; application/json) but they reflect the behavior of the
          ;; `file' command and what the callers of this function
          ;; expect.
          (string-case (pathname-type path)
            (("java"
              "txt" "md" "org" "yml" "yaml" "html"
              "js" "ts" "json" "map"    ;.map for JS source maps.
              "lisp" "asd")
             '(:text :plain))
            ("xml" '(:text :xml))
            (("cpp" "hpp") '(:text :x-c++))
            (("py" "pyi")
             '(:text :x-python))
            (("dat" "pyc")
             '(:application :octet-stream))
            ("class"
             '(:application :x-java-applet))
            ("png" '(:image :png))
            ("jpeg" '(:image :jpeg))
            (t (file-mime-type path)))))))

(defun reasonable-external-format (filespec)
  #+ecl (declare (ignorable filespec))
  (let ((guess #-ecl
          (encoding-external-format
           (detect-file-encoding filespec))
          #+ecl :default))
    (if (eq guess :default)
        #+sbcl sb-impl::*default-external-format*
        #+ecl ext:*default-external-format*
        #+ccl ccl:*default-external-format*
        #-(or sbcl ecl ccl) :latin-1
        guess)))

(defun file-to-string (filespec &key (external-format (reasonable-external-format filespec)))
  "Return the contents of FILESPEC as a string."
  #+ccl (declare (ignorable external-format))
  (labels
      ((run-read ()
         (let (#+sbcl (sb-impl::*default-external-format* external-format)
               #+ecl (ext:*default-external-format* external-format)
               #+ccl (ccl:*default-external-format* external-format)
               (element-type (case external-format
                               ((:ascii :us-ascii) 'base-char)
                               (t 'character))))
           (with-open-file (in filespec :element-type element-type)
             (let* ((file-bytes (file-length in))
                    (seq (make-string file-bytes :element-type element-type))
                    (file-chars (read-sequence seq in)))
               (if (= file-bytes file-chars)
                   seq
                   ;; Truncate the unused tail of seq.  It is possible
                   ;; for read-sequence to read less than file-length
                   ;; when the file has multi-byte UTF-8 characters.
                   (subseq seq 0 file-chars)))))))

    (restart-case
        (if (member external-format '(:utf8 :utf-8))
            (run-read)
            (handler-case
                (run-read)
              (stream-error (c)
                ;; Try utf-8 as a default fallback.
                (declare (ignorable c))
                (setf external-format :utf-8)
                (run-read))))
      (use-encoding (encoding)
        :report "Specify another encoding."
        (setf external-format encoding)
        (run-read)))))

(declaim (ftype (function ((or string pathname))
                          (simple-array (unsigned-byte 8)))
                file-to-bytes))
(defun file-to-bytes (filespec)
  "Return the contents of FILESPEC as a byte array."
  (with-open-file (in filespec :element-type '(unsigned-byte 8))
    (let ((seq (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence seq in)
      seq)))

(define-condition file-access (error)
  ((file-access-path :initarg :path :initform nil
                     :reader file-access-path)
   (file-access-operation :initarg :operation :initform nil
                          :reader file-access-operation))
  (:report (lambda (condition stream)
             (format stream "Unable to ~a file ~a"
                     (file-access-operation condition)
                     (file-access-path condition)))))

(defun call-with-character-output-to-file
    (function filespec &key
                         (if-exists :supersede)
                         (external-format :default)
     &aux file-exists-p)
  " to FILESPEC.
Restarts available to handle cases where FILESPEC is not writable,
SET-FILE-WRITABLE, and where the appropriate encoding is not used,
USE-ENCODING. "
  (when-let (truename (probe-file filespec))
    (setf file-exists-p t
          filespec truename))
  (labels ((run-write ()
             (ensure-directories-exist filespec)
             (with-open-file (out filespec :direction :output
                                           :if-exists if-exists
                                           :external-format external-format)
               (funcall function out))))

    (when (and file-exists-p
               (not (member :user-write (file-permissions filespec))))
      (restart-case
          (error (make-condition 'file-access
                                 :path filespec
                                 :operation :write))
        (set-file-writable ()
          (format nil "Forcefully set ~a to be writable" filespec)
          (push :user-write (file-permissions filespec)))))

    (restart-case
        (if (member external-format '(:utf8 :utf-8))
            (run-write)
            (handler-case
                (run-write)
              (stream-error (c)
                ;; Try utf-8 as a default fallback.
                (declare (ignorable c))
                (setf external-format :utf-8)
                (run-write))))
      (use-encoding (encoding)
        :report "Specify another encoding."
        (setf external-format encoding)
        (run-write))))
  filespec)

(defmacro with-character-output-to-file ((stream filespec &rest args
                                          &key &allow-other-keys)
                                         &body body)
  "Open FILESPEC for character output (providing restarts for common
problems) and bind the stream to STREAM."
  `(call-with-character-output-to-file
    (lambda (,stream)
      ,@body)
    ,filespec
    ,@args))

(defun string-to-file (string filespec &key
                                         (if-exists :supersede)
                                         (external-format :default))
  "Write STRING to FILESPEC.
Restarts available to handle cases where FILESPEC is not writable,
SET-FILE-WRITABLE, and where the appropriate encoding is not used,
USE-ENCODING. "
  (with-character-output-to-file (out filespec
                                      :if-exists if-exists
                                      :external-format external-format)
    (write-string string out)))

(defun bytes-to-file (bytes filespec &key (if-exists :supersede))
  "Write BYTES to FILESPEC"
  (with-open-file (out filespec :element-type '(unsigned-byte 8)
                       :direction :output :if-exists if-exists)
    (write-sequence bytes out)))


;;; Temporary files
(defvar *temp-dir* (namestring (default-temporary-directory))
  "Set to non-nil for a custom temporary directory.")

(uiop:register-image-restore-hook
 (lambda ()
   (setf *temp-dir*
         (namestring (default-temporary-directory)))))

(defmacro with-temporary-file ((&key (stream (gensym "STREAM") streamp)
                                  (pathname (gensym "PATHNAME") pathnamep)
                                  (directory '*temp-dir*) prefix suffix type
                                  keep direction element-type external-format)
                               &body body)
  "Evaluate BODY where the symbol specified by the keyword argument PATHNAME
is bound to a newly created temporary file.

* PATHNAME Symbol to be bound to a newly created temporary file.
* DIRECTORY Directory where the temporary file is to be created.
* TYPE Extension of the temporary file.
* KEEP Whether to retain the temporary file.

All other keyword arguments are currently unimplemented and exist
for compatability purposes with `uiop/stream:with-temporary-file`."
  (declare (ignorable stream prefix suffix keep direction
                      element-type external-format))
  (assert pathnamep (pathname)
          "pathname must be given to `with-temporary-file'")
  (assert (not streamp) (stream)
          "stream not implemented for `with-temporary-file'")
  (once-only (keep)
    `(let* ((,pathname (temp-file-name :directory ,directory :type ,type)))
       (unwind-protect (progn ,@body)
         (locally (declare #+sbcl (sb-ext:muffle-conditions
                                   sb-ext:code-deletion-note))
           (unless ,keep
             (delete-path ,pathname)))))))

(defmacro with-temporary-file-of
    ((&rest args &key (pathname (gensym "PATHNAME")) &allow-other-keys)
     string &body body)
  "Execute BODY with STRING in a temporary file whose path is
bound to PATHNAME.  Additional keyword args are passed thru to
`with-temporary-file`."
  `(with-temporary-file (,@args)
     (when ,pathname (string-to-file ,string ,pathname))
     ,@body))

(defmacro with-temporary-fifo
    ((&rest args &key (pathname (gensym "PATHNAME")) &allow-other-keys)
     &body body)
  "Execute BODY with a temporary fifo whose path is bound to PATHNAME.
Additional keyword args are passed thru to `with-temporary-file`."
  `(with-temporary-file (,@args)
     (delete-path ,pathname)
     #-windows
     (progn
       #+sbcl
       (sb-posix:mkfifo ,pathname
                        (logior sb-posix:s-iwusr
                                sb-posix:s-irusr))
       #+ccl
       (#_mkfifo ,pathname (logior #$S_IWUSR #$S_IRUSR)))
     ,@body))

(defmacro with-temporary-directory
    ((&rest args &key (pathname (gensym "PATHNAME")) &allow-other-keys)
     &body body)
  "Execute BODY with a temporary directory whose path is bound to PATHNAME.
Additional keyword arguments behave correspondingly to those in
`with-temporary-file`."
  `(with-temporary-file (:pathname ,pathname ,@args)
     (delete-path ,pathname)
     (setf ,pathname (namestring (ensure-directory-pathname ,pathname)))
     (ensure-directories-exist ,pathname)
     ,@body))

(defmacro with-temporary-directory-of
    ((&rest args &key (pathname (gensym "PATHNAME")) &allow-other-keys)
     dir &body body)
  "Execute BODY with a temporary directory with the contents of DIR
whose path is bound to PATHNAME.  Additional keyword arguments behave
correspondingly to those in `with-temporary-file`."
  `(with-temporary-directory (:pathname ,pathname ,@args)
     (run-program (format nil "cp -pr ~a/. ~a"
                              (namestring ,dir)
                              (namestring ,pathname)))
     ,@body))

(defun temp-file-name (&key (directory *temp-dir*) type)
  (let ((base
          #+clisp
          (let ((stream (gensym)))
            (eval `(with-open-stream
                       (,stream (ext:mkstemp
                                 (if directory
                                     (namestring (make-pathname
                                                  :directory directory
                                                  :name "XXXXXX"))
                                     nil)))
                     (namestring (pathname ,stream)))))
          #+(or sbcl ccl ecl)
          (tempnam directory nil)
          #+allegro
          (system:make-temp-file-name nil directory)
          #-(or sbcl clisp ccl allegro ecl)
          (error "no temporary file backend for this lisp.")))
    (concatenate 'string base "." (or type ""))))

#+sbcl
(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (sb-alien:define-alien-routine (#-windows "tempnam" #+windows "_tempnam" tempnam)
      sb-alien:c-string
    (dir sb-alien:c-string)
    (prefix sb-alien:c-string)))

#+(and ccl linux)
(defun tempnam (dir prefix)
  (ccl:with-filename-cstrs ((base dir) (prefix (or prefix "")))
    (ccl:get-foreign-namestring
     (ccl:external-call "tempnam" :address base :address prefix :address))))

#+(and ccl windows)
(defun tempnam (dir prefix)
  (ccl:with-filename-cstrs ((base (or dir "")) (prefix (or prefix "")))
    (ccl:%get-cstring
     (ccl:external-call "_tempnam" :address
                        base :address
                        prefix :address))))

#+ecl
(defun tempnam (dir &optional prefix)
  (let ((dir-name (uiop:ensure-directory-pathname
                   (or dir *temporary-directory*))))
    (ext:mkstemp (if prefix
                     (uiop::merge-pathnames dir-name prefix)
                     dir-name))))

(defun delete-path (path)
  "Delete anything (file or directory) at PATH."
  (let ((probe (probe-file path)))
    (when probe
      (if (equal (directory-namestring probe)
                 (namestring probe))
          (if (null (list-directory probe))
              (delete-directory-tree (ensure-directory-pathname probe)
                                     :validate t)
              (run-program (concatenate 'string "rm -rf " (namestring probe))))
          (delete-file path)))))


;;;; Utilities from cl-fad.
;;;
;;; The functions `merge-pathnames-as-file', `merge-pathnames-as-directory',
;;; `canonical-pathname', `directory-wildcard', `list-directory', and
;;; `walk-directory' were adapted from cl-fad.  The CL-FAD license applies
;;; to the source for these functions and is included below:
;;;
;;; Copyright (c) 2004, Peter Seibel.  All rights reserved.
;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
(defun canonical-pathname (path)
  "Remove redundant information from PATH."
  (iter (with full-dir = (or (pathname-directory (pathname path))
                             (list :relative)))
        (with canon-dir-kind = (if (member (first full-dir) '(:relative :absolute))
                                   (pop full-dir)
                                   :relative))
        (with canon-dir = nil)
        (while full-dir)
        (cond ((string= "." (first full-dir))
               (pop full-dir))
              ((not (member (first full-dir) '(:back :up)))
               (push (pop full-dir) canon-dir))
              ;; Handling for ../
              ((member (first canon-dir) '(:back :up))
               (assert (eql canon-dir-kind :relative))
               (push (pop full-dir) canon-dir))
              (canon-dir
               (pop full-dir)
               (pop canon-dir))
              ;; Drop .. at the beginning of an absolute pathname.
              ((eql canon-dir-kind :absolute)
               (pop full-dir))
              ;; Preserve .. at the beginning of a relative
              ;; pathname.
              ((eql canon-dir-kind :relative)
               (push (pop full-dir) canon-dir))
              (t (push (pop full-dir) canon-dir)))
        (finally (return (make-pathname :defaults (pathname path)
                                        :directory
                                        (cons canon-dir-kind
                                              (nreverse canon-dir)))))))

(defun merge-pathnames-as-directory (&rest pathnames)
  "Given a list of pathnames, this returns a single
directory pathname containing the logical concatenation of them all."
  (if (null pathnames)
      (make-pathname)
      (let* ((pathnames (mapcar #'pathname pathnames))
             (defaults (first pathnames))
             (dir (pathname-directory defaults)))
        (make-pathname
          :defaults defaults
          :directory (iter (for pathname in (rest pathnames))
                           (for directory = (pathname-directory pathname))
                           (cond ((null dir)
                                  (setf dir directory))
                                 ((eq :absolute (first directory))
                                  (setf dir directory))
                                 ((eq :relative (first directory))
                                  (setf dir (append dir (rest directory)))))
                           (finally (return dir)))
          :name nil :type nil))))

(defun merge-pathnames-as-file (&rest pathnames)
  "Given a list of pathnames returns a single filename pathname
containing the logical concatenation of them all."
  (if (null pathnames)
      (make-pathname)
      (let ((file-name (first (last pathnames))))
        (make-pathname :defaults (apply #'merge-pathnames-as-directory pathnames)
                       :name (pathname-name file-name)
                       :type (pathname-type file-name)
                       :version (pathname-version file-name)))))

(defun directory-wildcard (dirname)
  "Returns a wild pathname designator that designates all files within
the directory named by the non-wild pathname designator DIRNAME."
  (when (wild-pathname-p dirname)
    (error "Can only make wildcard directories from non-wildcard directories."))
  (make-pathname :name #-:cormanlisp :wild #+:cormanlisp "*"
                 :type #-(or :clisp :cormanlisp) :wild
                       #+:clisp nil
                       #+:cormanlisp "*"
                 :defaults (pathname-as-directory dirname)))

(defun list-directory (dirname &key (follow-symlinks t))
  "Returns a fresh list of pathnames corresponding to all files within
the directory named by the non-wild pathname designator DIRNAME.
The pathnames of sub-directories are returned in directory form -
see PATHNAME-AS-DIRECTORY.

  If FOLLOW-SYMLINKS is true, then the returned list contains
truenames (symlinks will be resolved) which essentially means that it
might also return files from *outside* the directory.  This works on
all platforms.

  When FOLLOW-SYMLINKS is NIL, it should return the actual directory
contents, which might include symlinks.  Currently this works on SBCL
and CCL."
  (declare (ignorable follow-symlinks))
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  #+(or :ecl :clasp)
  (let ((dir (pathname-as-directory dirname)))
    (concatenate 'list
                 (directory (merge-pathnames (pathname "*/") dir))
                 (directory (merge-pathnames (pathname "*.*") dir))))
  #-(or :ecl :clasp)
  (let ((wildcard (directory-wildcard dirname)))
    #+:abcl (system::list-directory dirname)
    #+:sbcl (directory wildcard :resolve-symlinks follow-symlinks)
    #+(or :cmu :scl :lispworks) (directory wildcard)
    #+(or :openmcl :digitool) (directory wildcard :directories t
                                                  :follow-links follow-symlinks)
    #+:allegro (directory wildcard :directories-are-files nil)
    #+:clisp (nconc (directory wildcard :if-does-not-exist :keep)
                    (directory (clisp-subdirectories-wildcard wildcard)))
    #+:cormanlisp (nconc (directory wildcard)
                         (cl::directory-subdirs dirname)))
  #-(or :sbcl :cmu :scl :lispworks :openmcl :allegro :clisp
        :cormanlisp :ecl :abcl :digitool :clasp)
  (error "LIST-DIRECTORY not implemented"))

(defun walk-directory (dirname fn &key directories
                                       (if-does-not-exist :error)
                                       (test (constantly t))
                                       (follow-symlinks t))
  "Recursively applies the function FN to all files within the
directory named by the non-wild pathname designator DIRNAME and all of
its sub-directories.  FN will only be applied to files for which the
function TEST returns a true value.  If DIRECTORIES is not NIL, FN and
TEST are applied to directories as well.  If DIRECTORIES
is :DEPTH-FIRST, FN will be applied to the directory's contents first.
If DIRECTORIES is :BREADTH-FIRST and TEST returns NIL, the directory's
content will be skipped. IF-DOES-NOT-EXIST must be one of :ERROR
or :IGNORE where :ERROR means that an error will be signaled if the
directory DIRNAME does not exist.  If FOLLOW-SYMLINKS is T, then your
callback will receive truenames.  Otherwise you should get the actual
directory contents, which might include symlinks.  This might not be
supported on all platforms.  See LIST-DIRECTORY."
  (labels ((walk (name)
             (cond
               ((directory-pathname-p name)
                ;; the code is written in a slightly awkward way for
                ;; backward compatibility
                (cond ((not directories)
                       (dolist (file (list-directory name
                                       :follow-symlinks follow-symlinks))
                         (walk file)))
                      ((eql directories :breadth-first)
                       (when (funcall test name)
                         (funcall fn name)
                         (dolist (file (list-directory name
                                         :follow-symlinks follow-symlinks))
                           (walk file))))
                      ;; :DEPTH-FIRST is implicit
                      (t (dolist (file (list-directory name
                                         :follow-symlinks follow-symlinks))
                           (walk file))
                         (when (funcall test name)
                           (funcall fn name)))))
               ((funcall test name)
                (funcall fn name)))))
    (let ((pathname-as-directory (pathname-as-directory dirname)))
      (case if-does-not-exist
        ((:error)
         (cond ((not (directory-exists-p pathname-as-directory))
                (error "File ~S does not exist."
                       pathname-as-directory))
               (t (walk pathname-as-directory))))
        ((:ignore)
         (when (directory-exists-p pathname-as-directory)
           (walk pathname-as-directory)))
        (otherwise
         (error "IF-DOES-NOT-EXIST must be one of :ERROR or :IGNORE."))))
    (values)))


;; Other pathname utilities (delete?)

(defsubst pathname-as-directory (pathname)
  "Alias for `uiop:ensure-directory-pathname'."
  (uiop:ensure-directory-pathname pathname))

(def +key-translations+
  '((:owner-read . :user-read)
    (:owner-write . :user-write)
    (:owner-execute . :user-exec)))

(defun file-permissions (pathname)
  "Wrap `org.shirakumo.attributes' for compatibility with
`osicat:file-permissions'."
  (let ((attrs (file:decode-attributes (file:attributes pathname))))
    (iter (for (key value . nil) on attrs by #'cddr)
          (when value
            (collect (or (assocdr key +key-translations+)
                         key))))))

(defun (setf file-permissions) (value pathname)
  "Wrap `org.shirakumo.attributes' for compatibility with
`osicat:file-permissions'."
  (let ((attrs-plist
          (iter (for key in value)
                (collect (or (rassocar key +key-translations+)
                             key))
                (collect t))))
    (setf (file:attributes pathname)
          (file:encode-attributes attrs-plist))))

(defun pathname-relativize (root-path path)
  "Return namestring of PATH relative to ROOT-PATH.  If ROOT-PATH's
directory is not a prefix of PATH, there is no effect.  ROOT-PATH or
PATH may be either pathnames or namestrings of pathnames."
  (let* ((path (canonical-pathname path))
         (root-dir (pathname-directory
                    (canonical-pathname
                     (ensure-directory-pathname root-path))))
         (path-dir (pathname-directory path))
         (root-dir-len (length root-dir)))
    (namestring
     (if (and (<= root-dir-len (length path-dir))
              (every #'equal root-dir path-dir)
              (equal (pathname-device root-path)
                     (pathname-device path)))
         (make-pathname :defaults path
                        :directory (cons :relative (subseq path-dir root-dir-len)))
         path))))

(defun directory-p (pathname)
  "Return a directory version of PATHNAME if it indicates a directory."
  (cond
    ((directory-pathname-p pathname) pathname)
    ;; Wild pathnames are not directory pathnames.
    ;; Also, `directory-exists-p' faults on wild pathnames.
    ((wild-pathname-p pathname) nil)
    ;; `directory-exists-p' (like this function) returns the pathname.
    (t (directory-exists-p (pathname-as-directory pathname)))))

(defun chdir (pathname)
  "First `chdir' to PATHNAME then set as `*default-pathname-defaults*'."
  (let ((pathname (truename (pathname-as-directory pathname))))
    (uiop/os:chdir pathname)
    (setf *default-pathname-defaults* pathname)))

(defun utimes (path access-time modify-time
               &optional (access-time-nsec 0) (modify-time-nsec 0))
  "Set access time, modify time of os file. These are the standard
 times with resolution of 1 second (as returned by 'stat'). You can optionally
 provide the nanosecond part of access time and modify time."
  (setf (file:access-time path) access-time
        (file:modification-time path) modify-time))

(defun copy-file-with-attributes (old new
                                  &key (permissions t)
                                    (user nil)
                                    (group t)
                                    (times t))
  "Copy an os file from old path to new, optionally maintaining permissions,
 owner, group and access/modify times. By default all attributes are copied,
 except user, as changing the user usually requires root privilege."
  ;; copy the file bytes
  (copy-file old new)
  ;; optionally copy file permissions
  (when permissions
    (setf (file-permissions new) (file-permissions old)))
  ;; preserve user, group
  (let* ((old-uid (file:owner old))
         (old-gid (file:group old))
         (atime (file:access-time old))
         (mtime (file:modification-time old)))
    (cond ((and user group)
           (setf (file:owner new) old-uid
                 (file:group new) old-gid))
          ((or user group)
           (let ((new-gid (file:group new))
                 (new-uid (file:owner new)))
             (setf (values (file:owner new)
                           (file:group new))
                   (if user
                       ;; preserve user, keep new group
                       (values old-uid new-gid)
                       ;; else preserve group, keep new user
                       (values new-uid old-gid))))))
    ;; copy access/modify times -- we truncate nanoseconds to
    ;; zero.
    (when times
      (utimes new atime mtime))))
