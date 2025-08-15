;;; test.lisp --- tests for the `gt' package
(defpackage :gt/test
  (:nicknames :gt/test)
  (:use :common-lisp
        :curry-compose-reader-macros
        :named-readtables
        :stefil+
        :gt/filesystem
        :gt/misc
        :gt/shell
        #+gt :testbot)
  (:local-nicknames (:file :org.shirakumo.file-attributes))
  #+gt (:shadowing-import-from :testbot :batch-test)
  (:import-from :serapeum :dict :string^= :trim-whitespace)
  (:import-from :gt :equal? :lines)
  (:import-from :fset :seq)
  (:import-from :serapeum :drop-prefix)
  (:shadowing-import-from :fset :set)
  (:export :test :batch-test :testbot-test))
(in-package :gt/test)
(in-readtable :curry-compose-reader-macros)

(defroot test)
(defsuite test "GT top-level test suite.")

(deftest rebind-temp-dir-at-runtime-test ()
  (with-temporary-directory (:pathname outer)
    (let ((*temp-dir* outer))
      (with-temporary-file (:pathname inner)
        (is (string^= (namestring outer) (namestring inner)))))))

(deftest file-to-string-empty-file ()
  (with-temporary-file-of (:pathname tmp) ""
                          (is (equal "" (file-to-string tmp)))
                          (is (equal "" (file-to-string tmp :external-format :ascii)))))

(deftest file-to-string-non-utf8-encoding ()
  #-ccl       ; CCL silently reads w/o warning despite bad encoding...
  (is (string= (file-to-string (asdf:system-relative-pathname :gt "test/etc/latin-1.c"))
               "/* Here is a non-ASCII character: § */")))

(deftest file-to-bytes-test ()
  (with-temporary-file-of (:pathname tmp) ""
                          (equalp (file-to-bytes tmp) #())))

(defun equal-modulo-leading-private (left right)
  (equal (drop-prefix "/private" left) (drop-prefix "/private" right)))

(deftest string-to-file-test ()
  (with-temporary-file-of (:pathname tmp) ""
                          (is (equal-modulo-leading-private
                               (namestring (string-to-file "foo" tmp))
                               tmp))
                          (is (equal (file-to-string tmp) "foo"))))

(deftest file-bytes-roundtrip-test ()
  (is (null
       (loop repeat 20
          do (let* ((n (random 100))
                    (bytes (coerce (loop repeat n collect (random 256))
                                   '(vector (unsigned-byte 8)))))
               (with-temporary-file-of
                   (:pathname tmp) ""
                   (bytes-to-file bytes tmp)
                   (let ((seq (file-to-bytes tmp)))
                     (unless (and (= (length bytes) (length seq))
                                  (typep seq '(vector (unsigned-byte 8)))
                                  (every #'= bytes seq))
                       (return (list n bytes seq))))))))))

(deftest canonical-pathname-test ()
  (is (equal (namestring (canonical-pathname #p"")) ""))
  (is (equal (canonical-pathname #p"foo/") #p"foo/"))
  (is (equal (canonical-pathname #p"bar/../foo/") #p"foo/"))
  (let ((p #p"bar/../foo/"))
    (is (equal #p"foo/"
               (canonical-pathname
                (make-pathname
                 :directory (substitute :up :back (pathname-directory p))
                 :defaults p))))
    (is (equal #p"foo/"
               (canonical-pathname
                (make-pathname
                 :directory (substitute :back :up (pathname-directory p))
                 :defaults p)))))
  ;; Handle repeated ..
  (is (equal (canonical-pathname
              #p"/usr/bin/../lib/gcc/x86_64-linux-gnu/9/../../../../include/c++/9")
             #p"/usr/include/c++/9"))
  ;; Keep .. at the beginning of a relative pathname.
  (is (equal (canonical-pathname #p"../x/y")
             #p"../x/y"))
  (is (equal (canonical-pathname #p"../../../x/")
             (canonical-pathname #p"../../../x/")))
  (is (equal (canonical-pathname #p"x/../../y")
             (canonical-pathname #p"../y")))
  (is (equal (canonical-pathname #p"x/../../../y")
             (canonical-pathname #p"../../y")))
  ;; Drop .. at the beginning of an absolute pathhname.
  (is (equal (canonical-pathname #P"/../x/")
             #p"/x/"))
  (is (equal (canonical-pathname #p"/../../../x/")
             (canonical-pathname #p"/x/"))))

(deftest merge-pathname-as-directory-test ()
  (is (equal (merge-pathnames-as-directory) #p""))
  (is (equal (merge-pathnames-as-directory #p"" #p"d1/" #p"")
             #p"d1/"))
  (is (equal (merge-pathnames-as-directory #p"d1/" #p"d2/")
             #p"d1/d2/"))
  (is (equal (merge-pathnames-as-directory #p"/d1/" #p"d2/")
             #p"/d1/d2/"))
  (is (equal (merge-pathnames-as-directory #p"d1/" #p"/d2/")
             #p"/d2/")))

(deftest merge-pathname-as-file-test ()
  (is (equal (merge-pathnames-as-file) #p""))
  (is (equal (merge-pathnames-as-file #p"f.txt") #p"f.txt"))
  (is (equal (merge-pathnames-as-file #p"" #p"d1/" #p"f")
             #p"d1/f"))
  (is (equal (merge-pathnames-as-file #p"" #p"d1/" #p"f.txt")
             #p"d1/f.txt"))
  (is (equal (merge-pathnames-as-file #p"d1/g.c" #p"d2/h.cpp")
             #p"d1/d2/h.cpp"))
  (is (equal (merge-pathnames-as-file #p"/d1/" #p"d2/")
             #p"/d1/d2/"))
  (is (equal (merge-pathnames-as-file #p"d1/" #p"/d2/")
             #p"/d2/"))
  (is (equal (merge-pathnames-as-file #p"/d/"
                                      (make-pathname :directory nil
                                                     :name "f"
                                                     :type "c"
                                                     :version :newest))
             (make-pathname :directory '(:absolute "d")
                            :name "f"
                            :type "c"
                            :version :newest))))

(deftest directory-wildcard-test ()
  (is (handler-case (progn (directory-wildcard #p"*/") nil)
        (error () t)))
  #+sbcl
  (is (equal (directory-wildcard "d/") #p"d/*.*")))

(deftest list-directory-test ()
  (is (handler-case (progn (list-directory #p"*/") nil)
        (error () t)))
  (with-temporary-directory (:pathname d)
    (let ((f (merge-pathnames-as-file d "f.txt")))
      (string-to-file "" f)
      (is (equal-modulo-leading-private (list-directory d) (list f))))))

(deftest walk-directory-test ()
  (is (handler-case (progn (walk-directory #p"*/" (constantly nil)) nil)
        (error () t)))
  (is (handler-case (progn (walk-directory #p"/" (constantly nil)
                                           :if-does-not-exist :bogus))
        (error () t)))
  (is (handler-case (walk-directory #p"nonexistent/" (constantly nil))
        (error () t)))
  (is (null (multiple-value-list
             (walk-directory "nonexistent/" (constantly nil)
                             :if-does-not-exist :ignore))))
  (with-temporary-directory (:pathname d)
    (let ((f (merge-pathnames-as-file d "f.txt")))
      (string-to-file "" f)
      (is (null (multiple-value-list
                 (walk-directory d (constantly nil)))))
      (is (null (multiple-value-list
                 (walk-directory d (constantly nil)
                                 :if-does-not-exist :ignore))))
      (is (eql (let ((count 0))
                 (walk-directory d (lambda (x)
                                     (declare (ignore x))
                                     (incf count)))
                 count)
               1))
      (is (eql (let ((count 0))
                 (walk-directory d (lambda (x)
                                     (declare (ignore x))
                                     (incf count))
                                 :directories :breadth-first)
                 count)
               2))
      (is (eql (let ((count 0))
                 (walk-directory d (lambda (x)
                                     (declare (ignore x))
                                     (incf count))
                                 :directories :breadth-first
                                 :test (constantly nil))
                 count)
               0))
      (is (eql (let ((count 0))
                 (walk-directory d (lambda (x)
                                     (declare (ignore x))
                                     (incf count))
                                 :directories :depth-first
                                 :test (constantly nil))
                 count)
               0))
      (is (eql (let ((count 0))
                 (walk-directory d (lambda (x)
                                     (declare (ignore x))
                                     (incf count))
                                 :test (constantly nil))
                 count)
               0))
      (is (eql (let ((count 0))
                 (walk-directory d (lambda (x)
                                     (declare (ignore x))
                                     (incf count))
                                 :directories t)
                 count)
               2)))))

(deftest pathname-relativize-test ()
  (is (equal (pathname-relativize "/" "/foo") "foo"))
  (is (equal (pathname-relativize #p"/" "/foo") "foo"))
  (is (equal (pathname-relativize "/bar/" "/foo/bar/x.y") "/foo/bar/x.y")))

(deftest which-test ()
  (is (null (which "dsjafpoarue")))
  (is (not (null (which #-windows "ls" #+windows "cmd.exe")))))

#-windows
(deftest shell-directory-test ()
  (multiple-value-bind (stdout stderr errno)
      (shell "pwd" :directory "/tmp/")
    (is (equal-modulo-leading-private "/tmp" (trim-whitespace stdout)))
    (is (equal-modulo-leading-private "" stderr))
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
(deftest (read-and-write-shell-files :long-running) ()
  (let ((test-string "Hello world. Hello world. Hello world."))
    (is (string= test-string
                 (handler-case
                     (with-temporary-file (:pathname temp.xz)
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
(deftest (read-and-write-bytes-shell-files :long-running) ()
  (let ((byte #x25))
    (is (equal byte
               (handler-case
                   (with-temporary-file (:pathname temp.xz)
                     (write-shell-file (out temp.xz "xz")
                                       (write-byte byte out))
                     ;; NOTE: sleep one second to account for rare stochastic
                     ;; end-of-file errors in which I think we're going from
                     ;; writing to reading too quickly for the file system.
                     (sleep 1)
                     (read-shell-file (in temp.xz "xzcat")
                                      (read-byte in)))
                 (error (c) (declare (ignorable c)) nil))))))

#-windows
(deftest shell-input-test ()
  (is (equal (multiple-value-list (shell "cat" :input "foo"))
             '("foo" "" 0)))
  (is (equal
       (multiple-value-list
        (with-input-from-string (s "bar")
          (shell "cat" :input s)))
       '("bar" "" 0))))

#-windows
(deftest shell-bash-test ()
  (is (equal (multiple-value-list (shell "echo" :bash t))
             (list (string #\Newline) "" 0)))
  (is (equal (multiple-value-list (shell "cat" :bash t :input "x"))
             '("x" "" 0))))

#-windows
(deftest shell-debug-test ()
  (flet ((%r (s)
           (let ((pos (position #\Newline s)))
             (if pos (subseq s 0 pos) pos))))
    (is (equal
         (let ((gt/shell::*shell-debug* t))
           (%r (with-output-to-string (*standard-output*)
                 (shell "echo"))))
         "  cmd: echo"))
    (is (equal
         (let ((gt/shell::*shell-debug* t))
           (%r (with-output-to-string (*standard-output*)
                 (shell "echo" :input "foo"))))
         "  cmd: echo"))
    (is (equal
         (let ((gt/shell::*shell-debug* '(:cmd)))
           (%r (with-output-to-string (*standard-output*)
                 (shell "echo"))))
         "  cmd: echo"))
    (let ((gt/shell::*shell-debug* '(:input)))
      (is (search "  input:"
                  (with-output-to-string (*standard-output*)
                    (shell "echo" :input "bar")))))))

#-windows
(deftest shell-error-test ()
  (is (equal (multiple-value-list (shell "exit 1")) '("" "" 1)))
  (let ((gt/shell::*shell-non-error-codes* '(2)))
    (is (equal (multiple-value-list (shell "exit 2")) '("" "" 2))))
  (handler-case (shell "exit 3")
    (shell-command-failed (e)
      (is (equal (gt/shell::command e) "exit 3"))
      (is (equal (gt/shell::exit-code e) 126))
      (is (equal (gt/shell::stderr e) ""))))
  (handler-case (shell "exit 126")
    (shell-command-failed (e)
      (is (equal (gt/shell::command e) "exit 126"))
      (is (equal (gt/shell::exit-code e) 126))
      (is (equal (gt/shell::stderr e) ""))))
  (is
   (equal (multiple-value-list
           (handler-case (shell "exit 3")
             (shell-command-failed () (invoke-restart 'gt/shell::ignore-shell-error nil))))
          '("" "" 3))))

#-windows
(deftest kill-process-test ()
  (is
   (with-retries (10)
     (ignore-errors
       ;; kill all descendants (e.g. grandchildren), not just immediate children
       (let* ((script "FILE=$(mktemp) && echo $FILE && sh -c \"tail -f $FILE\"")
              (proc (launch-program script :output :stream))
              (filename (read-line (process-info-output proc))))
         (is (process-alive-p proc))
         #-darwin (is (string^= "/tmp/tmp." filename))
         (flet ((lsof () (length (lines (shell (format nil "lsof ~a" filename))))))
           (is (= 2 (lsof)))                 ; header line plus one process
           (kill-process proc)
           (is (not (process-alive-p proc)))
           (is (= 0 (lsof))))))
     (return t))))            ; header isn't printed when no processes

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

(deftest with-quiet-compilation-test ()
  (is (null (eval '(with-quiet-compilation nil)))))

(deftest without-compiler-notes-test ()
  (is (null (without-compiler-notes nil))))

(deftest arglist-test ()
  (is (equal (arglist 'arglist) '(gt/misc::fname))))

(deftest if-let*-test ()
  (is (equal (eval '(let ((x 1)) (if-let* ((x 2) (y 3)) (+ x y) x))) 5))
  (is (equal (eval '(let ((x 1)) (if-let* ((x 2) (y nil)) (+ x y) x))) 1))
  (is (equal (eval '(let ((x 1)) (if-let* ((x nil) (y 3)) (+ x y) x))) 1))
  (is (equal (eval '(let ((x 1)) (if-let* ((x 2) (x 3)) x x))) 3))
  (is (equal (eval '(let ((x 1)) (if-let* (x) 2 x))) 1))
  (is (equal (eval '(let ((x 1)) (if-let* (x nil) x x))) 1))
  (is (equal (eval '(let ((x 1)) (if-let* (x 2) x x))) 2))
  (is (equal (eval '(if-let* () 1 2)) 1)))

(deftest multiple-value-or-test ()
  (flet ((%t (forms result)
           (is (equal (multiple-value-list (eval (cons 'multiple-value-or forms)))
                      result))))
    (%t '(1) '(1))
    (%t nil '(nil))
    (%t '(nil) '(nil))
    (%t '((values 1 2)) '(1 2))
    (%t '((values 1 2) 3) '(1 2))
    (%t '((values 1 2) nil) '(1 2))
    (%t '(nil (values 1 2) nil) '(1 2))
    (%t '((values)) nil)
    (%t '(1 (values 2 3)) '(1))))

(deftest symbol-cat-test ()
  (let ((*package* (find-package :keyword)))
    (is (equal (symbol-cat :x :y) :x-y))
    (is (equal (symbol-cat :|x|) :|X|))
    (is (equal (symbol-cat) :||))))

(deftest symbol-cat-in-package-test ()
  (let ((pkg (find-package :keyword)))
    (is (equal (symbol-cat-in-package pkg :x :y) :x-y))
    (is (equal (symbol-cat-in-package pkg :x) :x))
    (is (equal (symbol-cat-in-package pkg :|x| :|y| :z) :x-y-z))
    (is (equal (symbol-cat-in-package pkg) :||))))

(deftest plist-get-test ()
  (is (equal (plist-get :a nil) nil))
  (is (equal (plist-get :a nil) nil))
  (is (equal (plist-get :a '(:a 1)) 1))
  (is (equal (plist-get :b '(:a 1)) nil))
  (is (equal (plist-get :b '(:a 1 :b 2 :c 3)) 2))
  (is (equal (plist-get :b '(:a :b :c 3)) nil))
  (is (equal (plist-get "B" '(:a 1 :b 2 :c 3) :test 'string=) 2))
  (is (handler-case (progn (plist-get :a '(:b)) nil)
        (error (e) e))))

(deftest plist-drop-if-test ()
  (is (equal (plist-drop-if #'keywordp nil) nil))
  (is (equal (plist-drop-if #'keywordp '(:a 1)) nil))
  (is (equal (plist-drop-if #'keywordp '(x 2 :a 1 y 3)) '(x 2 y 3)))
  (is (equal (plist-drop-if #'keywordp '(x :b :a 1 y 3)) '(x :b y 3)))
  (is (handler-case (progn (plist-drop-if #'null '(:a 1 :b)) nil)
        (error (e) e))))

(deftest plist-drop-test ()
  (is (equal (plist-drop :a nil) nil))
  (is (equal (plist-drop :a '(:a 1)) nil))
  (is (equal (plist-drop :a '(:a 1 :b 2)) '(:b 2)))
  (is (equal (plist-drop :a '(:b 2 :a 1)) '(:b 2)))
  (is (equal (plist-drop :a '(:b :a :a 1)) '(:b :a)))
  (is (equal (plist-drop "A" '(:b :a :a 1 :|a| 3) :test #'string=)
             '(:b :a :|a| 3))))

(deftest plist-merge-test ()
  (is (equal (plist-merge nil nil) nil))
  (is (equal (plist-merge '(:a 1) nil) '(:a 1)))
  (is (equal (plist-merge nil '(:a 1)) '(:a 1)))
  (is (equal (plist-merge '(:a 1) '(:b 2)) '(:a 1 :b 2)))
  (is (equal (plist-merge '(:a 1) '(:a 2)) '(:a 1))))

(deftest aget-test ()
  (is (equal (aget :a nil) nil))
  (is (equal (aget 1 nil) nil))
  (is (equal (aget :a '((:a . 1))) 1))
  (is (equal (aget :a '((:b . 1) (:a . 2) (:a . 3))) 2))
  (is (equal (aget :a '((:a . 1) (:b . 2)) :test (complement #'eql)) 2))

  (is (equal (eval '(aget :a nil)) nil))
  (is (equal (eval '(aget :a '((:a . 1)))) 1))
  (is (equal (eval '(aget :a '((:b . 1)) :test (complement #'eql))) 1))
  (is (equal (eval '(let ((x :a)) (aget x '((:a . 1))))) 1))

  (locally (declare (notinline aget)) ;; disable compiler macro for aget
    (is (equal (aget :a nil) nil))
    (is (equal (aget 1 nil) nil))
    (is (equal (aget :a '((:a . 1))) 1))
    (is (equal (aget :a '((:b . 1) (:a . 2) (:a . 3))) 2))
    (is (equal (aget :a '((:a . 1) (:b . 2)) :test (complement #'eql)) 2))))

(deftest aget-setf-test ()
  (let (x)
    (is (equal (setf (aget :a x) 1) 1))
    (is (equal x '((:a . 1)))))
  (is (equal (eval '(let (x) (setf (aget :a x) 1))) 1))
  (is (equal (eval '(let (x) (setf (aget :a x) 1) x)) '((:a . 1)))))

(deftest areplace-test ()
  (let ((x (list (cons :a 1) (cons :b 2) (cons :c 3))))
    (is (equal (areplace :a 4 x) '((:a . 4) (:b . 2) (:c . 3))))
    (is (equal (areplace :b 4 x) '((:b . 4) (:a . 1) (:c . 3))))
    (is (equal (areplace :c 4 x) '((:c . 4) (:a . 1) (:b . 2))))
    (is (equal (areplace :d 4 x) '((:d . 4) (:a . 1) (:b . 2) (:c . 3))))
    (is (equal x '((:a . 1) (:b . 2) (:c . 3))))))

(deftest adrop-test ()
  (let ((x (list (cons 1 :a) (cons :a 2) (cons :b 3) (cons :c 4))))
    (is (equal (adrop nil x) x))
    (is (equal (adrop '(:z) x) x))
    (is (equal (adrop '(1 :a :b :c) x) nil))
    (is (equal (adrop '(2) x) x))))

(deftest alist-filter-test ()
  (let ((x (list (cons 1 :a) (cons :a 2) (cons :b 3) (cons :c 4))))
    (is (equal (alist-filter nil x) nil))
    (is (equal (alist-filter '(:z) x) nil))
    (is (equal (alist-filter '(1 :a :b :c) x) x))
    (is (equal (alist-filter '(1) x) '((1 . :a))))
    (is (equal (alist-filter '(:a) x) '((:a . 2))))))

(deftest getter-test ()
  (let ((fn (getter ':a)))
    (is (equal (funcall fn nil) nil))
    (is (equal (funcall fn '((:a . 1))) 1))
    (is (equal (funcall fn '((:b . 2))) nil))
    (is (equal (funcall fn '((:b . 2) (:a . 3))) 3))))

(deftest counts-test ()
  (is (equal (counts nil) nil))
  (is (equal (counts '(:a)) '((:a . 1))))
  (is (equal (counts '(:a) :frac t) '((:a . 1))))
  (is (equal (counts '(:a :a) :frac t) '((:a . 1))))
  (is (equal (counts '(:a :b)) '((:b . 1) (:a . 1))))
  (is (equal (counts '(:a :b) :frac t) '((:b . 1/2) (:a . 1/2)))))

(deftest proportional-pick-test ()
  (is (equal (proportional-pick '(1) #'identity) 0)))

(deftest random-bool-test ()
  (is (random-bool 1.0))
  (is (not (random-bool 0.0)))
  (is (<= 20 (loop repeat 100 count (random-bool)) 80)))

;;;------------------------------------------------------

(deftest replace-all-test ()
  (flet ((%t (s p r expected &optional (test nil test-p))
           (flet ((%t2 ()
                    (if test-p
                        (is (equal (replace-all s p r :test test) expected))
                        (is (equal (replace-all s p r) expected)))))
             (%t2)
             (setf s (coerce s 'base-string)
                   p (coerce p 'base-string)
                   r (coerce r 'base-string))
             (%t2))))
    (%t "" "x" "y" "")
    (%t "x" "x" "y" "y")
    (%t "xxx" "x" "y" "yyy")
    (%t "xxx" "xx" "y" "yx")
    (%t "XXX" "xx" "y" "yX" #'char-equal)))

(deftest apply-replacements-test ()
  (is (equal (apply-replacements nil "") ""))
  (is (equal (apply-replacements nil "x") "x"))
  (is (equal (apply-replacements '((nil . "z")) "x") "x"))
  (is (equal (apply-replacements '(("x" . "y")) "x") "y")))

(deftest repeatedly-test ()
  (is (equal (eval '(let ((x nil)) (repeatedly 5 (push :a x)) x)) '(:a :a :a :a :a)))
  (is (equal (eval '(let ((x nil)) (repeatedly 0 (push :a x)) x)) nil))
  (is (equal (eval '(let ((x nil)) (repeatedly -1 (push :a x)) x)) nil)))

(deftest indexed-test ()
  (is (equal (indexed nil) nil))
  (is (equal (indexed '(:a :b :c)) '((0 :a) (1 :b) (2 :c)))))

(deftest transpose-test ()
  (is (equal (transpose '((:a :b :c))) '((:a) (:b) (:c))))
  (is (equal (transpose '((:a) (:b) (:c))) '((:a :b :c))))
  (is (equal (transpose '((:a :b) (:c :d))) '((:a :c) (:b :d)))))

(deftest interleave-test ()
  (is (equal (interleave nil t) nil))
  (is (equal (interleave '(:a) t) '(:a)))
  (is (equal (interleave '(:a :b :c) t) '(:a t :b t :c))))

(deftest chunks-test ()
  (is (equal (chunks nil 1) nil))
  (is (equal (chunks '(:a) 1) '((:a))))
  (is (equal (chunks '(:a) 2) nil))
  (is (equal (chunks '(:a) 2 t) '((:a))))
  (is (equal (chunks '(:a :b :c) 2) '((:a :b))))
  (is (equal (chunks '(:a :b :c) 2 t) '((:a :b) (:c)))))

(deftest binary-search-test ()
  (is (null (binary-search 1 #())))
  (is (null (binary-search 0 #(1))))
  (is (null (binary-search 1 #(0))))
  (is (equal (binary-search 0 #(0)) 0))
  (is (equal (binary-search 0 #(0 1 2)) 0))
  (is (equal (binary-search 0 #(0 1 2) :low 1) nil))
  (is (equal (binary-search 1 #(0 1 2)) 1))
  (is (equal (binary-search 2 #(0 1 2)) 2))
  (is (equal (binary-search 2 #(0 1 2) :high 1) nil))
  (is (equal (binary-search -1 #(0 1 2)) nil))
  (is (equal (binary-search 4 #(0 1 2)) nil))
  (is (equal (binary-search 3 #(0 10 20)) nil))
  (is (equal (binary-search 15 #(0 10 20)) nil)))

(deftest levenshtein-distance-test ()
  (is (equal (levenshtein-distance "" "") 0))
  (is (equal (levenshtein-distance "x" "") 1))
  (is (equal (levenshtein-distance "" "y") 1))
  (is (equal (levenshtein-distance "x" "y") 1))
  (is (equal (levenshtein-distance "ax" "ay") 1))
  (is (equal (levenshtein-distance "xab" "aby") 2))
  (is (equal (levenshtein-distance "abx" "yab") 2)))

(deftest tails-test ()
  (is (equal (tails nil) nil))
  (is (equal (tails '(:a)) '((:a))))
  (is (equal (tails '(:a :b :c)) '((:a :b :c) (:b :c) (:c)))))

(deftest pairs-test ()
  (is (equal (pairs nil) nil))
  (is (equal (pairs '(:a)) nil))
  (is (equal (pairs '(:a :b :c)) '((:a . :b) (:a . :c) (:b . :c)))))

(deftest mapcar-improper-list-test ()
  (is (equal (mapcar-improper-list #'1+ nil) nil))
  (is (equal (mapcar-improper-list #'1+ 1) 2))
  (is (equal (mapcar-improper-list #'1+ '(1)) '(2)))
  (is (equal (mapcar-improper-list #'1+ '(1 . 3)) '(2 . 4))))

(deftest split-quoted-test ()
  (is (equal (split-quoted "") nil))
  (is (equal (split-quoted "a b") '("a" "b")))
  (is (equal (split-quoted "a\\ b") '("a\\ b")))
  (is (equal (split-quoted "\"a b\"") '("\"a b\"")))
  (is (equal (split-quoted "a ' ' b") '("a" "' '" "b"))))

(deftest escape-string-test ()
  (is (equal (escape-string "") ""))
  (is (equal (escape-string "x") "x")))

(deftest unescape-string-test ()
  (is (equal (unescape-string "") ""))
  (is (equal (unescape-string "x") "x")))

(deftest escape-chars-test ()
  (is (equal (escape-chars "" "") ""))
  (is (equal (escape-chars "" "\\") "\\"))
  (is (equal (escape-chars "y" "xyz") "x\\yz")))

(deftest make-thread-safe-hash-table-test ()
  (let ((h (make-thread-safe-hash-table)))
    (is (hash-table-p h))
    (is (eql (hash-table-count h) 0))
    (is (null (gethash :a h)))
    (is (equal (setf (gethash :a h) 1) 1))
    (is (eql (hash-table-count h) 1))
    (is (eql (gethash :a h) 1))))

(deftest random-hash-table-key-test ()
  (is (null (random-hash-table-key (make-hash-table))))
  (let* ((keys (loop for i from 0 to 9 collect i))
         (h (make-hash-table)))
    (dolist (k keys)
      (setf (gethash k h) (+ k 100)))
    (is (null
         (loop repeat 100
            do (let ((k (random-hash-table-key h)))
                 (unless (member k keys)
                   (return k))))))))

;;; Double our equality tests by always testing both ways.

(defun strong-equal? (a b)
  "Bijective equality: A=B and B=A."
  (and (equal? a b)
       (equal? b a)))

(defun weak-equal? (a b)
  "True if A=B or B=A."
  (or (equal? a b)
      (equal? b a)))

(deftest equal?-test ()
  (is (strong-equal? 1 1))
  (is (not (weak-equal? 1 2)))
  (is (strong-equal? #\a #\a))
  (is (not (weak-equal? #\a #\b)))
  (is (strong-equal? "foo" "foo"))
  (is (not (weak-equal? "foo" "bar")))
  (is (strong-equal? (seq 1 2 3) (seq 1 2 3)))
  (is (not (weak-equal? (seq 1 2 3) (seq 1 2 4))))
  (is (not (weak-equal? (seq 1 2 3) (seq 1 2 3 4))))
  (is (strong-equal? (list 1 2 3) (list 1 2 3)))
  (is (not (weak-equal? (list 1 2 3) (list 1 2 4))))
  (is (not (weak-equal? (list 1 2 3) (list 1 2 3 4))))
  (is (strong-equal? '(a . b) '(a . b)))
  (is (not (weak-equal? '(a . b) '(a . (b . c)))))
  (is (strong-equal? #(1 2) #(1 2)))
  (is (not (weak-equal? #(1 2) #(1 2 3))))
  (is (not (weak-equal? #(2 2) #(1 2)))))

(deftest hash-table-equal?-test ()
  (let ((a-equalp (make-hash-table :test #'equalp))
        (a (make-hash-table))
        (b-equalp (make-hash-table :test #'equalp))
        (b (make-hash-table)))

    (is (not (weak-equal? a-equalp a)))

    (mapc (lambda (pair)
            (destructuring-bind (key . value) pair
              (setf (gethash key a) value)
              (setf (gethash key b) value)))
          '((a . 1)
            (b . 2)
            (c . 3)))

    (is (strong-equal? a b))
    (setf (gethash 'd b) 4)
    (is (not (weak-equal? a b)))

    (mapc (lambda (pair)
            (destructuring-bind (key . value) pair
              (setf (gethash key a-equalp) value)
              (setf (gethash key b-equalp) value)))
          '(("foo" . 1)
            ("bar" . 2)
            ("baz" . 3)))

    (is (strong-equal? a-equalp b-equalp))
    (setf (gethash "foo" b-equalp) 0)
    (is (not (weak-equal? a-equalp b-equalp)))))

(defclass example-object ()
  ((foo :initarg :foo :accessor foo)
   (bar :initarg :bar :accessor bar)))

(defmethod fset:compare ((a example-object) (b example-object))
  (fset:compare-slots a b #'foo #'bar))

(deftest object-equal?-test ()
  (let ((a (make-instance 'example-object :foo 1 :bar 2))
        (a2 (make-instance 'example-object :foo 1 :bar 2))
        (b (make-instance 'example-object :foo 2 :bar 1)))
    (is (strong-equal? a a))
    (is (strong-equal? b b))
    (is (strong-equal? a a2))
    (is (strong-equal? a2 a))
    (is (not (weak-equal? a b)))
    (is (not (weak-equal? b a)))))

(deftest collection-equal?-test ()
  ;; Seqs.
  (is (strong-equal? (seq) (seq)))
  (is (not (weak-equal? (seq) (seq 1))))
  (is (not (weak-equal? (seq 1) (seq))))
  (is (not (weak-equal? (seq 1 2) (seq 2 1))))
  (is (strong-equal? (seq 1) (seq 1)))
  (is (strong-equal? (seq (make-instance 'example-object :foo 1 :bar 1))
                     (seq (make-instance 'example-object :foo 1 :bar 1))))
  (is (not (weak-equal? (seq (make-instance 'example-object :foo 1 :bar 2))
                        (seq (make-instance 'example-object :foo 2 :bar 1)))))
  (is (not (weak-equal? (seq (make-instance 'example-object :foo 2 :bar 1))
                        (seq (make-instance 'example-object :foo 1 :bar 2)))))
  ;; Sets.
  (is (strong-equal? (set) (set)))
  (is (not (weak-equal? (set 1) (set))))
  (is (not (weak-equal? (set) (set 1))))
  (is (strong-equal? (set 1) (set 1)))
  (is (strong-equal? (set 1 2) (set 2 1)))
  (is (strong-equal? (set (make-instance 'example-object :foo 1 :bar 1)
                          (make-instance 'example-object :foo 2 :bar 2))
                     (set (make-instance 'example-object :foo 1 :bar 1)
                          (make-instance 'example-object :foo 2 :bar 2))))
  (is (strong-equal? (set (make-instance 'example-object :foo 1 :bar 1)
                          (make-instance 'example-object :foo 2 :bar 2))
                     (set (make-instance 'example-object :foo 2 :bar 2)
                          (make-instance 'example-object :foo 1 :bar 1))))
  (is (strong-equal? (set (make-instance 'example-object :foo 1 :bar 1))
                     (set (make-instance 'example-object :foo 1 :bar 1))))
  (is (not (weak-equal? (set (make-instance 'example-object :foo 1 :bar 2))
                        (set (make-instance 'example-object :foo 2 :bar 1)))))
  (is (not (weak-equal? (set (make-instance 'example-object :foo 2 :bar 1))
                        (seq (make-instance 'example-object :foo 1 :bar 2)))))
  ;; Maps
  (is (strong-equal? (fset:map) (fset:map)))
  (is (strong-equal? (fset:map (:x 1)) (fset:map (:x 1))))
  (is (strong-equal?
       (fset:map (:x (make-instance 'example-object :foo 1 :bar 1)))
       (fset:map (:x (make-instance 'example-object :foo 1 :bar 1)))))
  (is (not (weak-equal?
            (fset:map (:x (make-instance 'example-object :foo 1 :bar 1)))
            (fset:map (:x (make-instance 'example-object :foo 1 :bar 2))))))
  (is (strong-equal?
       (fset:map ((make-instance 'example-object :foo 1 :bar 1) 1))
       (fset:map ((make-instance 'example-object :foo 1 :bar 1) 1))))
  (is (not (weak-equal?
            (fset:map ((make-instance 'example-object :foo 1 :bar 1) 1))
            (fset:map ((make-instance 'example-object :foo 1 :bar 2) 1)))))
  (is (not (weak-equal? (fset:map (:x 1)) (fset:map (:x 2)))))
  (is (not (weak-equal? (fset:map (:x 1)) (fset:map (:y 1)))))
  ;; Cross-types.
  (is (not (weak-equal? (seq) (set))))
  (is (not (weak-equal? (seq) (fset:map))))
  (is (not (weak-equal? (set) (fset:map))))
  (is (not (weak-equal? (set) (fset:bag))))
  (is (not (weak-equal? (fset:map) (fset:bag))))
  (is (not (weak-equal? (seq 1) (set 1))))
  (is (not (weak-equal? (set 1) (fset:bag 1))))
  (is (not (weak-equal? (fset:map (:x 1)) (fset:bag :x)))))

(fset:define-tuple-key +K0+)

(deftest tuple-equal?-test ()
  (is (strong-equal? (fset:empty-tuple) (fset:empty-tuple)))
  (is (strong-equal? (fset:tuple (+K0+ 0)) (fset:tuple (+K0+ 0))))
  (is (not (weak-equal? (fset:tuple (+K0+ 0)) (fset:tuple (+K0+ 1)))))
  ;; Dictionaries aren't equal per FSet, so this tests we're actually
  ;; descending with equal?.
  (is (strong-equal? (fset:tuple (+K0+ (dict :x 1)))
                     (fset:tuple (+K0+ (dict :x 1)))))
  (is (not (weak-equal? (fset:tuple (+K0+ (dict :x 1)))
                        (fset:tuple (+K0+ (dict :x 2))))))
  ;; Cross-type.
  (is (not (weak-equal? (fset:tuple) (fset:map))))
  (is (not (weak-equal? (fset:map (+K0+ 0)) (fset:tuple (+K0+ 0)))))
  (is (not (weak-equal? (fset:tuple) (set))))
  (is (not (weak-equal? (fset:tuple) (seq))))
  (is (not (weak-equal? (fset:tuple) (fset:bag)))))

(deftest copy-file-with-attributes-test ()
  (let ((permissions '(:USER-READ :USER-WRITE :USER-EXEC
                       :GROUP-READ :GROUP-EXEC :OTHER-READ :OTHER-EXEC))
        (access-time 1625150495)
        (modify-time 1625150491)
        (file-contents "hello"))
    (with-temporary-file (:pathname file1)
      (with-temporary-file (:pathname file2)
        (with-open-file (os file1 :direction :output :if-exists :supersede)
          (format os "~A~%" file-contents))
        (setf (gt/filesystem:file-permissions file1) permissions)
        (gt/filesystem::utimes file1 access-time modify-time)
        ;; copy the file
        (copy-file-with-attributes file1 file2)
        ;; make sure file2 contains the data, correct times, and permissions

        ;; check that times and permissions are the same
        (is (= (file:access-time file1)
               (file:access-time file2)))
        (is (= (file:modification-time file1)
               (file:modification-time file2)))
        (is (equal (gt/filesystem:file-permissions file1)
                   (gt/filesystem:file-permissions file2)))
        ;; check that data is the same
        (with-open-file (is1 file2 :direction :input)
          (with-open-file (is2 file1 :direction :input)
            (is (= (file-length is1) (file-length is2))) ; file lengths equal?
            (is (and (string= (read-line is1) file-contents)
                     (string= (read-line is2) file-contents)))))))))

(deftest extendable-string-test ()
  (let ((xstr (extendable-string "san francisco")))
    (dotimes (i 10) (vector-push-extend #\x xstr))
    (is (string-equal "san franciscoxxxxxxxxxx" xstr))
    (is (adjustable-array-p xstr))
    (is (array-has-fill-pointer-p xstr))))

(deftest time-as-string-test ()
  (multiple-value-bind (result time)
    (time-as-string
     (dotimes (i 1000 'foo) (cons i i)))
    (is (stringp time))
    (is (> (length time) 0))
    (is (eq result 'foo)))
  (multiple-value-bind (a b c time)
      (time-as-string
       (values 10 20 30))
    (is (eql a 10))
    (is (eql b 20))
    (is (eql c 30))
    (is (stringp time))
    (is (> (length time) 0))))
