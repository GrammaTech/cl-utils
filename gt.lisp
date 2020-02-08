(uiop/package:define-package :gt
    (:nicknames :gt/gt)
  (:use-reexport :common-lisp :alexandria :serapeum :closer-mop
                 :trivia :iterate :fset :gmap :split-sequence :cl-ppcre
                 :named-readtables :curry-compose-reader-macros)
  (:shadow :~> :~~>)                    ; Shadow serapeum arrows.
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
  (:shadowing-import-from :closer-mop :defmethod
                          :defgeneric :standard-generic-function
                          :slot-definition-name :class-slots)
  (:shadowing-import-from :fset
                          :@
                          :unionf :appendf :with :removef
			  ;; Shadowed type/constructor names.
			  :set
			  ;; Shadowed set operations.
			  :union :intersection :set-difference :complement
			  ;; Shadowed sequence operations.
			  :first :last :subseq :reverse :sort :stable-sort
			  :reduce
			  :find :find-if :find-if-not
			  :count :count-if :count-if-not
			  :position :position-if :position-if-not
			  :remove :remove-if :remove-if-not
			  :substitute :substitute-if :substitute-if-not
			  :some :every :notany :notevery
                          ;; Shadowed sequence operations from serapeum.
                          :concat :range :partition :filter)
  (:shadowing-import-from :cl-ppcre
                          :scan))       ; Shadow serapeum:scan.

;;; TODO: :@ from fset but include the serapeum functionality.
;;; TODO: Define MAPCONCAT over fset sequences
