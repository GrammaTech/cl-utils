(defsystem "gt"
  :description "GrammaTech utilities."
  :version "0.0.0"
  :author "GrammaTech"
  :licence "MIT"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:gt/gt)
  :in-order-to ((test-op (test-op "gt/test"))))

(defsystem "gt/test"
    :author "GrammaTech"
    :licence "MIT"
    :description "Test the GT package."
    :perform (test-op (o c) (symbol-call :gt/test '#:test)))

(register-system-packages "misc-extensions" '(:gmap))
