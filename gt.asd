(defsystem "gt"
  :description "GrammaTech utilities."
  :version "0.0.0"
  :author "GrammaTech"
  :licence "MIT"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:gt/gt)
  :in-order-to ((test-op (load-op "gt/test")))
  :perform (test-op (o c) (symbol-call :gt/test '#:test)))

(register-system-packages "misc-extensions" '(:gmap))
