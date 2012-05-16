
(asdf:defsystem :epigraph
  :name "epigraph"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version "0.0.2"
  :licence "BSD"
  :depends-on (alexandria)
  :in-order-to ((asdf:test-op (asdf:load-op :epigraph-test)))
  :perform (asdf:test-op :after (op c)
                         (map nil (intern (symbol-name '#:run!) '#:5am)
                              '(:epigraph)))
  :serial t
  :components
  ((:static-file "version" :pathname #p"version.lisp-expr")
   (:static-file "COPYRIGHT")
   (:static-file "README")
   (:cl-source-file "package")
   (:cl-source-file "utilities")
   (:cl-source-file "epigraph")
   (:cl-source-file "simple-edge-list-graph")))

(asdf:defsystem :epigraph-test
  :depends-on (fiveam epigraph)
  :components
  ((:module
    :test
    :components
    ((:cl-source-file "package")
     (:cl-source-file "epigraph-test" :depends-on (package))))))
