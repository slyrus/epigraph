
(asdf:defsystem :epigraph-test
    :name "epigraph-test"
    :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
    :version "0.0.2"
    :licence "BSD"
    :depends-on (epigraph)
    :components
    ((:module
      :test
      :components
      ((:cl-source-file "package")
       (:cl-source-file "epigraph-test" :depends-on (package))))))
