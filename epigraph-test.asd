
(asdf:defsystem :epigraph-test
    :name "epigraph-test"
    :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
    :version #.(with-open-file
                   (vers (merge-pathnames "version.lisp-expr" *load-truename*))
                 (read vers))
    :licence "BSD"
    :depends-on (epigraph)
    :components
    ((:module
      :test
      :components
      ((:cl-source-file "package")
       (:cl-source-file "epigraph-test" :depends-on (package))))))
