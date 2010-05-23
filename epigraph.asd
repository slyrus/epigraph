
(asdf:defsystem :epigraph
    :name "epigraph"
    :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
    :version "0.0.2"
    :licence "BSD"
    :depends-on (alexandria)
    :components
    ((:static-file "version" :pathname #p"version.lisp-expr")
     (:static-file "COPYRIGHT")
     (:static-file "README")
     (:cl-source-file "package")
     (:cl-source-file "utilities" :depends-on (package))
     (:cl-source-file "epigraph" :depends-on (package utilities))))
