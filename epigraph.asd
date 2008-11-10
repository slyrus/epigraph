
(asdf:defsystem :epigraph
    :name "epigraph"
    :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
    :version #.(with-open-file
                   (vers (merge-pathnames "version.lisp-expr" *load-truename*))
                 (read vers))
    :licence "BSD"
    :depends-on (alexandria)
    :components
    ((:static-file "version" :pathname #p"version.lisp-expr")
     (:static-file "COPYRIGHT")
     (:static-file "README")
     (:cl-source-file "package")
     (:cl-source-file "utilities" :depends-on (package))
     (:cl-source-file "epigraph" :depends-on (package utilities))))
