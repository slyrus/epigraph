
(asdf:operate 'asdf:load-op :ch-asdf)
(asdf:operate 'asdf:load-op :smarkup)

(defpackage #:epigraph-doc-system (:use #:cl #:asdf #:ch-asdf #:smarkup))
(in-package #:epigraph-doc-system)

#.(smarkup::enable-quote-reader-macro)

(defsystem :epigraph-doc
  :name "epigraph-doc"
  :author "Cyrus Harmon" 
  :version "0.0.2"
  :depends-on (ch-asdf smarkup)
  :components
  ((:module
    "doc"
    :components
    ((:object-from-file :epigraph-doc-sexp
                        :pathname #p"epigraph-doc.sexp")

     (:filtered-object :epigraph-doc-filtered-sexp
                       :filters (:lisp :smarkup-metadata :html-metadata)
                       :depends-on (:epigraph-doc-sexp)
                       :input-object :epigraph-doc-sexp)
   
     (:filtered-object :epigraph-doc-html-filtered-sexp
                       :filters (:html-metadata)
                       :depends-on (:epigraph-doc-filtered-sexp)
                       :input-object :epigraph-doc-filtered-sexp)

     (:object-xhtml-file :epigraph-doc-xhtml
                         :pathname #p"index.xhtml"
                         :depends-on (:epigraph-doc-filtered-sexp)
                         :input-object :epigraph-doc-filtered-sexp)

     (:object-cl-pdf-file :epigraph-doc-pdf
                          :pathname #p"epigraph.pdf"
                          :depends-on (:epigraph-doc-filtered-sexp)
                          :input-object :epigraph-doc-filtered-sexp)
     (:static-file :simple-css
                   :pathname #p "simple.css")))))

