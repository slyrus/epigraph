((:smarkup-metadata
  (:copyright
   "Copyright 2008, Cyrus Harmon. All Rights Reserved. See COPYRIGHT
  file for details")
  (:title "epigraph  A library for representing and processing graphs,
  nodges and edges.")
  (:author "Cyrus L. Harmon"))
 (:html-metadata (:htmlcss "simple.css") )
 
 (:span
  (:title "Epigraph")
  (:p "Epigraph is relatively simple, small, and, hopefully,
       lightweight package for representing graphs in common lisp. A
       graph, in the computer science sense, not the visualization of
       points, lines, etc... in a cartesian coordinate system, is a
       set of ndoes of edges. Epigraph is designed for representing
       graphs and for performing operations on the graphs, such as
       adding and removing nodes (objects), adding and removing edges
       between the nodes, and various ways of searching the graph.")
  (:h1 "Examples")

  (:h1 "Dictionary")

  (:h2 "Graphs")

  (:p "Graphs contain nodes and edges. In order for a graph to
  represent its nodes and edges, it needs some way of associating the
  nodes and edges with the graph. Since nodes know nothing about the
  edges and graph(s) containing them, we must use the graph itself to
  represent this data.")

  (:p "Epigraph has a node-agnostic graph model, that is to say the
  graph class doesn't explicitly provide (or demand) any support for a
  node object, as such. In other words, any lisp value can be used as
  a node. For various computations on graphs we will need to be able
  to find and compare the nodes with other lisp values. Therefore we
  need to consider the kind of equality used by graphs in comparing
  nodes. Just as lisp hash-tables have a :test paramter, we add an
  optional :test parameter to the various ways of constructing
  graphs. In this way if a graph will only use lisp objects as keys it
  can do so by comparing the nodes using the lisp eq operator (or
  eql, equal, etc...). If a graph hopes to use strings as the key, the
  nodes will need to be compared using the equal operator. For a
  common simple case in which the nodes are just the strings
  themselves, we can make the graph as follows:")

  (:code "(make-graph :node-test 'equal)")

  (:h2 "Edges")
  
  (:p "Edges connect two nodes in a graph. Currently, edges are just
  represented by simple " (:code "CONS") "es, and know nothing about
  the graph in which they are found, nor do they have any knowledge of
  the \"type\" of edge they represent, such as directed, undirected,
  etc...")
  
  (:pre
   (:code
    #q{(defclass edge ()
  ((graph :accessor edge-graph :initarg :graph)
   (node1 :accessor node1 :initarg :node1)
   (node2 :accessor node2 :initarg :node2)
   (data :accessor edge-data :initarg :data :initform nil))
  (:documentation "Instances of the edge class represent edges between
  nodes in a graph."))}))))

