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
       adding and removing nodes, adding and removing edges between
       the nodes, and various ways of searching the graph.")
  (:h1 "Examples")

  (:h1 "Dictionary")

  (:h2 "Nodes")
  (:p "Instances of the class " (:code "NODE") " are relatively simple
  objects that have a (presumably unique, let's discuss this later,
  and presumably, at some point, every node will have to have a
  non-nil name, but we are not enforcing that at the moment. In
  addition, each node has a data slot for associating arbitrary data
  with the node.")

  (:h2 "Edges")
  (:p "Edges connect two nodes in a graph. Currently, edges are just
  represented by simple " (:code "CONS") "es, and know nothing about
  the graph in which they are found, nor do they have any knowledge of
  the \"type\" of edge they represent, such as directed, undirected,
  etc...")

  (:h2 "Graphs")
  (:p "Graphs contain nodes and edges. In order for a graph to
  represent its nodes and edges, it needs some way of associating the
  nodes and edges with the graph. Since nodes know nothing about the
  edges and graph(s) containing them, we must use the graph itself to
  represent this data.")
  ))
