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
  (:p "Epigraph is written by Cyrus Harmon and can currently be found at: "
      (:a
       :href
       "http://git.cyrusharmon.org/cgi-bin/gitweb.cgi?p=epigraph.git"
       "http://git.cyrusharmon.org/cgi-bin/gitweb.cgi?p=epigraph.git"))
  
  (:p "Epigraph is still in its early stages of development and the
  API may still change drastically.")

  (:h1 "Examples")

  (:pre
   (:code #q{(let ((g (make-graph :node-test 'equal)))
   (add-node g "n1")
   (add-node g "n2")
   (add-edge-between-nodes g "n1" "n2")
   (add-edge-between-nodes g "n1" "n2")
   (add-edge-between-nodes g "n1" "n1")
   (describe g)
   (find-self-edges g "n1"))}))
  
  (:pre
   (:code #q{(defparameter *graph* 
   (let ((g (make-graph :node-test 'equal)))
     (loop for i from 1 below 12
        do (add-node g (concatenate 'string "Node " (princ-to-string i))))
     (add-edge-between-nodes g "Node 1" "Node 2")
     (add-edge-between-nodes g "Node 1" "Node 3")
     (add-edge-between-nodes g "Node 3" "Node 4")
     (add-edge-between-nodes g "Node 3" "Node 5")
     (add-edge-between-nodes g "Node 2" "Node 6")
     (add-edge-between-nodes g "Node 6" "Node 7")
     (add-edge-between-nodes g "Node 6" "Node 8")
     (add-edge-between-nodes g "Node 4" "Node 9")
     (add-edge-between-nodes g "Node 5" "Node 10")
     (add-edge-between-nodes g "Node 5" "Node 11")
     (add-edge-between-nodes g "Node 1" "Node 11")
     (add-edge-between-nodes g "Node 8" "Node 7")
     g))
          
   (edgep *graph* "Node 1" "Node 2")
   (bfs *graph* "Node 1" "Node 11")}))
  
  (:h1 "Dictionary")
  
  (:h2 "Edges")
  
  (:p "Instances of the " (:code "EDGE") " class represent a
  connection between two nodes. Currently, all edges are considered to
  be undirected, although it is anticipated that support for directed
  edges will be added in the future.")
  
  (:pre
   (:code
    #q{(defclass edge ()
  ((node1 :accessor node1 :initarg :node1)
   (node2 :accessor node2 :initarg :node2)
   (data :accessor edge-data :initarg :data :initform nil))
  (:documentation "Instances of the EDGE class represent edges between
  nodes in a graph."))}))
  
  (:ul
   (:li "[generic function] "
        (:code "print-edge-data object stream => result")
        (:p "Print data about the edge object. returns NIL if
   stream is non-nil or a string containg the edge object data if
   stream is NIL."))
   
   (:li "[generic function] "
        (:code "copy-edge edge => new-edge")
        (:p "copy-edge makes a new instance of the same class as edge,
   new-edge, and sets the node1, node2 and data of new-edge to be the
   same as the respective values in edge. Returns new-edge. Subclasses
   of edge should specialize this function to copy additional data as
   appropriate.")))
  
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

  (:p (:code "(make-graph :node-test 'equal)"))

  (:p "The graph class is defined as follows:")
  
  (:pre
   (:code #q{(defclass graph ()
  ((node-test :initarg :node-test :accessor graph-node-test :initform 'eql))
   (:documentation "The protocol class of graphs. The intent is that
  there will be concrete subclasses of GRAPH with different
  implementations of storing the nodes and edges."))}))
  

  (:ul
   (:li "[parameter] "
        (:code "*default-graph-class*")
        (:p "When the make-graph function is called without explicitly
   specifying a graph class, an object of the class named in the
   parameter *default-graph-class* will be created."))

   (:li "[parameter] "
        (:code "*default-edge-class*")
        (:p "When the make-edge-between-nodes function is called
   without explicitly specifying an edge class, an object of the class
   named in the parameter *default-edge-class* will be created and
   added to the graph."))
   
   (:li "[function] "
        (:code "make-graph args &key (class *default-graph-class*)
       node-test &allow-other-keys => graph")
        (:p "Creates an instance of the specified class, passing
       args (other than the :class keyword argument) to the
       make-instance call. Returns the newly created graph object."))

   (:li "[generic function] "
        (:code "copy-graph graph &key copy-edges => new-graph")
        (:p "Creates a new graph of the same class as graph and sets
       the edges of the new graph to be the same as the edges of
       graph, or, if copy-edges is non-nil, sets the edges of the new
       graph to be a copy of the edges of graph. Returns the new graph
       new-graph."))

   (:li "[generic function] "
        (:code "add-node graph node => node")
        (:p "Adds a node to the graph. Note that there are no
       requirements for the node object. Any lisp value can be used as
       a node in a graph. However, a given node can only be added to a
       graph once. But whether or not a given node is already in a
       graph is determined by the comparison function specified by the
       node-test slot of the graph."))

   (:li "[generic function] "
        (:code "graph-node-p graph node => result")
        (:p "Returns true if node is a node in graph."))
  
   (:li "[generic function] "
        (:code "node-count graph => node-count")
        (:p "Returns the number of nodes in graph."))

   (:li "[generic function] "
        (:code "add-edge graph edge => edge")
        (:p "Adds an an edge to the grap. Returns the newly added
       edge. This function is to be used when one has in instance of
       the edge class or a class that extends edge. To automatically
       create and add an edge between two nodes, see the
       add-edge-between-nodes function. NOTE: currently the add-edge
       function ensures that both of the nodes in edge are in graph,
       otherwise it returns an error. This may change in the
       future."))

   (:li "[generic function] "
        (:code "add-edge-between-nodes graph node1 node2 &key
       (edge-class *default-edge-class*) => edge")
        (:p "Creates an instance of edge-class between node1 and node2,
       adds it to graph and returns the newly created edge."))
  
   (:li "[generic function] "
        (:code "remove-edge graph edge => result")
        (:p "Removes edge from the specified graph. Returns nil if no
       edge was removed, otherwise an unspecified non-nil value."))

   (:li "[generic function] "
        (:code "remove-edge-between-nodes graph node1 node2 => result")
        (:p "If there are one or more edges between node1 and node2,
       remove one edge between the two nodes. Returns nil if no edge
       was removed, otherwise an unspecified non-nil value."))

   (:li "[generic function] "
        (:code "edgep graph node1 node2 => result")
        (:p "Returns t if there exists an edge in graph between node1 and
     node2, otherwise nil."))
  

   (:li "[generic function] "
        (:code "find-edges-from graph node &key test => edge-list")
        (:p "This is currently underspecified. One day when we have
     directed edges, this will return edges from a given node."))

   (:li "[generic function] "
        (:code "find-edges-to graph node &key test => edge-list")
        (:p "This is currently underspecified. One day when we have
     directed edges, this will return edges to a given node."))

   (:li "[generic function] "
        (:code "find-self-edges graph node &key test => edge-list")
        (:p "Returns of the loop edges for a given node, that is edges
       that start and end with the same node."))

   (:li "[generic function] "
        (:code "find-edges-containing graph node &key test => edge-list")
        (:p "Returns a list of the edges that start or end with the
       given node."))

   (:li "[generic function] "
        (:code "neighbors graph node &key test => node-list")
        (:p "Returns a list of the nodes that are directly connected to
     node by a single edge."))

   (:li "[generic function] "
        (:code "map-nodes graph fn => nil")
        (:p "Loops over the nodes in the graph (in an unspecified order)
     and calls fn with one argument, the node, for each node in the
     graph."))

   (:li "[generic function] "
        (:code "bfs graph start end &key key test")
        (:p "Performs a breadth-first search on graph starting at the
       start node and proceeding until the end value is found, if end
       is non-nil, or until all nodes have been searched if end is
       nil. Note that at each note the value to test against end is
       generated by calling key, which defaults to #'identity, on the
       node. The comparison is done by the test operator which
       defaults to the graph-node-test value of the graph object."))

   (:li "[generic function] "
        (:code "bfs-map graph start fn &key end key test")
        (:p ""))
  
   (:li "[generic function] "
        (:code "dfs")
        (:p ""))

   (:li "[generic function] "
        (:code "dfs-map")
        (:p "")))

  ))


