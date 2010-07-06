;;; file: epigraph.lisp
;;; author: cyrus harmon
;;;
;;; Copyright (c) 2008-2009 Cyrus Harmon (ch-lisp@bobobeach.com)
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :epigraph)

;;;
;;; edges
(defclass edge ()
  ((node1 :accessor node1 :initarg :node1)
   (node2 :accessor node2 :initarg :node2)
   (data :accessor edge-data :initarg :data :initform nil))
  (:documentation "Instances of the EDGE class represent edges between
  nodes in a graph."))

(defgeneric print-edge-data (object stream)
  (:method ((object edge) stream)
    (format stream "~S ~S"
            (node1 object)
            (node2 object)))
  (:documentation "Prints data about a given edge. This function is
  called from the print-object method specialied on EDGE objects. To
  add additional data to be printed by subclasses of edge create
  an :AFTER method on PRINT-EDGE-DATA."))

(defmethod print-object ((object edge) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (print-edge-data object stream)))

(defgeneric copy-edge (edge)
  (:method ((edge edge))
    (make-instance (class-of edge)
                   :node1 (node1 edge)
                   :node2 (node2 edge)
                   :data (edge-data edge)))
  (:documentation "Returns a copy of the edge."))

(defgeneric edge-nodes (edge)
  (:method ((edge edge))
    (list (node1 edge) (node2 edge)))
  (:documentation "Returns a list of the nodes connected by edge."))

(defgeneric edges-nodes-equal (edge1 edge2 &key test)
  (:method ((edge1 edge) (edge2 edge) &key (test 'equal))
    (and (funcall test (node1 edge1) (node1 edge2))
         (funcall test (node2 edge1) (node2 edge2))))
  (:documentation "Returns T if the two edges connect the same
  nodes. Note that currently there is no notion of direction between
  edges, so edges from A to B and from B to A are equivalent and
  therefore edges-nodes-equal returns T for those two edges."))

(defgeneric other-edge-node (edge node)
  (:method ((edge edge) node)
    (when (member node (edge-nodes edge) :test 'equal)
      (car (remove node (edge-nodes edge) :count 1 :test 'equal))))
  (:documentation "Returns the other node in the edge. That is if
  there is an edge between A and B, other-edge-node of A would return
  B. If the edge is a self-edge, returns node."))

;;;
;;; graphs and generic functions for operating on graphs
(defclass graph ()
  ((node-test :initarg :node-test :accessor graph-node-test :initform 'eql))
  (:documentation "The protocol class of graphs. The intent is that
  there will be concrete subclasses of GRAPH with different
  implementations of storing the nodes and edges."))

;;; should we define feature-rich subclasses of graph?
;;; e.g.
;; (defclass singly-connected-graph (graph) ())
;; (defclass acyclic-graph (graph) ())
;; (defclass undirected-graph (graph) ())
;; (defclass digraph (graph) ())

(defun node-position (graph item sequence
                      &key from-end (start 0) end key
                      (test (graph-node-test graph)) test-not)
  (position item sequence
            :from-end from-end :start start :end end :key key
            :test test :test-not test-not))

(defun node-find (graph item sequence
                  &key from-end (start 0) end key
                  (test (graph-node-test graph)) test-not)
  (find item sequence
        :from-end from-end :start start :end end :key key
        :test test :test-not test-not))

(defun node-remove (graph item sequence 
                    &key from-end (start 0) end key count
                    (test (graph-node-test graph)) test-not)
  (remove item sequence
          :from-end from-end :start start :end end :key key :count count
          :test test :test-not test-not))

(defun node-equal (graph node1 node2
                  &key (test (graph-node-test graph)))
  (funcall test node1 node2))

(defun make-node-hash-table (graph)
  (make-hash-table :test (graph-node-test graph)))

#+nil
(defun node-member (graph item list
                    &key key (test (graph-node-test graph)) test-not)
  (member item list :key key :test test :test-not test-not))

(defparameter *default-graph-class* 'simple-edge-list-graph)
(defparameter *default-edge-class* 'edge)

(defun make-graph (&rest args &key (class *default-graph-class*)
  node-test &allow-other-keys)
  "Creates a GRAPH instance whose actual class will either be
specified by GRAPH-CLASS or by *DEFAULT-GRAPH-CLASS*."
  (declare (ignore node-test))
  (apply #'make-instance class (remove-keyword-args :class args)))

(defgeneric copy-graph (graph &key copy-edges)
  (:documentation "Returns a copy of the graph which will contain the
  same nodes as the original graph and either the same edges as graph
  or copies of the edges, depending on the value of &key copy-edges."))

(defgeneric add-node (graph node)
  (:documentation "Add a node to the graph."))

(defgeneric remove-node (graph node)
  (:documentation "Remove a node from the graph."))

(defgeneric graph-node-p (graph node)
  (:documentation "Returns t if node is a node in graph."))

(defgeneric node-count (graph)
  (:documentation "Returns the number of nodes in graph."))

(defgeneric add-edge (graph edge)
  (:documentation "Adds an edge to the graph."))

(defgeneric add-edge-between-nodes (graph node1 node2 &key edge-class)
  (:documentation "Creates an edge from node1 to node2, adds it to
  the graph, and returns the edge."))

(defgeneric remove-edge (graph edge &key test)
  (:documentation "Removes edge from the graph."))

(defgeneric remove-edge-between-nodes (graph node1 node2)
  (:documentation "Removes the edge from node1 to node2 in the
  graph."))

(defgeneric edgep (graph node1 node2)
  (:documentation "Returns the edge that connects node1 and node2 in
  graph if the edge is present in the graph, otherwise returns NIL."))

(defgeneric graph-edges (graph)
  (:documentation "Returns the edges of graph-edges. Currently the
  format in which the edges are returned is not specified."))

(defgeneric find-edges-from (graph node &key test)
  (:documentation "Returns a list of the edges in graph that begin
  with node."))

(defgeneric find-edges-to (graph node &key test)
  (:documentation "Returns a list of the edges in graph that end
  with node."))

(defgeneric find-self-edges (graph node &key test)
  (:documentation "Returns a list of the edges in graph that begin and
  end with node."))

(defgeneric find-edges-containing (graph node &key test)
  (:documentation "Returns a list of the edges in graph that start or
  end with node."))

(defgeneric neighbors (graph node &key test)
  (:documentation "Returns a list of the nodes that are connected to
  node."))

(defgeneric map-nodes (fn graph)
  (:documentation "Calls FN on every node in the graph, without regard
  to the configuration of the graph."))

(defgeneric map-nodes->list (fn graph)
  (:documentation "Calls FN on every node in the graph, without regard
  to the configuration of the graph and returns the resulting values
  in a list."))

(defgeneric bfs (graph start end &key key test)
  (:documentation "Performs a breadth-first-search on graph starting
  at start and returns a path end if end is reachable from start,
  otherwise returns NIL. [DOCUMENT KEY AND TEST ARGS PLEASE!]"))

(defgeneric bfs-map (graph start fn &key end key test)
  (:documentation "Performs a breadth-first-search on graph starting
  at start until node end is found, if it is specified, calling fn, a
  function of one argument, for each node as it is found. [DOCUMENT
  KEY AND TEST ARGS PLEASE!]"))

(defgeneric dfs (graph start end &key key test)
  (:documentation "Performs a depth-first-search on graph starting at
  start and returns a path end if end is reachable from start,
  otherwise returns NIL. [DOCUMENT KEY AND TEST ARGS PLEASE!]"))

(defgeneric dfs-map (graph start fn &key end key test)
  (:documentation "Performs a depth-first-search on graph starting at
  start until node end is found, if it is specified, calling fn, a
  function of one argument, for each node as it is found. [DOCUMENT
  KEY AND TEST ARGS PLEASE!]"))

(defmethod bfs ((graph graph) start end
                &key (key 'identity) (test (graph-node-test graph)))
  (let ((visited-nodes (make-hash-table :test test)))
    (labels
        ((bfs-visit (node-set-list)
           (let (children)
             (map nil
                  (lambda (node-and-path)
                    (destructuring-bind (node . path)
                        node-and-path
                      (when (funcall test (funcall key node) end)
                        (return-from bfs
                          (nreverse (cons node path))))
                      (setf (gethash node visited-nodes) node)
                      (let ((neighbors (neighbors graph node)))
                        (map nil
                             (lambda (x)
                               (unless (gethash x visited-nodes)
                                 (push (cons x (cons node path)) children)))
                             neighbors))))
                  node-set-list)
             (when children
               (bfs-visit children)))))
      (bfs-visit (list (cons start nil))))))

(defparameter *bfs-depth* nil)

(defmethod bfs-map ((graph graph) start fn
                    &key
                    (end nil end-supplied-p)
                    (key 'identity)
                    (test (graph-node-test graph)))
  (let ((visited-nodes (make-hash-table :test test)))
    (labels
        ((bfs-visit (node-list level)
           (let ((*bfs-depth* level)
                 children)
             (map nil
                  (lambda (node)
                    (funcall fn node)
                    (when (and end-supplied-p
                               (funcall test (funcall key node) end))
                      (return-from bfs-map))
                    (setf (gethash node visited-nodes) node)
                    (let ((neighbors (neighbors graph node)))
                      (map nil
                           (lambda (x)
                             (unless (gethash x visited-nodes)
                               (push x children)))
                           neighbors)))
                  node-list)
             (when children
               (bfs-visit children (1+ level))))))
      (bfs-visit (list start) 1))))

(defmethod bfs-map-edges ((graph graph) start fn
                          &key
                          (end nil end-supplied-p)
                          (key 'identity)
                          (test (graph-node-test graph)))
  (let ((visited-edges (make-hash-table :test 'eq)))
    (apply #'bfs-map graph start
           (lambda (node)
             (map nil
                  (lambda (edge)
                    (unless (gethash edge visited-edges)
                      (funcall fn edge)
                      (setf (gethash edge visited-edges) edge)))
                  (find-edges-containing graph node)))
           (append
            (when end-supplied-p `(:end ,end))
            (when key `(:key ,key))
            (when test `(:test ,test))))))

(defmethod dfs ((graph graph) start end
                &key
                (key 'identity)
                (test (graph-node-test graph)))
  (let ((visited-nodes (make-hash-table :test test)))
    (labels ((dfs-visit (node path)
               (when (funcall test
                            (funcall key node)
                            end)
                 (return-from dfs (nreverse (cons node path))))
               (setf (gethash node visited-nodes) node)
               (let ((neighbors (neighbors graph node)))
                 (map nil
                      (lambda (x)
                        (unless (gethash x visited-nodes)                              
                          (dfs-visit x (cons node path))))
                      neighbors))))
      (dfs-visit start nil))))

(defparameter *dfs-depth* nil)

(defmethod dfs-map ((graph graph) start fn
                    &key
                    (end nil end-supplied-p)
                    (key 'identity)
                    (test (graph-node-test graph)))
  (let ((visited-nodes (make-hash-table :test test))
        (*dfs-depth* 0))
    (labels ((dfs-visit (node path)
               (let ((*dfs-depth* (1+ *dfs-depth*)))
                 (funcall fn node)
                 (when (and end-supplied-p
                            (funcall test (funcall key node) end))
                   (return-from dfs-map))
                 (setf (gethash node visited-nodes) node)
                 (let ((neighbors (neighbors graph node)))
                   (map nil
                        (lambda (x)
                          (unless (gethash x visited-nodes)
                            (dfs-visit x (cons node path))))
                        neighbors)))))
      (dfs-visit start nil))))

(defmethod dfs-map-edges ((graph graph) start fn
                          &key
                          (end nil end-supplied-p)
                          (key 'identity)
                          (test (graph-node-test graph)))
  (let ((visited-edges (make-hash-table :test 'eq)))
    (apply #'dfs-map graph start
           (lambda (node)
             (map nil
                  (lambda (edge)
                    (unless (gethash edge visited-edges)
                      (funcall fn edge)
                      (setf (gethash edge visited-edges) edge)))
                  (find-edges-containing graph node)))
           (append
            (when end-supplied-p `(:end ,end))
            (when key `(:key ,key))
            (when test `(:test ,test))))))
;;;
;;; Alternative implementation of bfs and dfs, using a more general
;;; graph-search routine and a q class that can accept new items at
;;; either the head or the tail.
(defclass q ()
  ((items :accessor qitems :initform nil)
   (last :accessor qlast :initform nil)))

(defun makeq ()
  (make-instance 'q))

(defgeneric qappend (q item)
  (:method (q item)
    (if (qitems q)
        (setf (cdr (qlast q)) (list item)
              (qlast q) (cdr (qlast q)))
        (setf (qitems q) (list item)
              (qlast q) (qitems q)))))

(defgeneric qpush (q item)
  (:method (q item)
    (if (qitems q)
        (push item (qitems q))
        (setf (qitems q) (list item)
              (qlast q) (qitems q)))))

(defgeneric qpop (q)
  (:method (q)
    (pop (qitems q))))

(defmethod graph-search ((graph graph) start end
                         &key
                         (key 'identity)
                         (test (graph-node-test graph))
                         (queueing-function 'qappend))
  (let* ((visited-nodes (make-hash-table :test test))
         (search-nodes (makeq)))
    (qappend search-nodes (cons start nil))
    (labels
        ((visit ()
           (destructuring-bind (node . path)
               (qpop search-nodes)
             (when (funcall test (funcall key node) end)
               (return-from graph-search
                 (nreverse (cons node path))))
             (setf (gethash node visited-nodes) node)
             (let ((neighbors (neighbors graph node)))
               (map nil
                    (lambda (x)
                      (unless (gethash x visited-nodes)
                        (funcall queueing-function search-nodes
                                 (cons x (cons node path)))))
                    neighbors)))
           (when (qitems search-nodes)
             (visit))))
      (visit))))

(defmethod bfs2 ((graph graph) start end
                 &key
                 (key 'identity)
                 (test (graph-node-test graph)))
  (apply #'graph-search graph start end
         :queueing-function 'qappend
         (append
          (when key `(:key ,key))
          (when test `(:test ,test)))))

(defmethod dfs2 ((graph graph) start end
                 &key
                 (key 'identity)
                 (test (graph-node-test graph)))
  (apply #'graph-search graph start end
         :queueing-function 'qpush
         (append
          (when key `(:key ,key))
          (when test `(:test ,test)))))


;;;
;;; simple edge list graph
(defclass simple-edge-list-graph (graph)
  ;; FIXME! the :test for the node-hash should probably be the
  ;; graph-node-test not necessarily 'equal!
  ((node-hash :accessor graph-node-hash
              :initarg :nodes
              :initform (make-hash-table :test 'equal))
   (edges :accessor graph-edge-list :initarg :edges :initform nil))
  (:documentation "A concrete subclass of graph that represents the
  edges in the graph with a list of edges between nodes."))

(defmethod graph-node-p ((graph simple-edge-list-graph) node)
  (gethash node (graph-node-hash graph)))

(defmethod node-count ((graph simple-edge-list-graph))
  (hash-table-count (graph-node-hash graph)))

(defmethod add-node ((graph simple-edge-list-graph) node)
  (unless (graph-node-p graph node)
    (progn
      (setf (gethash node (graph-node-hash graph)) node)))
  node)

;;; FIXME! What do we do if we try to remove a node which still has edges containing it?
;;; 1. Remove the node and leave the edges?
;;; 2. Remove the edges and remove the node?
;;; 3. Throw an error?
(defmethod remove-node ((graph simple-edge-list-graph) node)
  (remhash node (graph-node-hash graph))
  node)

(defmacro with-graph-iterator ((function graph) &body body)
  `(with-hash-table-iterator (,function (graph-node-hash ,graph))
     ,@body))
 
(defgeneric first-node (graph)
  (:documentation "Returns the first node in a graph. Note that not
  all graphs are rquired to support this function and that even graphs
  that do support this might not have a consistent ordering of the
  nodes such that successive first-node calls might not return the
  same node."))

(defmethod first-node ((graph simple-edge-list-graph))
  (with-graph-iterator (next-entry graph)
    (nth-value 1 (next-entry))))

(defmethod graph-nodes ((graph simple-edge-list-graph))
  (let (l)
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k l))
             (graph-node-hash graph))
    l))

(defmethod map-nodes (fn (graph simple-edge-list-graph))
  (maphash (lambda (k v)
             (declare (ignore v))
             (funcall fn k))
           (graph-node-hash graph)))

(defmethod map-nodes->list (fn (graph simple-edge-list-graph))
  (let (l)
    (maphash (lambda (k v)
               (declare (ignore v))
               (push (funcall fn k) l))
             (graph-node-hash graph))
    l))

(defmethod graph-edges ((graph simple-edge-list-graph))
  (graph-edge-list graph))

(defmethod map-edges (fn (graph simple-edge-list-graph))
  (map nil fn (graph-edges graph)))

(defmethod map-edges->list (fn (graph simple-edge-list-graph))
  (map 'list fn (graph-edges graph)))


(defmethod copy-graph ((graph simple-edge-list-graph) &key copy-edges)
  (let ((new (make-instance (class-of graph))))
    (setf (graph-node-hash new)
          (alexandria:copy-hash-table (graph-node-hash graph))
          (graph-edge-list new)
          (if copy-edges
              (loop for edge in (graph-edge-list graph)
                 collect (copy-edge edge))
              (graph-edge-list graph))
          (graph-node-test new)
          (graph-node-test graph))
    new))

(defmethod add-edge ((graph simple-edge-list-graph) (edge edge))
  (unless (graph-node-p graph (node1 edge))
    (error "Node ~A not in graph ~A" (node1 edge) graph))
  (unless (graph-node-p graph (node2 edge))
    (error "Node ~A not in graph ~A" (node2 edge) graph))
  (push edge (graph-edge-list graph))
  edge)

(defmethod add-edge-between-nodes ((graph simple-edge-list-graph)
                                   node1 node2
                                   &key (edge-class *default-edge-class*))
  (unless (graph-node-p graph node1)
    (error "Node ~A not in graph ~A" node1 graph))
  (unless (graph-node-p graph node2)
    (error "Node ~A not in graph ~A" node2 graph))
  (let ((edge (make-instance edge-class
                             :node1 node1
                             :node2 node2)))
    (push edge (graph-edge-list graph))
    edge))

(defmethod remove-edge ((graph simple-edge-list-graph) (edge edge)
                        &key (test (graph-node-test graph)))
  (setf (graph-edge-list graph)
        (remove edge
                (graph-edge-list graph)
                :test test)))

(defmethod remove-edge-between-nodes ((graph simple-edge-list-graph)
                                      node1 node2)
  (let ((edge (edgep graph node1 node2)))
    (when edge
      (setf (graph-edge-list graph)
            (remove edge (graph-edge-list graph))))))

;;; FIXME! Should edgep check both (node1 . node2) and (node2 . node1)
;;; or should we just check the former???
(defmethod edgep ((graph simple-edge-list-graph) node1 node2)
  (find-if
   (lambda (edge)
     (or (and (equal (node1 edge) node1)
              (equal (node2 edge) node2))
         (and (equal (node2 edge) node1)
              (equal (node1 edge) node2))))
   (graph-edges graph)))

(defmethod find-edge ((graph simple-edge-list-graph) node1 node2)
  (or (edgep graph node1 node2)
      (edgep graph node2 node1)))

(defmethod find-edges-from ((graph simple-edge-list-graph) node
                            &key (test (graph-node-test graph)))
  (remove-if-not (lambda (x)
                   (funcall test node x))
                 (graph-edges graph) :key #'node1))

(defmethod find-edges-to ((graph simple-edge-list-graph) node
                          &key (test (graph-node-test graph)))
  (remove-if-not (lambda (x)
                   (funcall test node x))
                 (graph-edges graph) :key #'node2))

(defmethod find-self-edges ((graph simple-edge-list-graph) node
                            &key (test (graph-node-test graph)))
  (remove-if-not (lambda (x)
                   (funcall test (node1 x) (node2 x)))
                 (graph-edges graph)))

(defmethod self-edge-p ((graph simple-edge-list-graph) edge
                        &key (test (graph-node-test graph)))
  (funcall test (node1 edge) (node2 edge)))

(defmethod find-edges-containing ((graph simple-edge-list-graph) node
                                  &key (test (graph-node-test graph)))
  (union (apply #'find-edges-from graph node
                (when test `(:test ,test)))
         (apply #'find-edges-to graph node
                (when test `(:test ,test)))))

(defmethod neighbors (graph node
                      &key (test (graph-node-test graph))) 
  (let ((edges (apply #'find-edges-containing graph node
                      (when test `(:test ,test)))))
    (let ((neighbors
           (union (map (type-of edges) #'node1 edges)
                  (map (type-of edges) #'node2 edges)
                  :test test)))
      (if (find-self-edges graph test)
          neighbors
          (remove node neighbors :test test)))))

(defmethod neighbors-and-edges (graph node
                                &key (test (graph-node-test graph))) 
  (let ((edges (apply #'find-edges-containing graph node
                      (when test `(:test ,test)))))
    (apply #'mapcar #'list (loop for edge in edges
                      append (cond ((not (funcall test node (node1 edge)))
                                    (list (list (node1 edge) edge)))
                                   ((not (funcall test node (node2 edge)))
                                    (list (list (node2 edge) edge)))
                                   (t nil))))))

(defun find-cycle (graph
                   &key (start (first-node graph))
                   (test (graph-node-test graph)))
  "Finds a cycle in graph, if one exists, using depth-first-search and
returns two VALUES, a list of the edges in the cycle, and a list of
the nodes in the cycle."
  (let ((traversed-edges (make-hash-table))
        (visited-nodes (make-hash-table :test test)))
    (labels ((visit (node node-path edge-path)
               (setf (gethash node visited-nodes) node)
               (let ((edges (find-edges-containing graph node)))
                 (map nil
                      (lambda (edge)
                        (unless (gethash edge traversed-edges)
                          (setf (gethash edge traversed-edges) edge)
                          (let ((neighbor (if (funcall test
                                                       (node1 edge)
                                                       node)
                                              (node2 edge)
                                              (node1 edge))))
                            (when (gethash neighbor visited-nodes)
                              (let ((cycle-nodes (member neighbor
                                                         (nreverse (cons node node-path))
                                                         :test test)))
                                (return-from find-cycle
                                  (values
                                   (member-if
                                    (lambda (x)
                                      (and (find (graph:node1 x) cycle-nodes)
                                           (find (graph:node2 x) cycle-nodes)))
                                    (nreverse (cons edge edge-path)))
                                   cycle-nodes))))
                            (visit neighbor
                                   (cons node node-path)
                                   (cons edge edge-path)))))
                      edges))))
      (visit start nil nil))))

(defun find-cycle-edges (graph
                         &key
                         (start (first-node graph))
                         (test (graph-node-test graph)))
  (let ((visited-nodes (make-hash-table :test test))
        cycle-edges)
    (labels ((dfs-visit (node path)
               (setf (gethash node visited-nodes) node)
               (let ((neighbors (neighbors graph node)))
                 (map nil
                      (lambda (x)
                        (cond ((funcall test x (car path)))
                              ((gethash x visited-nodes)
                               (push x cycle-edges))
                              (t (dfs-visit x (cons node path)))))
                      neighbors))))
      (dfs-visit start nil))
    cycle-edges))

(defun break-cycles (graph
                     &key
                     (start (first-node graph))
                     (pick-function (lambda (x y)
                                      (declare (ignore y))
                                      x)))
  "Finds all of the cycles in a graph and returns four VALUEs, a list
of the edges that make complete the cycles, a list of the paths that
form the cycles, a list of the cycle-forming edges, and a copy of
GRAPH, with the cycle-forming edges removed."
  (let ((graph (copy-graph graph :copy-edges t))
        cycle-edges
        removed-edges)
    (loop for (edge-path node-path) = (multiple-value-list
                                       (apply 'find-cycle graph
                                              (when start `(:start ,start))))
       while edge-path
       do
         (push (list edge-path node-path) cycle-edges)
         (let ((removed (funcall pick-function
                                 (car edge-path)
                                 (car (last edge-path)))))
           (remove-edge graph removed)
           (push removed removed-edges)))
    (when cycle-edges
      (apply #'values (append (apply #'mapcar #'list cycle-edges)
                              (list removed-edges graph))))))

(defun remove-connected-component (graph &key (start (first-node graph)))
  (let ((new-graph
         (make-instance (class-of graph)
                        :node-test (graph-node-test graph)))
        edges-to-remove
        nodes-to-remove)
    (add-node new-graph start)
    (push start nodes-to-remove)
    (bfs-map-edges graph start
                   (lambda (edge)
                     (add-node new-graph (node1 edge))
                     (add-node new-graph (node2 edge))
                     (add-edge new-graph edge)
                     (pushnew edge edges-to-remove)
                     (pushnew (node1 edge) nodes-to-remove)
                     (pushnew (node2 edge) nodes-to-remove)))
    (map nil (lambda (edge) (remove-edge graph edge))
         edges-to-remove)
    (map nil (lambda (node) (remove-node graph node))
         nodes-to-remove)
    (when (first-node new-graph)
      new-graph)))

(defun find-connected-components (graph &key (start (first-node graph)))
  (let ((orig-copy (copy-graph graph)))
    (loop for node = start then (first-node orig-copy)
       for component = (remove-connected-component orig-copy :start node)
       while component
       collect component)))

(defun find-longest-paths (graph)
  "Finds a longest path acecssible from each node in the
graph. Returns a list of vectors containing the longest paths for each
node. Each vector represents the path to the farthest node from the
first node of the vector to the last node of the vector."
  (mapcar (lambda (start-node)
            (let ((depth 0) deepest path)
              (dfs-map graph
                       start-node
                       (lambda (node)
                         (if (> *dfs-depth* depth)
                             (setf deepest node
                                   depth *dfs-depth*
                                   path (cons node path)))))
              (make-array (length path) :initial-contents (nreverse path))))
          (graph-nodes graph)))

(defun find-longest-path (graph)
  "Returns a single vector containing a longest path reachable from
any node in graph. The vector contains the elements in path order,
starting with the first node and ending in the most distant node on
the path."
  (elt (sort (find-longest-paths graph) #'> :key #'length) 0))

(defun graph-distance (graph node1 node2)
  "Returns the (shortest) number of edges between node1 and node2. If
node1 is node2, the distance is 0."
  (let ((path (graph:bfs graph node1 node2)))
    (when path
      (1- (length path)))))

;;; faster graph-distance-matrix that uses dfs-map and the *dfs-depth* special variable
(defun graph-distance-hash-table (graph)
  "Returns a hash-table where each value is another hash-table, where
each value is the distance from the node that is the key of the first
hash-table to the node that is the key of the second hash-table."
  (let ((outer-hash (make-hash-table :test 'eq)))
    (map-nodes
     (lambda (node1)
       (let ((inner-hash (make-hash-table :test 'eq)))
         (bfs-map graph node1
                  (lambda (node2)
                    (setf (gethash node2 inner-hash)
                          (1- *bfs-depth*))))
         (setf (gethash node1 outer-hash)
               inner-hash)))
     graph)
    outer-hash))

(defun graph-distance-matrix (graph)
  "Returns two values, an array where each entry i,j is the distance
from node i to node j (distance from a node to itself is 0) and an
array of nodes in the order corresponding to a dimension of the array
of distances."
  (let ((outer-hash  (graph-distance-hash-table graph)))
    (let* ((nodes (loop for k being the hash-keys of outer-hash collect k))
           (num-nodes (length nodes))
           (node-array (make-array num-nodes :initial-contents nodes)))
      (let ((distance-matrix (make-array (list num-nodes num-nodes))))
        (loop for i from 0 below num-nodes
             for inner-hash = (gethash (elt node-array i) outer-hash)
           do (loop for j from 0 below num-nodes
                 do (setf (aref distance-matrix i j)
                          (gethash (elt node-array j) inner-hash))))
        (values distance-matrix node-array)))))
