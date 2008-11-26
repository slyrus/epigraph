;;; file: epigraph.lisp
;;; author: cyrus harmon
;;;
;;; Copyright (c) 2008 Cyrus Harmon (ch-lisp@bobobeach.com)
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

(defparameter *default-graph-class* 'simple-edge-list-graph)
(defparameter *default-edge-class* 'edge)

(defun make-graph (&rest args &key (class *default-graph-class*)
  node-test &allow-other-keys)
  "Creates a GRAPH instance whose actual class will either be
specified by GRAPH-CLASS or by *DEFAULT-GRAPH-CLASS*."
  (declare (ignore node-test))
  (apply #'make-instance class (remove-keyword-args :class args)))

(defgeneric copy-graph (graph &key copy-edges)
  (:documentation "Returns a copy of the graph which will contain
  copies of the edges, but the same nodes as the original graph."))

(defgeneric add-node (graph node)
  (:documentation "Add a node to the graph."))

(defgeneric graph-node-p (graph node)
  (:documentation "Returns t if node is a node in graph."))

(defgeneric node-count (graph)
  (:documentation "Returns the number of nodes in graph."))

(defgeneric add-edge (graph edge)
  (:documentation "Adds an edge to the graph."))

(defgeneric add-edge-between-nodes (graph node1 node2 &key edge-class)
  (:documentation "Creates a node from node1 to node2 and adds it to
  the graph.."))

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

(defgeneric map-nodes (graph fn)
  (:documentation "Calls FN on every node in the graph, without regard
  to the configuration of the graph."))

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

(defmethod bfs-map ((graph graph) start fn
                    &key
                    (end nil end-supplied-p)
                    (key 'identity)
                    (test (graph-node-test graph)))
  (let ((visited-nodes (make-hash-table :test test)))
    (labels
        ((bfs-visit (node-list)
           (let (children)
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
               (bfs-visit children)))))
      (bfs-visit (list start)))))

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

(defmethod map-nodes ((graph simple-edge-list-graph) fn)
  (maphash (lambda (k v)
             (declare (ignore v))
             (funcall fn k))
           (graph-node-hash graph)))

(defmethod graph-edges ((graph simple-edge-list-graph))
  (graph-edge-list graph))

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
  (push edge (graph-edge-list graph)))

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
    (push edge (graph-edge-list graph))))

(defmethod remove-edge ((graph simple-edge-list-graph) (edge edge)
                        &key (test (graph-node-test graph)))
  (setf (graph-edge-list graph)
        (remove edge
                (graph-edge-list graph)
                :test test)))

(defmethod remove-edge-between-nodes ((graph simple-edge-list-graph)
                        node1 node2)
  (let ((edge (cons node1 node2)))
    (setf (graph-edge-list graph)
          (remove edge
                  (graph-edge-list graph)
                  :key (lambda (x)
                         (cons (node1 x) (node2 x)))
                  :test 'equalp))))

(defmethod edgep ((graph simple-edge-list-graph) node1 node2)
  (find (cons node1 node2)
        (graph-edges graph)
        :key (lambda (x)
               (cons (node1 x) (node2 x)))
        :test 'equalp))

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

(defmethod find-edges-containing ((graph simple-edge-list-graph) node
                                  &key (test (graph-node-test graph)))
  (union (apply #'find-edges-from graph node
                (when test `(:test ,test)))
         (apply #'find-edges-to graph node
                (when test `(:test ,test)))))

(defmethod neighbors (graph element
                      &key (test (graph-node-test graph))) 
  (let ((edges (apply #'find-edges-containing graph element
                      (when test `(:test ,test)))))
    (let ((neighbors
           (union (map (type-of edges) #'node1 edges)
                  (map (type-of edges) #'node2 edges)
                  :test test)))
      (if (find-self-edges graph test)
          neighbors
          (remove element neighbors :test test)))))

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

(defun find-cycles (graph &key (start (first-node graph)))
  "Finds all of the cycles in a graph and returns three VALUEs, a list
of the edges that make complete the cycles, a list of the paths that
form the cycles, and a copy of GRAPH, with the cycle-forming edges
removed."
  (let ((graph (copy-graph graph :copy-edges t))
        (cycle-edges))
    (loop for (edge-path node-path) = (multiple-value-list
                                       (apply 'find-cycle graph
                                              (when start `(:start ,start))))
       while edge-path
       do
         (push (list edge-path node-path) cycle-edges)
         (remove-edge graph (car edge-path)))
    (when cycle-edges
      (apply #'values (append (apply #'mapcar #'list cycle-edges) (list graph))))))

