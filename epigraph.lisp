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
;;; nodes
(defclass node ()
  ((name :accessor node-name :initarg :name :initform nil)
   (data :accessor node-data :initarg :data :initform nil))
  (:documentation "A simple class for representing nodes in a graph,
  which will consist of a set of nodes, and a set of edges between the
  nodes."))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (node-name object))))

(defun make-node (name)
  (make-instance 'node :name name))

;;;
;;; edges
(defclass edge ()
  ((graph :accessor edge-graph :initarg :graph)
   (node1 :accessor node1 :initarg :node1)
   (node2 :accessor node2 :initarg :node1))
  (:documentation "Instances of the edge class represent edges between
  nodes in a graph."))

;;;
;;; graphs and generic functions for operating on graphs
(defclass graph ()
  ()
  (:documentation "The protocol class of graphs. The intent is that
  there will be concrete subclasses of graph with different
  implementations of storing the nodes and edges."))

;;; should we define feature-rich subclasses of graph?
;;; e.g.
(defclass singly-connected-graph (graph) ())
(defclass acyclic-graph (graph) ())
(defclass undirected-graph (graph) ())
(defclass digraph (graph) ())

(defparameter *default-graph-class* 'edge-list-graph)

(defun make-graph (&optional (graph-class *default-graph-class*))
  (make-instance graph-class))

(defgeneric copy-graph (graph)
  (:documentation "Returns a copy of the graph which will contain
  copies of the edges, but the same nodes as the original graph."))

(defgeneric add-node (graph node)
  (:documentation "Add a node to the graph."))

(defgeneric get-node (graph identifier))

(defgeneric graph-node-p (graph node)
  (:documentation "Returns t if node is a node in graph."))

(defgeneric add-edge (graph node1 node2)
  (:documentation "Adds an edge to the graph from node1 to node2."))

(defgeneric remove-edge (graph node1 node2)
  (:documentation "Removes the edge from node1 to node2 in the
  graph."))

(defgeneric edgep (graph node1 node2)
  (:documentation "Returns the edge that connects node1 and node2 in
  graph if the edge is present in the graph, otherwise returns NIL."))

(defgeneric graph-edges (graph)
  (:documentation "Returns the edges of graph-edges. Currently the
  format in which the edges are returned is not specified."))

(defgeneric find-edges-from (graph node)
  (:documentation "Returns a list of the edges in graph that begin
  with node."))

(defgeneric find-edges-to (graph node)
  (:documentation "Returns a list of the edges in graph that end
  with node."))

(defgeneric find-edges-containing (graph node)
  (:documentation "Returns a list of the edges in graph that start or
  end with node."))

(defgeneric neighbors (graph node)
  (:documentation "Returns a list of the nodes that are connected to
  node."))

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

(defmethod bfs ((graph graph) start end &key (key 'identity) (test 'eql))
  (let ((visited-nodes (make-hash-table)))
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
                    key
                    test)
  (let ((visited-nodes (make-hash-table)))
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

(defmethod dfs ((graph graph) start end &key key test)
  (let ((visited (list start)))
    (labels ((dfs-visit (node path)
               (if (funcall test (funcall key node) end)
                   (return-from dfs (nreverse (cons node path)))
                   (let ((neighbors (neighbors graph node)))
                     (map nil
                          (lambda (x)
                            (unless (member x visited)
                              (push x visited)
                              (dfs-visit x (cons node path))))
                          neighbors)))))
      (dfs-visit start nil))))

(defmethod dfs-map ((graph graph) start fn
                    &key
                    (end nil end-supplied-p)
                    key
                    test)
  (let ((visited (list start)))
    (labels ((dfs-visit (node path)
               (funcall fn node)
               (when (and end-supplied-p
                          (funcall test (funcall key node) end))
                 (return-from dfs-map))
               (let ((neighbors (neighbors graph node)))
                 (map nil
                      (lambda (x)
                        (unless (member x visited)
                          (push x visited)
                          (dfs-visit x (cons node path))))
                      neighbors))))
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
                         (test 'eql)
                         (queueing-function 'qappend))
  (let* ((visited-nodes (make-hash-table))
         (search-nodes (makeq)))
    (qappend search-nodes (cons start nil))
    (labels
        ((bfs-visit ()
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
             (bfs-visit))))
      (bfs-visit))))

(defmethod bfs2 ((graph graph) start end
                 &key
                 (key 'identity)
                 (test 'eql))
  (apply #'graph-search graph start end
         :queueing-function 'qappend
         (append
          (when key `(:key ,key))
          (when test `(:test ,test)))))

(defmethod dfs2 ((graph graph) start end
                 &key
                 (key 'identity)
                 (test 'eql))
  (apply #'graph-search graph start end
         :queueing-function 'qpush
         (append
          (when key `(:key ,key))
          (when test `(:test ,test)))))


;;;
;;; edge list graph
(defclass edge-list-graph (graph)
  ((node-hash :accessor graph-node-hash
              :initarg :nodes
              :initform (make-hash-table))
   (node-name-hash :accessor graph-node-name-hash
                   :initarg :nodes
                   :initform (make-hash-table :test 'equal))
   (edges :accessor graph-edge-list :initarg :edges :initform nil))
  (:documentation "A concrete subclass of graph that represents the
  edges in the graph with a list of edges between nodes."))

(defmethod graph-node-p ((graph edge-list-graph) (node node))
  (gethash node (graph-node-hash graph)))

(defmethod add-node ((graph edge-list-graph) (node node))
  (unless (graph-node-p graph node)
    (progn
      (setf (gethash node (graph-node-hash graph)) node)
      (when (node-name node)
        (setf (gethash (node-name node) (graph-node-name-hash graph)) node))))
  node)

(defmethod add-node ((graph edge-list-graph) (name string))
  (add-node graph (make-node name)))

(defmethod get-node ((graph edge-list-graph) (node node))
  node)

(defmethod get-node ((graph edge-list-graph) name)
  (gethash name (graph-node-name-hash graph)))

(defun find-node (graph node-identifier)
  (typecase node-identifier
    (node node-identifier)
    (string (get-node graph node-identifier))))

(defmacro with-graph-iterator ((function graph) &body body)
  `(with-hash-table-iterator (,function (graph-node-hash ,graph))
     ,@body))
  
(defmethod first-node ((graph edge-list-graph))
  (with-graph-iterator (next-entry graph)
    (nth-value 1 (next-entry))))

(defmethod graph-edges ((graph edge-list-graph))
  (graph-edge-list graph))

(defmethod copy-graph ((graph edge-list-graph))
  (let ((new (make-instance (class-of graph))))
    (setf (graph-node-hash new)
          (alexandria:copy-hash-table (graph-node-hash graph))
          (graph-edge-list new)
          (copy-tree (graph-edge-list graph)))
    new))

(defmethod add-edge ((graph edge-list-graph) (node1 node) (node2 node))
  (unless (graph-node-p graph node1)
    (error "Node ~A not in graph ~A" node1 graph))
  (unless (graph-node-p graph node2)
    (error "Node ~A not in graph ~A" node2 graph))
  (let ((edge (cons node1 node2)))
    (push edge (graph-edge-list graph))))

(defmethod add-edge ((graph edge-list-graph)
                     node-identifier-1 node-identifier-2)
  (let ((node1 (get-node graph node-identifier-1))
        (node2 (get-node graph node-identifier-2)))
    (when (and node1 node2)
      (add-edge graph node1 node2))))

(defmethod remove-edge ((graph edge-list-graph)
                        (node1 node) (node2 node))
  (let ((edge (cons node1 node2)))
    (setf (graph-edge-list graph)
          (remove edge (graph-edge-list graph) :test 'equalp))))

(defmethod remove-edge ((graph edge-list-graph)
                        node-identifier-1 node-identifier-2)
  (let ((node1 (get-node graph node-identifier-1))
        (node2 (get-node graph node-identifier-2)))
    (when (and node1 node2)
      (remove-edge graph node1 node2))))

(defmethod edgep ((graph edge-list-graph) (node1 node) (node2 node))
  (find (cons node1 node2) (graph-edges graph) :test 'equalp))

(defmethod edgep ((graph edge-list-graph)
                  node-identifier-1 node-identifier-2)
  (let ((node1 (get-node graph node-identifier-1))
        (node2 (get-node graph node-identifier-2)))
    (when (and node1 node2) (edgep graph node1 node2))))

(defmethod find-edges-from ((graph edge-list-graph) (node node))
  (remove-if-not (lambda (x)
                   (eq node x))
                 (graph-edges graph) :key #'car))

(defmethod find-edges-from ((graph edge-list-graph) node-identifier)
  (let ((node (get-node graph node-identifier)))
    (when node
      (find-edges-from graph node))))

(defmethod find-edges-to ((graph edge-list-graph) (node node))
  (remove-if-not (lambda (x)
                   (eq node x))
                 (graph-edges graph) :key #'cdr))

(defmethod find-edges-to ((graph edge-list-graph) node-identifier)
  (let ((node (get-node graph node-identifier)))
    (when node
      (find-edges-to graph node))))

(defmethod find-edges-containing ((graph edge-list-graph) node)
  (union (find-edges-from graph node)
         (find-edges-to graph node)))

(defmethod find-edges-containing ((graph edge-list-graph) node-identifier)
  (let ((node (get-node graph node-identifier)))
    (when node
      (find-edges-containing graph node))))

(defmethod neighbors (graph element) 
  (let ((edges (find-edges-containing graph element)))
    (union (map (type-of edges) #'car edges)
           (map (type-of edges) #'cdr edges))))

