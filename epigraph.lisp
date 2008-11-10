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
;;; graphs and generic functions for operating on graphs
(defclass graph ()
  ()
  (:documentation "The protocol class of graphs. The intent is that
  there will be concrete subclasses of graph with different
  implementations of storing the nodes and edges."))

(defparameter *default-graph-class* 'edge-list-graph)

(defun make-graph (&optional (graph-class *default-graph-class*))
  (make-instance graph-class))

(defgeneric copy-graph (graph)
  (:documentation "Returns a copy of the graph which will contain
  copies of the edges, but the same nodes as the original graph."))

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
  (let (visited)
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
                      (push node visited)
                      (let ((neighbors (neighbors graph node)))
                        (map nil
                             (lambda (x)
                               (unless (member x visited)
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
  (let (visited)
    (labels
        ((bfs-visit (node-list)
           (let (children)
             (map nil
                  (lambda (node)
                    (funcall fn node)
                    (when (and end-supplied-p
                               (funcall test (funcall key node) end))
                      (return-from bfs-map))
                    (push node visited)
                    (let ((neighbors (neighbors graph node)))
                      (map nil
                           (lambda (x)
                             (unless (member x visited)
                               (push x children)))
                           neighbors)))
                  node-list)
             (when children
               (bfs-visit children)))))
      (bfs-visit (list start)))))

(defclass bfs-node-data ()
  ((visited :accessor visited :initarg :visited :initform nil)
   (parent :accessor parent :initarg :parent)))

;;;
;;; the following function doesn't explicitly store the path back
;;; from the node to the start, but rather stores the parent of each
;;; node in a hashtable. I thought this would be a big win, but
;;; through the magic of cons and sharing list structure, the original
;;; version was actually quite efficient.
(defmethod bfs2 ((graph graph) start end
                 &key
                 (key 'identity)
                 (test 'eql))
  (let ((visited-nodes (make-hash-table)))
    (labels
        ((walk-back (node)
           (let ((parent (gethash node visited-nodes)))
             (if parent
                 (cons node (walk-back parent))
                 (list node))))
         (bfs-visit (nodes node-parents)
           (let (children parents)
             (map nil
                  (lambda (node parent)
                    (setf (gethash node visited-nodes) parent)
                    (when (funcall test (funcall key node) end)
                      (return-from bfs2
                        (nreverse (walk-back node))))
                    (let ((neighbors (neighbors graph node)))
                      (map nil
                           (lambda (x)
                             (unless (nth-value 1 (gethash x visited-nodes))
                               (push x children)
                               (push node parents)))
                           neighbors)))
                  nodes node-parents)
             (when children
               (bfs-visit children parents)))))
      (bfs-visit (list start) (list nil)))))

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

(defmethod add-node ((graph edge-list-graph) (node node))
  (unless (gethash node (graph-node-hash graph))
    (progn
      (setf (gethash node (graph-node-hash graph)) node)
      (when (node-name node)
        (setf (gethash (node-name node) (graph-node-name-hash graph)) node))))
  node)

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
  (add-node graph node1)
  (add-node graph node2)
  (let ((edge (cons node1 node2)))
    (pushnew edge (graph-edge-list graph) :test 'equalp)))

(defmethod add-edge ((graph edge-list-graph)
                     node-identifier-1
                     node-identifier-2)
  (let ((node1 (get-node graph node-identifier-1))
        (node2 (get-node graph node-identifier-2)))
    (add-node graph node1)
    (add-node graph node2)
    (let ((edge (cons node1 node2)))
      (pushnew edge (graph-edge-list graph) :test 'equalp))))

(defmethod remove-edge ((graph edge-list-graph) (node1 node) (node2 node))
  (let ((edge (cons node1 node2)))
    (setf (graph-edge-list graph)
          (remove edge (graph-edge-list graph) :test 'equalp))))

(defmethod edgep ((graph edge-list-graph) (node1 node) (node2 node))
  (find (cons node1 node2) (graph-edges graph) :test 'equalp))

(defmethod find-edges-from ((graph edge-list-graph) node)
  (remove-if-not (lambda (x)
                   (eq node x))
                 (graph-edges graph) :key #'car))

(defmethod find-edges-to ((graph edge-list-graph) node)
  (remove-if-not (lambda (x)
                   (eq node x))
                 (graph-edges graph) :key #'cdr))

(defmethod find-edges-containing ((graph edge-list-graph) node)
  (union (find-edges-from graph node)
         (find-edges-to graph node)))

(defmethod neighbors (graph element) 
  (let ((edges (find-edges-containing graph element)))
    (remove element
            (union (map (type-of edges) #'car edges)
                   (map (type-of edges) #'cdr edges)))))

