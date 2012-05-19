;;; file: simple-edge-list-graph.lisp
;;; author: cyrus harmon
;;;
;;; Copyright (c) 2011 Cyrus Harmon (ch-lisp@bobobeach.com)
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
 
(defmethod first-node ((graph simple-edge-list-graph))
  (with-graph-iterator (next-entry graph)
    (nth-value 1 (next-entry))))

(defmethod nodes ((graph simple-edge-list-graph))
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

(defmethod edges ((graph simple-edge-list-graph))
  (graph-edge-list graph))

(defmethod map-edges (fn result-type (graph simple-edge-list-graph))
  (map result-type fn (edges graph)))

(defmethod map-edges->list (fn (graph simple-edge-list-graph))
  (map 'list fn (edges graph)))


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
   (edges graph)))

(defmethod find-edge ((graph simple-edge-list-graph) node1 node2)
  (or (edgep graph node1 node2)
      (edgep graph node2 node1)))

(defmethod find-edges-from ((graph simple-edge-list-graph) node
                            &key (test (graph-node-test graph)))
  (remove-if-not (lambda (x)
                   (funcall test node x))
                 (edges graph) :key #'node1))

(defmethod find-edges-to ((graph simple-edge-list-graph) node
                          &key (test (graph-node-test graph)))
  (remove-if-not (lambda (x)
                   (funcall test node x))
                 (edges graph) :key #'node2))

(defmethod find-self-edges ((graph simple-edge-list-graph) node
                            &key (test (graph-node-test graph)))
  (remove-if-not (lambda (x)
                   (funcall test (node1 x) (node2 x)))
                 (edges graph)))

(defmethod self-edge-p ((graph simple-edge-list-graph) edge
                        &key (test (graph-node-test graph)))
  (funcall test (node1 edge) (node2 edge)))

(defmethod find-edges-containing ((graph simple-edge-list-graph) node
                                  &key (test (graph-node-test graph)))
  (union (apply #'find-edges-from graph node
                (when test `(:test ,test)))
         (apply #'find-edges-to graph node
                (when test `(:test ,test)))))
