;;; file: epigraph-test.lisp
;;; author: cyrus harmon
;;;
;;; Copyright (c) 2008-2012 Cyrus Harmon (ch-lisp@bobobeach.com)
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

(in-package :epigraph-test)

(5am:def-suite :epigraph)
(5am:in-suite :epigraph)

(5am:test (graph.1)
  (let ((g (make-graph :node-test 'equal)))
    (let ((n1 (add-node g "n1"))
          (n2 (add-node g "n2")))
      (add-edge-between-nodes g n1 n2)
      (5am:is (edgep g n1 n2))
      (add-node g "n3")
      (add-edge-between-nodes g "n2" "n3")
      (5am:is (edgep g n2 "n3"))
      (add-edge-between-nodes g "n3" (add-node g "n4"))
      (5am:is (edgep g "n3" "n4"))
      (5am:is (edgep g "n4" "n3")))))

(5am:test (graph.2)
  (let ((g (make-graph :node-test 'equal)))
    (let ((n1 (add-node g "n1"))
          (n2 (add-node g "n2")))
      (add-edge-between-nodes g n1 n2)
      (add-node g "n3")
      (add-edge-between-nodes g "n2" "n3")
      (add-edge-between-nodes g "n3" (add-node g "n4")))
    (let ((g2 (copy-graph g)))
      (5am:is (= (node-count g)
                 (node-count g2))))))

(defparameter *big-graph* (graph:make-graph :node-test 'equal))

(5am:test (large-graph-test.1)
  (let ((g (make-graph :node-test 'equal)))
    (loop for i below 500
       do (add-node g (princ-to-string i)))
    (loop for i below 499
       do (add-edge-between-nodes g
                                  (princ-to-string i)
                                  (princ-to-string (1+ i))))
    (loop for i below 6000
       for j = (random 500)
       for k  = (random 500)
       do (add-edge-between-nodes g
                                  (princ-to-string j)
                                  (princ-to-string k)))
    g))

(5am:test (large-graph-test-2)
  (let ((g (make-graph)))
    (loop for i below 500
          do (add-node g i))
    (loop for i below 499
       do (add-edge-between-nodes g i (1+ i)))
    (loop for i below 6000
          for j = (random 500)
          for k  = (random 500)
          do (add-edge-between-nodes g j k))
    g))

(5am:test (find-connected-components.1)
  (let ((g (make-graph)))
    (add-node g "n1")
    (add-node g "n2")
    (add-node g "n3")
    (let ((components (find-connected-components g)))
      (5am:is (null (set-difference (mapcar #'nodes components)
                                    '(("n1") ("n2") ("n3"))
                                    :test #'equal)))
      (5am:is  (equal (mapcar #'edges components)
                      (list nil nil nil))))))

(5am:test (find-connected-components.2)
  (let ((g (make-graph)))
    (declare (optimize (debug 3)))
    (add-node g "n1")
    (let ((n2 (add-node g "n2"))
          (n3 (add-node g "n3")))
      (add-edge-between-nodes g n2 n3)
      (let ((components (find-connected-components g)))
        (mapcar #'nodes components)
        (5am:is (null (set-difference (mapcar (lambda (x)
                                                (sort (nodes x)
                                                      #'string<))
                                              components)
                                      '(("n1") ("n2" "n3"))
                                      :test #'equal)))
        (5am:is (and (edgep g "n2" "n3")
                     (null (set-difference
                            (mapcar #'edges components)
                            (list nil (list (edgep g "n2" "n3")))
                            :test #'equal))))))))

(5am:test (find-connected-components.3)
  (let ((g (make-graph)))
    (let ((n1 (add-node g "n1"))
          (n2 (add-node g "n2"))
          (n3 (add-node g "n3"))
          (n4 (add-node g "n4")))
      (add-edge-between-nodes g n1 n2)
      (add-edge-between-nodes g n3 n4)
      (let ((components (find-connected-components g)))
        (5am:is (= (length components) 2))
        (5am:is (null (set-difference (mapcar (lambda (x)
                                                (sort (nodes x)
                                                      #'string<))
                                              components)
                                      '(("n1" "n2") ("n3" "n4"))
                                      :test #'equal)))))))

;;; edge tests

;;; add-edge-between-nodes and edgep
(5am:test (add-edge-between-nodes.1)
  (let ((g (make-graph :node-test 'equal)))
    (let ((n1 (add-node g "n1"))
          (n2 (add-node g "n2")))
      (let ((edge (add-edge-between-nodes g n1 n2)))
        (5am:is (edgep g n1 n2))
        (5am:is (edgep g "n1" "n2"))
        (5am:is (edgep g n1 "n2"))
        (5am:is (null (set-difference (nodes edge)
                                      (list n1 n2))))))
    g))

;;; remove-edge
(5am:test (remove-edge.1)
  (let ((g (make-graph :node-test 'equal)))
    (let ((n1 (add-node g "n1"))
          (n2 (add-node g "n2")))
      (let ((edge (add-edge-between-nodes g n1 n2)))
        (5am:is (edgep g n1 n2))
        (5am:is (null (set-difference (nodes edge)
                                      (list n1 n2))))
        (remove-edge g edge)
        (5am:is (null (edgep g n1 n2)))))
    g))

;;; other-edge-node
(5am:test (other-edge-node.1)
  (let ((g (make-graph :node-test 'equal)))
    (let ((n1 (add-node g "n1"))
          (n2 (add-node g "n2")))
      (let ((edge (add-edge-between-nodes g n1 n2)))
        (5am:is (eq n2 (other-edge-node edge n1)))
        (5am:is (eq n1 (other-edge-node edge n2)))
        (5am:is (eq n2 (other-edge-node edge "n1")))
        (5am:is (equal "n2" (other-edge-node edge n1)))
        (5am:is (equal "n2" (other-edge-node edge "n1")))

        ;; make sure we return null for objects that aren't in the graph
        ;; -- or should we error here?
        (5am:is (null (other-edge-node edge g)))))
    g))


