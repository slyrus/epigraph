;;; file: epigraph-test.lisp
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

(in-package :epigraph-test)

(defparameter *node* (make-instance 'node :name "test node 1"))
(print *node*)

(defparameter *graph* 
  (let ((g (make-graph)))
    (let ((n1 (add-node g "n1"))
          (n2 (add-node g "n2")))
      (add-edge g n1 n2)
      (add-node g "n3")
      (add-edge g "n2" "n3")
      (add-edge g "n3" (add-node g "n4")))
    g))

(defparameter *graph-copy* (copy-graph *graph*))

(defparameter *big-graph* (epigraph::make-graph))

(defun test-me ()
  (print 'ok)
  (loop for i below 500
     do (add-node *big-graph* (epigraph::make-node (princ-to-string i))))
  (loop for i below 499
     do (add-edge *big-graph* (princ-to-string i) (princ-to-string (1+ i))))
  (loop for i below 6000
     for j = (random 500)
     for k  = (random 500)
     do 
       #+nil (print (cons j k))
       (add-edge *big-graph* (princ-to-string j) (princ-to-string k))))

(test-me)

(time
 (epigraph::bfs3 *big-graph*
                 (epigraph::find-node *big-graph* "1")
                 "408"
                 :key #'node-name
                 :test 'equal))

(time
 (epigraph::dfs *big-graph*
                (epigraph::find-node *big-graph* "1")
                "408"
                :key #'node-name
                :test 'equal))


