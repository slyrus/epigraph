;;; file: package.lisp
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

(cl:defpackage #:epigraph
  (:use #:cl)
  (:nicknames #:graph)
  (:export #:edge
           #:node1
           #:node2
           #:edge-nodes
           #:edge-data
           #:other-edge-node
           #:edges-nodes-equal
           #:*default-edge-class*
           #:print-edge-data
           #:copy-edge

           #:graph
           #:nodes
           #:make-graph
           #:*default-graph-class*

           #:add-node
           #:get-node
           #:remove-node
           #:node-count
           #:copy-graph
           #:add-edge
           #:add-edge-between-nodes
           #:remove-edge
           #:remove-edge-between-nodes
           #:edgep
           #:self-edge-p
           #:edges
           #:find-edges-from
           #:find-edges-to
           #:find-edges-containing
           #:neighbors
           
           #:node-position
           #:node-find
           #:node-remove
           #:node-equal

           #:map-nodes
           #:map-nodes->list
           #:map-edges
           #:map-edges->list

           #:bfs
           #:bfs-map
           #:bfs-map-edges
           #:dfs
           #:dfs-map
           #:dfs-map-edges

           #:simple-edge-list-graph
           #:first-node
           
           #:find-cycle
           #:find-cycle-edges
           #:break-cycles
           
           #:remove-connected-component
           #:find-connected-components

           #:find-longest-paths
           #:find-longest-path
           #:graph-distance
           #:graph-distance-hash-table
           #:graph-distance-matrix))
