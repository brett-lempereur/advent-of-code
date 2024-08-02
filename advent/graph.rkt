;;;
;;; Graphs and trees
;;;

#lang racket/base

(require racket/contract/base)
(require racket/set)
(require racket/struct)

(require data/heap)
(require threading)

(provide
 (struct-out graph)
 (contract-out
  [graph-neighbours (-> graph? any/c (set/c any/c))]
  [graph-weight (-> graph? any/c any/c number?)]
  [make-graph (-> graph?)]
  [graph-add-vertex (-> graph? any/c graph?)]
  [graph-add-edge (->* (graph? any/c any/c) (number?) graph?)]
  [graph-add-directed-edge (->* (graph? any/c any/c) (number?) graph?)]
  [graph-dijkstra (-> graph?
                      any/c
                      (values (hash/c any/c number?) (hash/c any/c any/c)))]
  [graph-a* (->* (graph? any/c any/c)
                 ((-> any/c number?))
                 (or/c boolean? (listof any/c)))]
  [graph-uniform-heuristic (-> any/c number?)]))

(module+ test
  (require rackunit))

;;;
;;; Data structures
;;;

;; A graph with edge weights.
(struct graph (vertices edges weights)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (g)
        'graph)
      (lambda (g)
        (list
         (set-count (graph-vertices g))
         (apply + (map set-count (hash-values (graph-edges g))))))))])

;;;
;;; Accessors
;;;

;; Return the neighbours of a vertex.
(define (graph-neighbours g u)
  (hash-ref (graph-edges g) u (set)))

;; Return the weight of the edge between two vertices.
(define (graph-weight g u v)
  (hash-ref (graph-weights g) (cons u v)))

;; Accessor tests.
(module+ test
  (define accessors-graph (graph (set 'a) (hash 'a (set 'c)) (hash '(a . c) 2)))
  (check-equal? (graph-neighbours accessors-graph 'a) (set 'c))
  (check-equal? (graph-neighbours accessors-graph 'c) (set))
  (check-equal? (graph-weight accessors-graph 'a 'c) 2))

;;;
;;; Builders
;;;

;; Return an empty graph.
(define (make-graph)
  (graph (set) (hash) (hash)))

;; Add a vertex to the graph if it does not already exist.
(define (graph-add-vertex g u)
  (struct-copy graph g [vertices (set-add (graph-vertices g) u)]))

;; Add an undirected edge to the graph if it does not already exist.
(define (graph-add-edge g u v [w 1])
  (~> g
      (graph-add-directed-edge u v w)
      (graph-add-directed-edge v u w)))

;; Add a directed edge to the graph, if it already exists updates the
;; weight.
(define (graph-add-directed-edge g u v [w 1])
  (define vertices (set-union (graph-vertices g) (set u v)))
  (define edges (set-add (graph-neighbours g u) v))
  (struct-copy graph g
               [vertices vertices]
               [edges (hash-set (graph-edges g) u edges)]
               [weights (hash-set (graph-weights g) (cons u v) w)]))

;; Builder tests.
(module+ test
  (define builders-graph
    (~> (make-graph)
        (graph-add-edge 'a 'b 2)
        (graph-add-directed-edge 'a 'c 1)
        (graph-add-vertex 'd)))
  (check-equal? (graph-vertices builders-graph) (set 'a 'b 'c 'd))
  (check-equal? (graph-edges builders-graph) (hash 'a (set 'b 'c) 'b (set 'a)))
  (check-equal? (graph-weight builders-graph 'a 'b) 2))

;;;
;;; Pathfinding
;;;

;; Find the distance and path to all reachable vertices.
(define (graph-dijkstra g start)
  (define distances (make-hash))
  (define previous (make-hash))
  ; Setup.
  (for ([v (graph-vertices g)])
    (hash-set! distances v +inf.0)
    (hash-set! previous v null))
  (hash-set! distances start 0)
  ; Loop.
  (let loop ([unvisited (graph-vertices g)])
    (let ([u (car (sort (set->list unvisited) <
                        #:key (λ (v) (hash-ref distances v))))])
      (for ([v (set-intersect unvisited (graph-neighbours g u))])
        (let ([d (+ (hash-ref distances u) (graph-weight g u v))])
          (when (< d (hash-ref distances v))
            (hash-set! distances v d)
            (hash-set! previous v u))))
      (let ([new-unvisited (set-remove unvisited u)])
        (if (set-empty? new-unvisited)
            (values distances previous)
            (loop new-unvisited))))))

;; Find the optimal path between two vertices, if one exists, using the
;; A* search algorithm.
(define (graph-a* g start goal [heuristic graph-uniform-heuristic])
  (define heap (make-heap (λ (a b) (<= (cdr a) (cdr b)))))
  (define visited (mutable-set))
  (define came-from (make-hash))
  (define scores (make-hash))
  ; Setup, add the start node score mappings.
  (hash-set! scores start 0)
  ; Search through the best opportunities until we reach the target
  ; or exhaust all paths.
  (let loop ([current start])
    (set-add! visited current)
    (if (equal? current goal)
        ; We have reached the goal so reconstruct the path.
        (let reconstruct-loop ([current current] [path (list)])
          (if (hash-has-key? came-from current)
              (reconstruct-loop (hash-ref came-from current)
                                (cons current path))
              path))
        ; We are still searching, process neighbours of the current
        ; node.
        (begin
          (for ([neighbour (graph-neighbours g current)])
            (let ([score (+ (hash-ref scores current)
                            (graph-weight g current neighbour))])
              (when (< score (hash-ref scores neighbour +inf.0))
                (hash-set! came-from neighbour current)
                (hash-set! scores neighbour score)
                (when (not (set-member? visited neighbour))
                  (let ([h (+ score (heuristic neighbour))])
                    (heap-add! heap (cons neighbour h)))))))
          (if (= (heap-count heap) 0)
              #f
              (let ([next (car (heap-min heap))])
                (heap-remove-min! heap)
                (loop next)))))))

;; A uniform heuristic that collapses the A* search algorithm to a
;; breadth-first search.
(define (graph-uniform-heuristic u)
  0)
