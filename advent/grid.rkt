;;;
;;; Finite two-dimensional grids
;;;

#lang racket/base

(require racket/contract/base)
(require racket/function)
(require racket/generator)
(require racket/list)
(require racket/sequence)
(require racket/set)
(require racket/struct)

(require advent/planar/point)

(provide
 (struct-out grid)
 cardinal-neighbours
 ordinal-neighbours
 (contract-out
  [port->grid (->* (input-port?) ((-> char? any/c)) grid?)]
  [grid-ref (-> grid? point? any/c)]
  [grid-pointf (-> grid? (-> any/c boolean?) (listof point?))]
  [grid-contains? (-> grid? point? boolean?)]
  [in-coordinates (-> grid? (sequence/c point?))]
  [in-neighbours (->* (grid? point?) ((listof point?)) (sequence/c point?))]
  [in-neighboursf (->* (grid? point? (-> any/c boolean?)) ((listof point?)) (sequence/c point?))]
  [in-regionf (->* (grid? point? (-> any/c boolean?)) ((listof point?)) (sequence/c point?))]))

;;;
;;; Data structures
;;;

;; A grid indexed by point with arbitrarily valued cells.
;;
;; The origin is at the top-left corner of the grid and positive axes move away
;; from the origin, such that the top-left cell is at `(point 0 0)` and the
;; bottom-right cell is at `(point (- width 1) (- height 1))`.
(struct grid (cells width height)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (g)
        'grid)
      (lambda (g)
        (list
         (grid-width g)
         (grid-height g)))))])

;;;
;;; Parsing
;;;

;; Reads a standard format grid from a port.
;;
;; Assumes that cells are arranged in lines and columns, and that the first
;; line represents the top of the grid.
(define (port->grid port [cell-converter identity])
  (define-values (cells mx my)
    (for/fold ([cells (hash)] [mx 0] [my 0])
              ([y (in-naturals)] [line (in-lines port)])
      (for/fold ([cells cells] [mx mx] [my my])
                ([x (in-naturals)] [char (in-string line)])
        (let ([p (point x y)]
              [v (cell-converter char)])
          (values (hash-set cells p v) (max x mx) (max y my))))))
  (grid cells (+ mx 1) (+ my 1)))

;;;
;;; Accessors
;;;

;; The cardinal neighbours of a cell.
(define cardinal-neighbours (list (point 0 -1) (point 1 0) (point 0 1) (point -1 0)))

;; The ordinal neighbours of a cell.
(define ordinal-neighbours (list (point -1 -1) (point 1 1) (point 1 -1) (point -1 1)))

;; Returns the value of a grid cell.
(define (grid-ref g p)
  (hash-ref (grid-cells g) p))

;; Returns a list of the points in a grid where the value satisfies the given
;; predicate.
(define (grid-pointf g predicate)
  (define width (grid-width g))
  (define height (grid-height g))
  (for*/fold ([output '()]) ([x (in-range 0 width)] [y (in-range 0 height)])
    (let ([p (point x y)])
      (if (predicate (grid-ref g p))
          (cons p output)
          output))))

;; Holds if the grid contains the given coordinate.
(define (grid-contains? g p)
  (and (>= (point-x p) 0) (< (point-x p) (grid-width g))
       (>= (point-y p) 0) (< (point-y p) (grid-height g))))

;;;
;;; Generators and iteration
;;;

;; Returns a generator that yields the coordinates of a grid.
(define (in-coordinates g)
  (in-generator
   (for* ([x (in-range 0 (grid-width g))] [y (in-range 0 (grid-height g))])
     (yield (point x y)))))

;; Returns a generator that yields the coordinates of neighbouring cells.
(define (in-neighbours g p [d cardinal-neighbours])
  (in-generator
   (for ([dp d])
     (let ([np (point+ p dp)])
       (when (grid-contains? g np)
         (yield np))))))

;; Returns a generator that yields the coordinates of neighbouring cells with a
;; value that satisfies the given predicate.
(define (in-neighboursf g p predicate [d cardinal-neighbours])
  (sequence-filter (λ (n) (predicate (grid-ref g n))) (in-neighbours g p d)))

;; Returns a generator that yields the coordinates of a region that satisfies
;; some policy.
(define (in-regionf g p predicate [d cardinal-neighbours])
  (define next-neighbours (λ (n) (for/set ([n (in-neighboursf g n predicate d)]) n)))
  (in-generator
   (let loop ([visited (set)] [unvisited (set p)])
     (unless (set-empty? unvisited)
       (let* ([current-point (set-first unvisited)]
              [next-visited (set-add visited current-point)]
              [next-unvisited
               (set-subtract (set-union (set-remove unvisited current-point)
                                        (next-neighbours current-point))
                             next-visited)])
         (yield current-point)
         (loop next-visited next-unvisited))))))
