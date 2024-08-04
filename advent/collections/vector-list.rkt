;;;
;;; Growable vector container
;;;

#lang racket/base

(require racket/contract/base)
(require racket/generator)
(require racket/struct)
(require racket/vector)

(provide
 (contract-out
  [vector-list? (-> any/c boolean?)]
  [make-vector-list (->* (integer?) () #:rest (listof any/c) vector-list?)]
  [vector-list-ref (-> vector-list? integer? any/c)]
  [vector-list-add! (->* (vector-list?) () #:rest (listof any/c) void)]
  [vector-list-insert! (-> vector-list? integer? any/c void)]
  [vector-list-remove! (-> vector-list? integer? void)]
  [vector-list-length (-> vector-list? integer?)]
  [vector-list-capacity (-> vector-list? integer?)]
  [vector-list->list (-> vector-list? list?)]
  [in-vector-list (-> vector-list? sequence?)]))

(module+ test
  (require rackunit))

;;;
;;; Data structures
;;;

;; A growable vector list.
(struct vector-list (elements size) #:mutable
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (l)
        'vector-list)
      (lambda (l)
        (list
         (vector-list-capacity l)
         (vector-list->list l)))))])

;;;
;;; Constructors
;;;

;; Makes a new vector list with the given capacity and initial elements.
(define (make-vector-list [capacity 10] . elements)
  (define v (make-vector capacity null))
  (for ([i (in-naturals)] [e elements])
    (vector-set! v i e))
  (vector-list v (length elements)))

;; Constructor tests.
(module+ test
  (define constructors-list (make-vector-list 3 'a 'b))
  (check-equal? (vector-list-elements constructors-list) (vector 'a 'b '()))
  (check-equal? (vector-list-size constructors-list) 2))

;;;
;;; Accessors
;;;

;; Returns the element at the given index.
(define (vector-list-ref v i)
  (when (>= i (vector-list-length v))
    (raise-user-error
     'vector-list-ref
     "index is out of range\n  index: ~a\n  valid range: [0, ~a]"
     i (- (vector-list-capacity v) 1)))
  (vector-ref (vector-list-elements v) i))

;; Returns the length of a vector list.
(define (vector-list-length v)
  (vector-list-size v))

;; Returns the capacity of a vector list.
(define (vector-list-capacity v)
  (vector-length (vector-list-elements v)))

;; Accessor tests.
(module+ test
  (define accessors-list (make-vector-list 5 'a 'b 'c))
  (check-equal? (vector-list-ref accessors-list 0) 'a)
  (check-equal? (vector-list-ref accessors-list 1) 'b)
  (check-equal? (vector-list-ref accessors-list 2) 'c)
  (check-equal? (vector-list-length accessors-list) 3)
  (check-equal? (vector-list-capacity accessors-list) 5))

;;;
;;; Mutation
;;;

;; Adds an element to the end of a list.
(define (vector-list-add! v . elements)
  (define new-size (+ (vector-list-length v) (length elements)))
  (vector-list-grow! v (length elements))
  (for ([i (in-naturals (vector-list-length v))] [e elements])
    (vector-set! (vector-list-elements v) i e))
  (set-vector-list-size! v new-size))

;; Insert an element to a list.
(define (vector-list-insert! v i element)
  (when (>= i (vector-list-length v))
    (raise-user-error
     'vector-list-insert!
     "index is out of range\n  index: ~a\n  valid range: [0, ~a]"
     i (- (vector-list-capacity v) 1)))
  (vector-set! (vector-list-elements v) i element))

;; Removes an element from a list.
(define (vector-list-remove! v i)
  (define elements (vector-list-elements v))
  (when (>= i (vector-list-length v))
    (raise-user-error
     'vector-list-remove!
     "index is out of range\n  index: ~a\n  valid range: [0, ~a]"
     i (- (vector-list-capacity v) 1)))
  (vector-copy! elements i elements (+ i 1) (vector-list-length v))
  (set-vector-list-size! v (- (vector-list-length v) 1)))

;; Mutation tests.
(module+ test
  (define mutation-list (make-vector-list 10 'a 'b 'c))
  (define small-list (make-vector-list 2 'a 'b))
  (vector-list-add! mutation-list 'd 'e)
  (vector-list-insert! mutation-list 0 'f)
  (vector-list-remove! mutation-list 1)
  (vector-list-add! small-list 'c)
  (check-equal? (vector-list-length mutation-list) 4)
  (check-equal? (vector-list-ref mutation-list 0) 'f)
  (check-equal? (vector-list-ref mutation-list 1) 'c)
  (check-equal? (vector-list-ref mutation-list 2) 'd)
  (check-equal? (vector-list-ref mutation-list 3) 'e)
  (check-equal? (vector-list-length small-list) 3)
  (check-equal? (vector-list-ref small-list 2) 'c))

;;;
;;; Conversion and iteration
;;;

;; Returns a list containing the elements of the vector list.
(define (vector-list->list v)
  (define elements (vector-list-elements v))
  (vector->list (vector-take elements (vector-list-length v))))

;; Returns a sequence that produces elements from the list.
(define (in-vector-list v)
  (in-generator
   (for ([n (in-range 0 (vector-list-size v))])
     (yield (vector-ref v n)))))

;; Conversion and iteration tests.
(module+ test
  (define conversion-list (make-vector-list 10 'a 'b 'c))
  (check-equal? (vector-list->list conversion-list) (list 'a 'b 'c)))

;;;
;;; Internal
;;;

;; Adds enough capacity to a list to hold additional elements.
(define (vector-list-grow! v [n 1])
  (define elements (vector-list-elements v))
  (define capacity (vector-list-capacity v))
  (define len (vector-list-length v))
  (when (> (+ len n) capacity)
    (let* ([new-capacity (max (* capacity 2) (+ capacity n))]
           [new-elements (vector-extend elements new-capacity null)])
      (set-vector-list-elements! v new-elements))))
