;;;
;;; Planar geometry line segments
;;;

#lang racket/base

(require racket/contract/base)
(require racket/match)

(require advent/planar/point)

(provide
 (struct-out segment)
 (contract-out
  [segment-length (-> segment? number?)]
  [segment-manhattan-length (-> segment? number?)]
  [segment-contains? (-> segment? point? boolean?)]
  [segment-intersect? (-> segment? segment? boolean?)]
  [segment-intersect (-> segment? segment? (or/c boolean? point?))]))

;;;
;;; Data structures
;;;

;; A line segment.
(struct segment (p q) #:transparent)

;;;
;;; Properties
;;;

;; Return the length of the line segment.
(define (segment-length s)
  (match-define (segment p q) s)
  (point-distance p q))

;; Return the manhattan length of the line segment.
(define (segment-manhattan-length s)
  (match-define (segment p q) s)
  (point-manhattan-distance p q))

;;;
;;; Intersection
;;;

;; Test whether a segment contains a point.
(define (segment-contains? s r)
  (match-define (segment (point px py) (point qx qy)) s)
  (match-define (point x y) r)
  (and
   ; The point must be colinear to the segment,
   (= (point-orientation (point px py) (point qx qy) r) 0)
   ; and within its bounds.
   (>= x (min px qx))
   (<= x (max px qx))
   (>= y (min py qy))
   (<= y (max py qy))))

;; Test whether two segments intersect.
(define (segment-intersect? s t)
  (match-define (segment a b) s)
  (match-define (segment c d) t)
  (define oa (point-orientation c d a))
  (define ob (point-orientation c d b))
  (define oc (point-orientation a b c))
  (define od (point-orientation a b d))
  (and (< (* oa ob) 0) (< (* oc od) 0)))

;; Return the point at which two segments intersect.
(define (segment-intersect a b)
  (match-define (segment (point apx apy) (point aqx aqy)) a)
  (match-define (segment (point bpx bpy) (point bqx bqy)) b)
  (if (not (segment-intersect? a b)) #f
      (let* ([dx (point (- apx aqx) (- bpx bqx))]
             [dy (point (- apy aqy) (- bpy bqy))]
             [divisor (point-cross-product dx dy)]
             [d (point (point-cross-product (segment-p a) (segment-q a))
                       (point-cross-product (segment-p b) (segment-q b)))])
        (if (= divisor 0) #f
            (point (/ (point-cross-product d dx) divisor)
                   (/ (point-cross-product d dy) divisor))))))
