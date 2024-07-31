;;;
;;; Planar geometry points
;;;

#lang racket/base

(require racket/contract/base)
(require racket/match)

(provide
 (struct-out point)
 (contract-out
  [point+ (-> point? point? point?)]
  [point- (-> point? point? point?)]
  [point-cross-product (-> point? point? number?)]
  [point-dot-product (-> point? point? number?)]
  [point-orientation (-> point? point? point? number?)]
  [point-distance (-> point? point? number?)]
  [point-manhattan-distance (-> point? point? number?)]))

;;;
;;; Data structures
;;;

;; A point.
(struct point (x y) #:transparent)

;;;
;;; Arithmetic
;;;

;; Add two points.
(define (point+ p q)
  (match-define (point px py) p)
  (match-define (point qx qy) q)
  (point (+ px qx) (+ py qy)))

;; Subtract two points.
(define (point- p q)
  (match-define (point px py) p)
  (match-define (point qx qy) q)
  (point (- px qx) (- py qy)))

;; Compute the cross product of two points.
(define (point-cross-product p q)
  (match-define (point px py) p)
  (match-define (point qx qy) q)
  (- (* px qy) (* py qx)))

;; Compute the dot product of two points.
(define (point-dot-product p q)
  (match-define (point px py) p)
  (match-define (point qx qy) q)
  (+ (* px qx) (* py qy)))

;; Compute the orientation of three points.
(define (point-orientation p q r)
  (point-cross-product (point- q p) (point- r p)))

;;;
;;; Distance
;;;

;; Return the distance between two points.
(define (point-distance p q)
  (match-define (point px py) p)
  (match-define (point qx qy) q)
  (sqrt (+ (expt (- px qx) 2) (expt (- py qy) 2))))

;; Return the manhattan distance between two points.
(define (point-manhattan-distance p q)
  (match-define (point px py) p)
  (match-define (point qx qy) q)
  (+ (abs (- px qx)) (abs (- py qy))))
