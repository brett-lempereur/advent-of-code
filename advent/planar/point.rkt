;;;
;;; Planar geometry points
;;;

#lang racket/base

(require racket/contract)
(require racket/match)

(provide
 (struct-out point)
 (contract-out
  [point+ (-> point? point? point?)]))

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
