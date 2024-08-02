#lang info

;; Collection structure
(define collection 'multi)

;; Package metadata
(define pkg-desc "Client and data structures for Advent of Code")
(define pkg-authors '("Brett Lempereur"))
(define version "0.1")
(define license 'MIT)

;; Dependencies
(define deps '("base"
               "data-lib"
               "http-easy"
               "html-parsing"
               "sxml"
               "threading-lib"))
(define build-deps '("rackunit-lib"))
