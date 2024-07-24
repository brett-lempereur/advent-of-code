;;;
;;; Functions for retrieving Advent of Code puzzles and examples.
;;;

#lang racket/base

(require racket/contract/base)
(require racket/file)
(require racket/port)
(require racket/string)

(require html-parsing)
(require net/http-easy)
(require sxml)
(require sxml/sxpath)

(provide
 (struct-out exn:fail:advent:fetch)
 (struct-out exn:fail:advent:example)
 (contract-out
  [puzzle-input-port (-> integer? integer? input-port?)]
  [puzzle-example-port (-> integer? integer? integer? input-port?)]
  [puzzle-clear-cache (-> void?)]))

;;;
;;; Puzzles and examples
;;;

;; A failure when attempting to retrieve an index.
(struct exn:fail:advent:example exn:fail ())

;; Return a port that produces input for a puzzle.
(define (puzzle-input-port year day)
  (define path (format "~a/day/~a/input" year day))
  (cache-port path))

;; Return a port that produces example input for a puzzle.
(define (puzzle-example-port year day index)
  (define path (format "~a/day/~a" year day))
  (define document (html->xexp (cache-port path)))
  (define blocks ((sxpath "//pre/code") document))
  (when (>= index (length blocks))
    (raise (exn:fail:advent:example
            (format "example index out of range: ~a" index)
            (current-continuation-marks))))
  (open-input-string (sxml:text (list-ref blocks index))))

;;;
;;; Cache
;;;

;; Return a port that produces the document at the given path on the
;; server, caching the response and reading that subsequently.
(define (cache-port path)
  (define filename (string-join (map path->string (explode-path path)) "-"))
  (define cache-path (build-path (puzzle-cache-path) filename))
  (unless (file-exists? cache-path)
    (make-parent-directory* cache-path)
    (with-output-to-file cache-path
      (lambda () (copy-port (server-port path) (current-output-port)))))
  (open-input-file cache-path))

;; Clears the input and example caches.
(define (puzzle-clear-cache)
  (delete-directory/files (puzzle-cache-path)))

;; Return the path of the puzzle and example cache.
(define (puzzle-cache-path)
  (build-path (find-system-path 'cache-dir) "advent-of-code" "cache"))

;;;
;;; Networking
;;;

;; A failure while attempting to fetch a document.
(struct exn:fail:advent:fetch exn:fail ())

;; Return a port that produces the document at the given path on the
;; server.
(define (server-port path)
  (define headers (hasheq 'cookie (format "session=~a" (user-session-token))))
  (define url (format "https://adventofcode.com/~a" path))
  (define response (get url #:headers headers #:stream? #t))
  (unless (eq? (response-status-code response) 200)
    (raise (exn:fail:advent:fetch
            (format "failed to fetch ~s: ~a"
                    path
                    (response-status-line response))
            (current-continuation-marks))))
  (response-output response))

;; Return the current session token.
(define (user-session-token)
  (define path (build-path (find-system-path 'home-dir) ".advent-of-code"))
  (with-input-from-file path
    (lambda () (string-trim (port->string)))))
