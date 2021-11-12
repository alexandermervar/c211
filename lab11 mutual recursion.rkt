;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |lab11 mutual recursion|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A PrefixForest is a [NEListOf PrefixTree]
 
; A PrefixTree is one of:
; - (make-end)
; - (make-initial 1String PrefixForest)
 
(define-struct end [])
(define-struct initial [letter forest])
 
; A 1String a String of length 1

; ------------------------------------
; a Word is a [Listof String]

; EXERCISE 5
; word->tree: Word -> PrefixTree
; Takes a Word and returns a PrefixTree
; which stores just the word.

; (define (word->tree w) ...)

(define (process-word w)
(cond
  [(empty? w) ...]
  [else ...]))

(check-expect (word->tree (list "s", "t", "r",
                                "i", "n", "g"))
                          (make-initial "s" (list (make-intial "t" (list (make-initial "r"
                                       (list (make-initial "i" (list (make-initial "n"
                                              (list (make-initial "g" (list (make-end))))))))))))))


(define (word->tree w)
  (cond
    [(empty? w) (make-end)]
    [else (make-initial (first w) (list (word->tree (rest w))))]))

; word-in-tree? : Word  PrefixTree-> Boolean
; determine whether or not a
; given Word is stored in a given PrefixTree

; (define (word-in-tree? w pt) ...)

(define (process-tree t)
  (cond
    [(end? t) ...]
    [(initial? t) ...(initial-letter t)...
                  (process-forest (initial-forest t)) ...]))

(define (word-in-tree? w pt)
  (cond
    [(end? t) (empty? w)]
    [(initial? t) (and (string=? (first w) (initial-letter t))
                  (word-in-forest? (rest w) (initial-forest t)))]))

; word-in-forest? : Word PrefixForest-> Boolean
; determine whether or not a
; given Word is stored in a given PrefixForest

; (define (word-in-tree? w pt) ...)

(define (process-forest f)
  (cond
    [(empty? (rest f)) (process-tree (first f))]
    [else ...(process-tree (first f)) ...
          (process-forest (rest f))...]))

(define (word-in-forest? w pf)
  (cond
    [(empty? (rest pf)) (word-in-tree? w (first pf))]
    [else (or (word-in-tree? w (first pf))
          (word-in-forest? w (rest pf)))]))
; ------------------------------------
(define forest1
  (list
   (make-initial "n"
                 (list
                  (make-end)
                  (make-initial "e" (list (make-end)))))
   (make-initial "f"
                 (list
                  (make-end)
                  (make-initial "f" (list (make-end)))
                  (make-initial "t" (list (make-end)))))
   (make-initial "r" (list (make-end)))))
(define tree1 (make-initial "o" forest1))