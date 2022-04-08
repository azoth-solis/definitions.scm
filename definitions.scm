#lang sicp
(#%require racket/trace)

;;; x:
(define x (list 1 2 3))

;;; z:
(define z (list 1 2 (list 3 4 (list 5 6) 7 8) 9 10))

;;; Double:
(define (double x) (* x 2))

;;; Even:
(define (even? x)
  (if (= (remainder x 2) 0)
      #t
      #f))

;;; Odd:
(define (odd? x)
  (if (= (remainder x 2) 1)
      #t
      #f))

;;; Square:
(define (square x) (* x x))

;;; Factorial:
(define (factorial x)
  (if (= x 0)
      1
      (* x (factorial (- x 1)))))

;;; Length:
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

;;; Reverse a list:
(define (reverse items)
  (define (reverse-iter items result)
    (if (null? items)
        result
        (reverse-iter (cdr items) (cons (car items) result))))
  (trace reverse-iter)
  (reverse-iter items nil))

;;; Reverse a tree:
(define (deep-reverse items)
  (define (deep-reverse-iter items result)
    (cond ((null? items)
           result)
          ((not (pair? (car items)))
           (deep-reverse-iter (cdr items)
                              (cons (car items) result)))
          (else (deep-reverse-iter (cdr items)
                                   (cons (deep-reverse-iter (car items) nil) result)))))
  (trace deep-reverse-iter)
  (deep-reverse-iter items nil))

;;; 2.28: Fringe:
(define (fringe items)
  (define (fringe-iter items result)
    (cond ((null? items) result)
          ((not (pair? (car items))) (fringe-iter (cdr items) (cons (car items) result)))
          (else (fringe-iter (cdr items) (fringe-iter (car items) result)))))
  (trace fringe-iter)
  (reverse (fringe-iter items nil)))

;;; Scale a tree (V.1):
(define (scale-tree-1 tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree-1 (car tree) factor) (scale-tree-1 (cdr tree) factor)))))
(trace scale-tree-1)

;;; Scale a tree (V.2):
(define (scale-tree-2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-2 sub-tree factor)
             (* sub-tree factor)))
       tree))
(trace scale-tree-2)

;;; Square a tree (V.1):
(define (square-tree-1 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree-1 (car tree)) (square-tree-1 (cdr tree))))))
(trace square-tree-1)

;;; Square a tree (V.2):
(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-2 sub-tree)
             (* sub-tree sub-tree)))
       tree))
(trace square-tree-2)

;;; Map over a tree:
(define (tree-map fn tree)
  (cond ((null? tree)
         nil)
        ((not (pair? tree))
         (fn tree))
        (else (cons (tree-map fn (car tree))
                    (tree-map fn (cdr tree))))))
(trace tree-map)

;;; Init:
(define (init items)
  (reverse (cdr (reverse items))))

;;; Last:
(define (last items)
  (car (reverse items)))

;;; Append two lists:
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
(trace append)

;;; Map over a list:
(define (map fn seq)
  (if (null? seq)
      nil
      (cons (fn (car seq)) (map fn (cdr seq)))))
(trace map)

;;; (Fix this!) Find the set of all subsets of a set:
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) x) rest))))) ; Find the lambda function.
(trace subsets)

;;; Filter:
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(trace filter)

;;; Accumulate:
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(trace accumulate)

;;; Enumerate a list:
(define (enumerate low high)
  (if (> low high)
      nil
      (cons low (enumerate (+ low 1) high))))

;;; Enumerate a tree:
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(trace enumerate-tree)

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))
