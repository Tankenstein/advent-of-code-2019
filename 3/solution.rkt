#lang racket

; input parsing
(define (parse-input-operation i)
  (list (substring i 0 1) (string->number (substring i 1))))

(define (parse-input filename)
  (let ([lines (string-split (file->string filename) "\n")])
    (map (lambda (line) (map parse-input-operation (string-split line ","))) lines)))

; move a coord once in a given direction
(define (move-direction coord direction)
  (case direction
    [("U") (list (car coord)        (add1 (cadr coord)))]
    [("D") (list (car coord)        (sub1 (cadr coord)))]
    [("L") (list (sub1 (car coord)) (cadr coord))]
    [("R") (list (add1 (car coord)) (cadr coord))]))

; apply to coords a single operation
(define (apply-operation coord operation)
  (let ([operation-count (cadr operation)]
        [direction (car operation)])
    (foldl (lambda (_ lst)
             (cons (move-direction
                     (if (empty? lst) coord (car lst)) direction)
                   lst))
      '()
      (range operation-count))))

; follow a wire and build a list of coordinates.
(define (build-path operations)
  (let ([path (foldl (lambda (operation lst)
                       (append (apply-operation (car lst) operation) lst))
                     '((0 0))
                     operations)])
    (drop-right path 1))) ; drop the original 0 point

(define (distance-from-start coord)
  (+ (abs (car coord)) (abs (cadr coord))))

(define (find-path-intersections paths)
  (let ([path-sets (map list->set paths)])
    (set->list (apply set-intersect path-sets))))

(define (reverse-index-of lst element)
  (- (length lst) (index-of lst element)))

; main program starts here
(define input-paths (map build-path (parse-input "input")))

; solution to problem 1
(define (find-closest-intersection paths)
  (let* ([intersections (find-path-intersections paths)]
         [distances (map distance-from-start intersections)])
    (apply min distances)))

(define (find-fewest-step-intersection paths) 
  (let* ([intersections (find-path-intersections paths)]
         [steps (map (lambda (intersection)
                       (foldl (lambda (path sum) (+ sum (reverse-index-of path intersection)))
                              0
                              paths)) intersections)])
    (apply min steps)))


(find-closest-intersection input-paths)
(find-fewest-step-intersection input-paths)
