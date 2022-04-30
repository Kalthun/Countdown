;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname countdown) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
         ;;;;;;;;;;;;;;;;;;;;         
         ;; James McDonagh ;;
         ;;  ID: 20840074  ;;
         ;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               ``                 ;;
;;             `:oo:`               ;;
;;            -oooooo/.             ;;
;;          .+ss+::/ooo+`           ;;
;;          ./o:-::--/o.            ;;
;;          `/s+-::-:oo+.           ;;
;;          `/o/--:--:o-            ;;
;;          `/o+:::::oo+.           ;;
;;          `:o/--:--:o:            ;;
;;          `:o+::/::+o+-           ;;
;;          `:o+::::-:o/`           ;;
;;          -oo/:/::+so:            ;;
;;          :oo::/:-:o/`            ;;
;;          -oo/://:+so:            ;;
;;          -oo/:/:::o+.            ;;
;;          .+s/://:/ss:`           ;;
;;          .+o/://::+o.            ;;
;;          .+s+:////ss/`           ;;
;;          .+s+:///:+s-.           ;;
;;   -yo./ho.ods//+/+hmy/hmy/dmh:   ;;
;; .smmdhdmdhdmmmyohmmmhyhdhyhmmmd- ;;
;; +mNmdhdmdhhhdmdhdmhyyhdmdhdmmh:  ;;
;;   `+dd/+dNmdhdmdhdmdhdmmd/`:y:   ;;
;;        `+dy-/dmddm+`:h+`         ;;
;;             /mmdmNo`             ;;
;;             /dNNNNs.             ;;
;;             -hNNNNs.             ;;
;;             :dNdmNd:             ;;
;;             .ymdmNd/`            ;;
;;           .sNNmddmNNd/           ;;
;;           :hNNmddmNNh:           ;;
;;             -hNNNNd/`            ;;
;;               -hd+               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2)

;; An Operator (Op) is (anyof '+ '- '* '/)

;; A Binary Expression Tree (BET) is one of:
;; * Nat
;; * (list Op BET BET)


;; a)


;; I.

;; (swap i j lst) produces lst with the elements at positions i and j swapped.
;; swap: Nat Nat (listof X) -> (listof X)
;; requires: i, j < (length lst).
;; Examples:
(check-expect (swap 0 3 '(0 1 2 3 4 5)) '(3 1 2 0 4 5))
(check-expect (swap 3 0 '(0 1 2 3 4 5)) '(3 1 2 0 4 5))

(define (swap i j lst)
  (build-list (length lst)
              (lambda (index)
                (cond [(= index i) (list-ref lst j)]
                      [(= index j) (list-ref lst i)]
                      [else (list-ref lst index)]))))

;; Tests:
(check-expect (swap 0 0 '(a b)) '(a b))
(check-expect (swap 0 1 '(a b)) '(b a))
(check-expect (swap 1 0 '(a b)) '(b a))


;; II.

;; (generate-permutations lst) produces all possible permutations of lst.
;; generate-permutations: (listof X) -> (listof (listof X))
;; Examples:
(check-expect
 (generate-permutations '(1))
 '((1)))
(check-expect
 (generate-permutations '(1 2))
 '((2 1) (1 2)))

(define (generate-permutations set)
  (cond [(empty? set) (list set)]
        [else
         (local [(define (insert-all element front back)
                   (cond [(empty? back)
                          (list (append front (list element)))]
                         [else
                          (cons (append front (list element) back)
                                (insert-all element
                                            (cons (first back) front)
                                            (rest back)))]))
                 
                 (define (permute-list element listofset)
                   (foldl (lambda (set base)
                            (append (insert-all element '() set) base))
                          empty
                          listofset))
                 ]
           (foldl permute-list (list (list (first set))) (rest set)))]))

;; Tests:
(check-expect
 (generate-permutations '()) '(()))
(check-expect
 (generate-permutations '(1 2 3))
 '((3 1 2) (1 3 2) (2 1 3) (3 2 1) (2 3 1) (1 2 3)))
(check-expect
 (generate-permutations '(3 3 3))
 '((3 3 3) (3 3 3) (3 3 3) (3 3 3) (3 3 3) (3 3 3)))
(check-expect
 (length (generate-permutations '(1 2 3 4 5))) 120)


;; III.

;; (generate-tuples lst n) produces all tuples of length n of elements in lst.
;; generate-tuples: (listof X) Nat -> (listof (listof X))
;; requires: lst cannot be empty when n = 0, and vice versa.
;; Examples:
(check-expect
 (generate-tuples '(+ -) 3)
 '((+ + +) (+ + -) (+ - +) (+ - -)
           (- + +) (- + -) (- - +) (- - -)))
(check-expect
 (generate-tuples '(+) 1)
 '((+)))

(define (generate-tuples lst n)
  (cond [(zero? n) (list empty)]
        [else
         (local [(define (branch element prev)
                   (cond [(empty? prev) empty]
                         [else
                          (append (list (cons element (first prev)))
                                  (branch element (rest prev)))]))
                 
                 (define (layout set prev)
                   (foldr (lambda (element base)
                            (append (branch element prev) base))
                          empty
                          set))
                 
                 (define (collapse n)
                   (cond [(= n 1) (build-list (length lst)
                                              (lambda (x)
                                                (list (list-ref lst x))))]
                         [else
                          (layout lst (collapse (sub1 n)))]))
                 ]
           (collapse n))]))

;; Tests:
(check-expect
 (generate-tuples '(+ - * /) 0)
 (list empty))
(check-expect
 (generate-tuples '() 0)
 (list empty))
(check-expect
 (generate-tuples '() 10)
 empty)
(check-expect
 (generate-tuples '(+ -) 1)
 '((+) (-)))
(check-expect
 (generate-tuples '(+ +) 1)
 '((+) (+)))
(check-expect
 (generate-tuples '(+ - * /) 2)
 '((+ +) (+ -) (+ *) (+ /)
         (- +) (- -) (- *) (- /)
         (* +) (* -) (* *) (* /)
         (/ +) (/ -) (/ *) (/ /)))
(check-expect
 (generate-tuples '(+ + + +) 2)
 '((+ +) (+ +) (+ +) (+ +)
         (+ +) (+ +) (+ +) (+ +)
         (+ +) (+ +) (+ +) (+ +)
         (+ +) (+ +) (+ +) (+ +)))
(check-expect
 (length (generate-tuples '(+ - * /) 4)) 256)


;; IV.

;; (create-bets nlon nloop) produces a list of all possible BET
;;   based off of nlon and nloop.
;; create-bets: (listof (listof Num)) (listof (listof Op)) -> (listof BET)
;; requires: the length of each (listof Num) in nlon is one greater than the
;;           length of each (listof Op) in nloop.
;;           the length of each (listof Num) in nlon is at least 1.
;; Examples:
(check-expect
 (create-bets
  '((8 6 4 2))
  '((/ + -)))
 '((/ (+ (- 8 6) 4) 2)
   (/ (+ 8 (- 6 4)) 2) 
   (/ (+ 8 6) (- 4 2))
   (/ 8 (+ (- 6 4) 2))
   (/ 8 (+ 6 (- 4 2)))))
(check-expect
 (create-bets
  '((1)) '(()))
 '(1))
     
(define (create-bets nlon nloop)
  (cond [(empty? (first nloop))
         (foldr (lambda (e b)
                  (append e b))
                empty nlon)]
        [else
         (local [(define (create-tree-structure node-cnt)
                   (local [(define (flatten lst)
                             (foldr append empty lst))
                    
                           (define (cross c left-lst right-lst)
                             (cond [(empty? left-lst)
                                    (cross c (list left-lst) right-lst)]
                                   [(empty? right-lst)
                                    (cross c left-lst (list right-lst))]
                                   [else
                                    (foldr (lambda (L result)
                                             (append (map (lambda (R)
                                                            (list c L R))
                                                          right-lst)
                                                     result))
                                           empty
                                           left-lst)]))
                    
                           (define (create-left-right-pairs node-cnt)
                             (local [(define (generate-pairs node-cont
                                                             l-min
                                                             r-max
                                                             acc)
                                       (cond [(zero? node-cont) acc]
                                             [else
                                              (generate-pairs (sub1 node-cont)
                                                              (add1 l-min)
                                                              (sub1 r-max)
                                                              (cons
                                                               (list l-min
                                                                     r-max)
                                                               acc))]))
                                     ]
                               (generate-pairs node-cnt 0 (sub1 node-cnt) '())))
                           ]
                     (flatten (map (lambda (pair)
                                     (local [(define left
                                               (create-tree-structure
                                                (first pair)))
                                      
                                             (define right
                                               (create-tree-structure
                                                (second pair)))
                                             ]
                                       (cross node-cnt left right)))
                                   (create-left-right-pairs node-cnt)))))
          
                 (define (filler lon loop eb)
                   (local [(define (get-chunks index-range lst acc)
                             (cond [(zero? index-range)
                                    (list (reverse acc) lst)]
                                   [else
                                    (get-chunks (sub1 index-range)
                                                (rest lst)
                                                (cons (first lst) acc))]))
          
                           (define (side lon loop eb)
                             (cond [(empty? eb) (first lon)]
                                   [else (filler lon loop eb)]))
                           ]
                     (cond [(and (empty? (second eb)) (empty? (third eb)))
                            (cons (first loop)
                                  (list
                                   (side (list (first lon))
                                         (rest loop) (second eb))
                                   (side (list (second lon))
                                         (rest loop) (third eb))))]
                           [(empty? (second eb))
                            (cons (first loop)
                                  (list
                                   (side (list (first lon)) '() (second eb))
                                   (side (rest lon) (rest loop) (third eb))))]
                           [(empty? (third eb))
                            (cons (first loop)
                                  (list
                                   (side (first (get-chunks
                                                 (add1 (first (second eb)))
                                                 lon
                                                 '()))
                                         (rest loop)
                                         (second eb))
                                   (side (list (first (reverse lon)))
                                         '() (third eb))))]
                           [else
                            (cons (first loop)
                                  (list
                                   (side (first (get-chunks
                                                 (add1 (first (second eb)))
                                                 lon
                                                 '()))
                                         (first (get-chunks (first (second eb))
                                                            (rest loop)
                                                            '()))
                                         (second eb))
                                   (side (second (get-chunks
                                                  (add1 (first (second eb)))
                                                  lon
                                                  '()))
                                         (second (get-chunks
                                                  (first (second eb))
                                                  (rest loop)
                                                  '()))
                                         (third eb))))])))
          
                 (define (fill-all lon loop loeb)
                   (foldr (lambda (eb base)
                            (cons (filler lon loop eb) base))
                          empty
                          loeb))
                 (define (cycle-operatiors lon nloop loeb)
                   (foldl (lambda (loop base)
                            (append (fill-all lon loop loeb) base))
                          empty
                          nloop))
                 (define (cycle-numbers nlon nloop loeb)
                   (foldl (lambda (lon base)
                            (append (cycle-operatiors lon nloop loeb) base))
                          empty
                          nlon))
                 ]
           (cycle-numbers
            nlon nloop (create-tree-structure (length (first nloop)))))]))

;; Tests:
(check-expect
 (create-bets '((1) (2) (3)) '(()))
 '(1 2 3))
(check-expect
 (create-bets '((1 2) (2 1)) '((+) (-)))
 '((- 2 1)
   (+ 2 1)
   (- 1 2)
   (+ 1 2)))
(check-expect
 (length (create-bets (generate-permutations '(1 2 3 4 5))
                      (generate-tuples '(+ - * /) 4)))
 430080)


;; V.

;; (evaluate-bets lobet target) produces a list of all BET from
;;   lobet that evaluate to the target value
;; evaluate-bets: (listof BET) Nat -> (listof BET)
;; Examples:
(check-expect
 (evaluate-bets
  (create-bets
   (generate-permutations '(2 4 8))
   (generate-tuples '(+ - *) 2))
  2)
 '((- 8 (+ 2 4))
   (- (- 8 2) 4)
   (- 8 (+ 4 2))
   (- (- 8 4) 2)))
(check-expect
 (evaluate-bets
  '((+ 9 10)) 21)
 '())

(define (evaluate-bets lobet target)
  (local [(define (evaluate-bet bet)
            (local [(define valid? (lambda (num)
                                     (and (> num 0)
                                          (integer? num))))
                    
                    (define (apply-op f bi-ex)
                      (local [(define base
                                (and (valid? (evaluate-bet
                                              (first bi-ex)))
                                     (valid? (evaluate-bet
                                              (second bi-ex)))))
                              ]
                        (cond [(symbol=? f '+)
                               (local [(define test
                                         (+ (evaluate-bet (first bi-ex))
                                            (evaluate-bet (second bi-ex))))]
                                 (cond [(and (valid? test) base) test]
                                       [else 0]))]
                              [(symbol=? f '-)
                               (local [(define test
                                         (- (evaluate-bet (first bi-ex))
                                            (evaluate-bet (second bi-ex))))]
                                 (cond [(and (valid? test) base) test]
                                       [else 0]))]
                              [(symbol=? f '*)
                               (local [(define test
                                         (* (evaluate-bet (first bi-ex))
                                            (evaluate-bet (second bi-ex))))]
                                 (cond [(and (valid? test) base) test]
                                       [else 0]))]
                              [(and (symbol=? f '/)
                                    (not (zero? (evaluate-bet (second bi-ex)))))
                               (local [(define test
                                         (/ (evaluate-bet (first bi-ex))
                                            (evaluate-bet (second bi-ex))))]
                                 (cond [(and (valid? test) base) test]
                                       [else 0]))]
                              [else 0])))
                    ]    
              (cond [(number? bet) bet]
                    [else (apply-op (first bet) (rest bet))])))
          
          (define (evaluate-bets/acc lobet acc)
            (cond [(empty? lobet) acc]
                  [(= target (evaluate-bet (first lobet)))
                   (evaluate-bets/acc (rest lobet) (cons (first lobet) acc))]
                  [else
                   (evaluate-bets/acc (rest lobet) acc)]))
          ]
    (evaluate-bets/acc lobet '())))

;; Tests:
(check-expect (evaluate-bets empty 14) '())
(check-expect
 (evaluate-bets '((/ 0 0)) 1)
 '())
(check-expect
 (evaluate-bets '((+ 1 1)) 2)
 '((+ 1 1)))
(check-expect
 (evaluate-bets '((+ 1 0)) 1)
 '())
(check-expect
 (evaluate-bets '((+ 2 -1)) 1)
 '())
(check-expect
 (evaluate-bets '((+ 0.5 1.5)) 2)
 '())
(check-expect
 (evaluate-bets '((- 2 1)) 1)
 '((- 2 1)))
(check-expect
 (evaluate-bets '((- 2 0)) 2)
 '())
(check-expect
 (evaluate-bets '((- 2 -1)) 3)
 '())
(check-expect
 (evaluate-bets '((- 1.5 0.5)) 1)
 '())
(check-expect
 (evaluate-bets '((* 2 1)) 2)
 '((* 2 1)))
(check-expect
 (evaluate-bets '((* 2 0)) 2)
 '())
(check-expect
 (evaluate-bets '((* 2 -1)) 2)
 '())
(check-expect
 (evaluate-bets '((* 2 0.5)) 1)
 '())
(check-expect
 (evaluate-bets '((/ 2 1)) 2)
 '((/ 2 1)))
(check-expect
 (evaluate-bets '((/ 2 0)) 1)
 '())
(check-expect
 (evaluate-bets '((/ 2 -1)) 2)
 '())
(check-expect
 (evaluate-bets '((/ 2 0.5)) 4)
 '())
(check-expect
 (evaluate-bets '((+ 1 (+ 1 1))) 3)
 '((+ 1 (+ 1 1))))
(check-expect
 (evaluate-bets '((+ 1 (+ -2 1))) 3)
 '())
(check-expect
 (evaluate-bets '((+ 1 (- 1 1 ))) 3)
 '())
(check-expect
 (evaluate-bets '((+ 1 (/ 1 2))) 3)
 '())
(check-expect
 (evaluate-bets '((/ 1 (- 1 1))) 3)
 '())
(check-expect
 (length (evaluate-bets (create-bets (generate-permutations '(1 5 7 10 25))
                                     (generate-tuples '(+ - * /) 4)) 175))
 282)

;; VI.

;; (countdown-numbers lon target) produces a BET using the numbers in lon
;;   that evaluates to target, or false if no such BET exists.
;; countdown-numbers: (listof Nat) Nat -> (anyof BET false)
;; Examples:
(check-expect
 (countdown-numbers '(1 1) 1)
 '(* 1 1))
(check-expect
 (countdown-numbers '(1 1) 3)
 false)

(define (countdown-numbers lon target)
  (cond [(empty? lon) false]
        [else 
         (local [(define (final-answer lobet)
                   (cond [(empty? lobet) false]
                         [else (first lobet)]))
                 ]
           (final-answer (evaluate-bets
                          (create-bets
                           (generate-permutations lon)
                           (generate-tuples
                            '(+ - * /) (sub1 (length lon))))
                          target)))]))

;; Tests:
(check-expect (countdown-numbers '() 100)
              false)
(check-expect (countdown-numbers '(1 1 1 1 1) 1)
              '(- (+ 1 (+ 1 1)) (+ 1 1)))
(check-expect (countdown-numbers '(1 2 3) 8)
              '(* (+ 3 1) 2))
(check-expect (countdown-numbers '(1 2 3 4 5) 180)
              '(* 5 (* (+ 2 1) (* 3 4))))
(check-expect (countdown-numbers '(1 14 20 9 100) 1429)
              '(+ (+ (* 100 14) (* 1 20)) 9))
(check-expect (countdown-numbers '(1 3 4 6) 24) false)