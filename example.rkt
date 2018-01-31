#lang racket

(require rackunit)
(require srfi/1)

;; memory limit
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 256))
(custodian-limit-memory (current-custodian) MAX-BYTES)

(define (min a b)
  (if (< a b) a b))
(define (max a b)
  (if (> a b) a b))
(define (round-to-precision num digits)
  (let ([factor (expt 10 digits)])
    (/ (round (* factor num)) factor)))
;;;;;;;;;;;;;;;;;;;;;;;
;; GENETIC ALGORITHM ;;
;;;;;;;;;;;;;;;;;;;;;;;
(struct Organism
  (dna)
  #:transparent)

(define (calculate-fitness org comparator)
  (define (iter remaining-target remaining-org-dna result)
    (cond [(or (null? remaining-target) (null? remaining-org-dna))
           result]
          [(comparator (car remaining-target) (car remaining-org-dna))
           (iter (cdr remaining-target)
                 (cdr remaining-org-dna)
                 (add1 result))]
          [else (iter (cdr remaining-target)
                      (cdr remaining-org-dna)
                      result)]))

  (let ([res (iter TARGET (Organism-dna org) 0)])
    (/ res (length (Organism-dna org)))))

(define (get-random-character)
  (let loop ([random-char (integer->char (+ 32 (random 95)))])
    (if (regexp-match ALPHABET-REGEX (list->string (list random-char)))
        (begin #;(printf "valid char: ~s~n" random-char)
               random-char)
        (begin #;(printf "illegal char: ~s~n" random-char)
               (loop (integer->char (+ 32 (random 95))))))))

(define (create-organism)
  (let ([dna (for/list ([elem TARGET])
               (get-random-character))])
    (Organism dna)))

(define (create-initial-population)
  (for/list ([counter POPULATION-SIZE])
    (create-organism)))

;; evolving the pool includes 2 actions:
;; - deciding which organisms to keep of the current generation
;; - creating an amount or organisms from the chosen ones to make a new generation
(define (evolve-pool population mutation-probability generation#)
  ;; IDEA:
  ;; percentage of some fitness of the the max-fitness times the population size
  (let* ([fitness-sorted-orgs
         (sort population (λ (org1 org2)
                            (> (calculate-fitness org1 char=?)
                               (calculate-fitness org2 char=?))))]
         [survival-count (inexact->exact (floor (* (length population) SURVIVAL-RATIO)))]
         [survivors (take fitness-sorted-orgs survival-count)]
         [mated-survivors (natural-selection survivors)])

    ;; create new pool
    (for/list ([current-population-size (in-range POPULATION-SIZE)]
               [org (in-cycle mated-survivors)])
      (let* ([fitness (calculate-fitness org char=?)]
             [mutation-threshold
              (max (* (- 1 (sqrt fitness)) mutation-probability)
                   MINIMUM-MUTATION-PROBABILITY)])
        #;(when (= current-population-size (sub1 POPULATION-SIZE))
          (printf "mutation-threshold: ~a~n" (exact->inexact mutation-threshold)))
        (mutate-organism org mutation-threshold)))))

#| > Randomly pick 2 organisms from the breeding pool and
use them as the parents to create the next generation of
organism for the population. |#
(define (natural-selection survivors)

  (define (mate-two survivors)
    (let ([the-two (take survivors 2)])
      #;(printf "~s~n" (length the-two))
      (crossover (car the-two) (cadr the-two))))

  (define (mate-survivors survivors result)
    (cond [(null? survivors)
           #;(printf "survivors empty: ~a~n" survivors)
           result]
          [(null? (cdr survivors))
           #;(printf "special case: ~a~n" survivors)
           (cons (car survivors) result)]
          [else
           #;(printf "remaining survivors: ~s~n" (length survivors))
           (let ([offspring (mate-two survivors)])
             (mate-survivors (cddr survivors) #;(cons (car survivors)
                                                      (cons (cadr survivors)
                                                            (cons offspring result)))

                             (cons offspring result)))]))

  (mate-survivors (shuffle survivors) '())
  ;; TODO: remove later?
  (list (first survivors) (second survivors)))

#|crossover does not calculate the fitness for the organism yet,
because a crossed over organism needs to be mutated first.|#
(define (crossover org1 org2)
  (let* ([org1-dna (Organism-dna org1)]
         [org2-dna (Organism-dna org2)]
         [mid-point (add1 (random (length org1-dna)))]
         [new-dna (append (take org1-dna mid-point)
                          (drop org2-dna mid-point))])
    (Organism new-dna)))

#|If a random number is below the mutation threshold,
no mutation is done otherwise mutation is done.|#
(define (mutate-organism org mutation-threshold)
  (let ([org-dna (Organism-dna org)])
    (struct-copy Organism org
                 [dna (for/list ([dna-segment org-dna])
                        (if (< (random) mutation-threshold)
                            (get-random-character)
                            dna-segment))])))

;; retrieves the fittest organism from a list of organisms
(define (get-fittest-organism population)
  (car (sort population
             (λ (org1 org2)
               (> (calculate-fitness org1 char=?)
                  (calculate-fitness org2 char=?))))))

(random-seed 0)
(define TARGET
  (string->list
   #;(string-append
    "I must not fear. "
    "Fear is the mind-killer. "
    "Fear is the little-death that brings total obliteration. "
    "I will face my fear. "
    "I will permit it to pass over me and through me. "
    "And when it has gone past I will turn the inner eye to see its path. "
    "Where the fear has gone there will be nothing. "
    "Only I will remain.")
   "To be or not to be."))
(define ALPHABET-REGEX #rx"[a-zA-Z0-9 .,;–!?-]")
(define MAX-GENERATIONS 4000) ; 1500
(define POPULATION-SIZE 500) ; 300
(define SURVIVAL-RATIO 1/20) ; 1/5
(define MAX-TIME 90) ; 90
(define INITIAL-MUTATION-PROBABILITY (/ 1 20)) ; 1/36
(define MINIMUM-MUTATION-PROBABILITY (/ 1 (length TARGET))) ; (/ 1 (length TARGET))

#|
BEST RESULT SO FAR:
gen-max: 1000
pop-size: 400
survi: 1/10
max-time: 90
INITIAL-MUTATION-PROBABILITY: 1/20
MINIMUM-MUTATION-PROBABILITY: (/ 2.5 (length TARGET))
|#

(define (main)
  (define start-time (current-inexact-milliseconds))
  (printf "Starting time: ~s~n" start-time)
  (define INITIAL-POPULATION (create-initial-population))

  ;; loop until found
  (let loop ([generation# 0]
             [population INITIAL-POPULATION])
    (let* ([fittest-org (get-fittest-organism population)]
           [fittest-org-fitness (calculate-fitness fittest-org char=?)])
      #;(printf "fittest org: ~s~n"
              (list->string (take (Organism-dna fittest-org) (min 50 (length TARGET)))))
      #;(printf "time passed: ~ss~n" (/ (- (current-inexact-milliseconds) start-time) 1000))
      (cond [(or (>= (/ (- (current-inexact-milliseconds) start-time) 1000) MAX-TIME)
                 (>= generation# MAX-GENERATIONS))
             (printf "Generation: ~s, Fitness: ~s~n"
                     generation#
                     (round-to-precision (exact->inexact fittest-org-fitness) 3))]
            [(equal? (Organism-dna fittest-org) TARGET)
             (printf "Generation: ~s, Fitness: ~s~n"
                     generation#
                     (round-to-precision (exact->inexact fittest-org-fitness) 3))]
            [else
             (printf "Generation: ~s, Fitness: ~s~n"
                     generation#
                     (round-to-precision (exact->inexact fittest-org-fitness) 3))
             (loop (add1 generation#)
                   (evolve-pool population
                                INITIAL-MUTATION-PROBABILITY
                                generation#))])))

  (define end-time (current-inexact-milliseconds))
  (printf "Ending time: ~s~n" end-time)
  (printf "Time spent: ~ss~n" (/ (- end-time start-time) 1000)))

(time (for ([i (in-range 50)]) (main)))
