#lang racket

(require rackunit)

;; memory limit
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

(define (min a b)
  (if (< a b) a b))
(define (max a b)
  (if (> a b) a b))

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
         [survivors (take fitness-sorted-orgs survival-count)])
    (for/list ([current-population-size (in-range POPULATION-SIZE)]
               [org (in-cycle survivors)])
      (let* ([fitness (calculate-fitness org char=?)]
             [mutation-threshold (max (* (- 1 (sqrt fitness))
                                         mutation-probability)
                                      MINIMUM-MUTATION-PROBABILITY)])
        (when (= current-population-size (sub1 POPULATION-SIZE))
          (printf "mutation-threshold: ~a~n" (exact->inexact mutation-threshold)))
        (mutate-organism org mutation-threshold)))))

#| > Randomly pick 2 organisms from the breeding pool and use them as the parents to create the next generation of organism for the population. |#
#;(define (natural-selection pool population target)
  ;; create a list of organisms as the next population
  (for/list ([population-member population])
    ;; The probability for two times the same organism is usually low.
    ;; This is because choosing randomly 2 of a big set is unlikely to choose the same twice.
    (let ([org-1 (list-ref pool (random (length pool)))]
          [org-2 (list-ref pool (random (length pool)))])
      ;; All new population members are children of the 2 randomly chosen parents
      ;; They are all mutated crossovers.
      (mutate-organism (crossover org-1 org-2)))))

#|crossover does not calculate the fitness for the organism yet,
because a crossed over organism needs to be mutated first.|#
#;(define (crossover org1 org2)
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
   (string-append
    "I must not fear. "
    "Fear is the mind-killer. "
    "Fear is the little-death that brings total obliteration. "
    "I will face my fear. "
    "I will permit it to pass over me and through me. "
    "And when it has gone past I will turn the inner eye to see its path. "
    "Where the fear has gone there will be nothing. "
    "Only I will remain.")))
(define ALPHABET-REGEX #rx"[a-zA-Z0-9 .,;–!?-]")
(define MAX-GENERATIONS 1500) ; 1200
(define POPULATION-SIZE 300) ; 250
(define SURVIVAL-RATIO 1/5) ; 1/5
(define MAX-TIME 90)
(define INITIAL-MUTATION-PROBABILITY 1/36) ; 1/40
(define MINIMUM-MUTATION-PROBABILITY (/ 1 (length TARGET))) ; 1/40

(define (main)
  (define start-time (current-inexact-milliseconds))
  (printf "Starting time: ~s~n" start-time)
  (define INITIAL-POPULATION (create-initial-population))

  ;; loop until found
  (let loop ([generation# 0]
             [population INITIAL-POPULATION])
    (let* ([fittest-org (get-fittest-organism population)]
           [fittest-org-fitness (calculate-fitness fittest-org char=?)])
      (printf "fittest org: ~s~n" (list->string (take (Organism-dna fittest-org) 50)))
      (printf "time passed: ~ss~n" (/ (- (current-inexact-milliseconds) start-time) 1000))
      (cond [(or (>= (/ (- (current-inexact-milliseconds) start-time) 1000) MAX-TIME)
                 (>= generation# MAX-GENERATIONS))
             (printf "Generation: ~s, Fitness: ~s~n"
                     generation#
                     (exact->inexact fittest-org-fitness))]
            [(equal? (Organism-dna fittest-org) TARGET)
             (printf "Generation: ~s, Fitness: ~s~n"
                     generation#
                     (exact->inexact fittest-org-fitness))]
            [else
             (printf "Generation: ~s, Fitness: ~s~n"
                     generation#
                     (exact->inexact fittest-org-fitness))
             (loop (add1 generation#)
                   (evolve-pool population
                                INITIAL-MUTATION-PROBABILITY
                                generation#))])))

  (define end-time (current-inexact-milliseconds))
  (printf "Ending time: ~s~n" end-time)
  (printf "Time spent: ~ss~n" (/ (- end-time start-time) 1000)))
