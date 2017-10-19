#lang racket

(require "pipegame.rkt")

(provide (struct-out sized-set)
         (rename-out [stat-set-average-time average-time])
         stat-set
         total-plays
         plays
         total-win-percent
         win-percent
         total-perfect-percent
         perfect-percent
         add-to-stats
         stats-add
         load-stats
         save-stats)

#|
This file provides processing of game statistics for the pipegame.
|#

;; =============================================================================
;;; DATA DEFINITIONS
;; =============================================================================


;; A Statistics is a [Listof SizedStatSet]
;; Represents a collection of SizedStatSets
;; Must contain at most one SizedStatSet for each board size

(struct sized-set (size stats) #:transparent)
;; A SizedStatSet is a (sized-set Number StatSet) where:
;; -- size represents the board size for which these statistics hold
;; -- stats is the set of statistics applicable to this board size


(struct stat-set (plays wins perfects average-time) #:transparent)
;; A StatSet is a (stat-set Number Number Number [Maybe Time]) where:
;; -- plays is the number of times the user has started a game
;; -- wins is the number of times the user has won a game
;; -- perfects is the number of times the user has completed a game with the
;;    perfect number or fewer moves
;; -- average-time is the average time taken to successfully complete a board,
;;    or #f if no plays are recorded yet

;; A Percent is an Integer [0-100]


;; =============================================================================
;; CONSTANTS
;; =============================================================================

(define STATS-FILE "statistics.txt")


;; =============================================================================
;;; PROCESSING BOARDS
;; =============================================================================

;; add-to-stats : Statistics Board Time -> Statistics
;; Factors the given board into the given set of statistics
(define (add-to-stats stats board time)
  (cond [(empty? stats) (list (sized-set (board-size board)
                                           (new-stat-set board time)))]
        [else
         (if (= (sized-set-size (first stats)) (board-size board))
             (cons (sized-set (board-size board)
                              (add-to-stat-set
                               (sized-set-stats (first stats)) board time))
                   (rest stats))
             (cons (first stats) (add-to-stats (rest stats) board time)))]))

;; add-to-stat-set : StatSet Board Time -> StatSet
;; Factors the given board and time into the given statset.
(define (add-to-stat-set old-stat-set board time)
  (define plays 1)
  (define wins (if (all-connected? board) 1 0))
  (define perfects (if (and (all-connected? board)
                            (<= (board-turns board) (board-perfect board)))
                       1
                       0))
  (define average-time (and (= wins 1) time))
  
  (add-stat-set old-stat-set (stat-set plays wins perfects average-time)))

;; new-stat-set : Board Time -> StatSet
;; Creates a new statset based on the given board
(define (new-stat-set board time)
  (add-to-stat-set (stat-set 0 0 0 #f) board time))


;; =============================================================================
;;; STATISTICS FUNCTIONS
;; =============================================================================

;; total-plays : Statistics -> Number
;; Determines the total number of plays across all board sizes
(define (total-plays stats)
  (foldr (λ (sized-set acc) (+ acc (sized-plays sized-set)))
         0
         stats))

;; sized-plays : SizedSet -> Number
;; Determines the number of plays in the given sized set
(define (sized-plays set)
  (plays (sized-set-stats set)))

;; plays : StatSet -> Number
;; Determines the number of plays in the given stat set
(define (plays set)
  (stat-set-plays set))

;; total-win-percent : Statistics -> Percent
;; Determines the total win percentage of all board sizes
(define (total-win-percent stats)
  (cond [(empty? stats) 100]
        [else (floor (weighted-avg (sized-win-percent (first stats))
                                   (sized-plays (first stats))
                                   (total-win-percent (rest stats))
                                   (total-plays (rest stats))))]))

;; sized-win-percent : SizedSet -> Percent
;; Determines the win percentage for the given sized set
(define (sized-win-percent set)
  (win-percent (sized-set-stats set)))

;; win-percent : StatSet -> Percent
;; Determines the win percentage of the given stat set
(define (win-percent set)
  (quotient (* 100 (stat-set-wins set)) (plays set)))

;; total-perfect-percent : Statistics -> Percent
;; Determines the percentage of plays that were perfect
(define (total-perfect-percent stats)
  (cond [(empty? stats) 100]
        [else (floor (weighted-avg (sized-perfect-percent (first stats))
                                   (sized-plays (first stats))
                                   (total-perfect-percent (rest stats))
                                   (total-plays (rest stats))))]))

;; sized-perfect-percent : SizedSet -> Percent
;; Determines the percentage of plays that were perfect for the given sized set
(define (sized-perfect-percent set)
  (perfect-percent (sized-set-stats set)))

;; perfect-percent : StatSet -> Percent
;; Determines the percentage of plays that were perfect for this stat set
(define (perfect-percent set)
  (quotient (* 100 (stat-set-perfects set)) (plays set)))


;; stats-add : Statistics Statistics -> Statistics
;; Adds the two sets of statistics
(define (stats-add stats1 stats2)
  
  ;; add-sized-set : SizedSet Statistics -> Statistics
  ;; Adds the given sized stat set to the given stats
  (define (add-sized-set set stats)
    (cond [(empty? stats) (list set)]
          [(not (= (sized-set-size set) (sized-set-size (first stats))))
           (cons (first stats) (add-sized-set set (rest stats)))]
          [else
           (define set1 (sized-set-stats set))
           (define set2 (sized-set-stats (first stats)))
           (cons (sized-set (sized-set-size set)
                            (add-stat-set set1 set2))
                 (rest stats))]))
    
  (cond [(empty? stats2) stats1]
        [else (stats-add (add-sized-set (first stats2) stats1) (rest stats2))]))

;; add-stat-set : StatSet StatSet -> StatSet
;; Adds the given two stat sets together
(define (add-stat-set set1 set2)
  (define plays (+ (stat-set-plays set1) (stat-set-plays set2)))
  (define wins (+ (stat-set-wins set1) (stat-set-wins set2)))
  (define perfs (+ (stat-set-perfects set1) (stat-set-perfects set2)))
  (define time (weighted-avg (stat-set-average-time set1)
                             (stat-set-wins set1)
                             (stat-set-average-time set2)
                             (stat-set-wins set2)))
  (stat-set plays wins perfs time))


;; weighted-avg : [Maybe Number] Number [Maybe Number] Number -> [Maybe Number]
;; Determines the weighted average of the two pairs of averages and weights.
;; If one average is #f, will return the value of the other.
(define (weighted-avg avg1 weight1 avg2 weight2)
  (cond [(not (and avg1 avg2)) (or avg1 avg2)]
        [else (/ (+ (* weight1 avg1) (* weight2 avg2)) (+ weight1 weight2))]))


 
;; =============================================================================
;;; SAVE/LOAD
;; =============================================================================

;; load-stats : -> Statistics
;; Reads saved statistics from the stats file if it exists (otherwise returns
;; empty Statistics)
(define (load-stats)
  (if (file-exists? STATS-FILE)
      (load-stats-from-file STATS-FILE)
      '()))

;; save-stats : Statistics -> Void
;; Adds the given statistics to the statistics found in the stats file and
;; saves to file (creates it if it does not exist)
(define (save-stats stats)
  (save-stats-to-file (stats-add (load-stats) stats) STATS-FILE))

;; load-stats-from-file : Path -> Statistics
;; Load statistics from the given file. Assumes file exists. 
(define (load-stats-from-file file)
  (unmarshal-lines (file->lines file)))

;; unmarshal-lines : [Listof String] -> Statistics
;; Unmarshal the given lines from the statistics file into a Statistics
(define (unmarshal-lines lines)
  (cond [(empty? lines) '()]
        [else (cons (unmarshal-line (first lines))
                    (unmarshal-lines (rest lines)))]))

;; unmarshal-line : String -> SizedStatSet
;; Unmarshals the given line of the statistics file into a sized stat set
(define (unmarshal-line line)
  (define split (regexp-split #rx"[:,]" line))
  (define size (string->number (first split)))
  (define data (rest split))
  (sized-set size (unmarshal-stats data)))

;; unmarshal-stats : [Listof String] -> StatSet
;; Unmarshals the given stats into a StatSet
;; Examples: (unmarshal-stats '("3" "2" "1" "20")) -> (stat-set 3 2 1 20)
;;           (unmarshal-stats '("4" "2" "1" "#f")) -> (stat-set 4 2 1 #f)
(define (unmarshal-stats stats)
  (stat-set (string->number (first stats))
            (string->number (second stats))
            (string->number (third stats))
            (string->number (fourth stats))))


;; save-stats-to-file : Statistics Path -> Void
;; Saves the given statistics to the given file in ascending size order
(define (save-stats-to-file stats file)
  (display-to-file (marshal-stats
                    (sort stats
                          <
                          #:key (λ (set) (sized-set-size set))))
                   file
                   #:mode 'text
                   #:exists 'replace))
                
;; marshal-stats : Statistics -> String
;; Marshals the given statistics into the stat save format
(define (marshal-stats stats)
  (cond [(empty? stats) ""]
        [else (string-append (marshal-sized-set (first stats))
                             "\n"
                             (marshal-stats (rest stats)))]))

;; marshal-sized-set : SizedStatSet -> String
;; Marshals the given sized stat set into the one line save format
(define (marshal-sized-set set)
  (string-append (number->string (sized-set-size set))
                 ":"
                 (marshal-stat-set (sized-set-stats set))))

;; marshal-stat-set : StatSet -> String
;; Marshals the given stat set into a comma-separated string
(define (marshal-stat-set stats)
  (string-append (number->string (stat-set-plays stats)) ","
                 (number->string (stat-set-wins stats)) ","
                 (number->string (stat-set-perfects stats)) ","
                 (marshal-average-time (stat-set-average-time stats))))

;; marshal-average-time : [Maybe Time] -> String
;; Converts the given maybe time to a string, where false is "#f"
(define (marshal-average-time time)
  (or (and (not (not time)) (number->string time)) "#f"))
  



;; =============================================================================
;;; TESTS
;; =============================================================================

(module+ test 
  (require rackunit rackunit/text-ui)

  ;; Testing Constants
  (define 2x2-stat-set (stat-set 5 3 1 #f))
  (define 3x3-stat-set (stat-set 7 4 1 #f))
  
  (define no-stats '())
  (define 3-only (list (sized-set 3 3x3-stat-set)))
  (define 2-and-3 (list (sized-set 2 2x2-stat-set)
                        (sized-set 3 3x3-stat-set)))

  (define 2x2-lost (board (list (power-source (posn 0 0) 'end 'west #t)
                                (cell (posn 1 0) 'elbow 'south #f)
                                (cell (posn 0 1) 'end 'east #f)
                                (cell (posn 1 1) 'elbow 'west #f))
                          2 (posn 1 1) 3 1))
  (define 2x2-gave-up (board (list (power-source (posn 0 0) 'end 'west #t)
                                   (cell (posn 1 0) 'elbow 'south #f)
                                   (cell (posn 0 1) 'end 'east #f)
                                   (cell (posn 1 1) 'elbow 'west #f))
                             2 (posn 1 1) 1 3))
  (define 2x2-won (board (list (power-source (posn 0 0) 'end 'east #t)
                               (cell (posn 1 0) 'elbow 'south #t)
                               (cell (posn 0 1) 'end 'east #t)
                               (cell (posn 1 1) 'elbow 'west #t))
                         2 (posn 1 1) 3 1))
  (define 2x2-perfect (board (list (power-source (posn 0 0) 'end 'east #t)
                                   (cell (posn 1 0) 'elbow 'south #t)
                                   (cell (posn 0 1) 'end 'east #t)
                                   (cell (posn 1 1) 'elbow 'west #t))
                             2 (posn 1 1) 1 1))
  (define 2x2-super-perfect (board (list (power-source (posn 0 0) 'end 'east #t)
                                         (cell (posn 1 0) 'elbow 'south #t)
                                         (cell (posn 0 1) 'end 'east #t)
                                         (cell (posn 1 1) 'elbow 'west #t))
                                   2 (posn 1 1) 1 2))
  (define 3x3-won (board (list (cell (posn 0 0) 'elbow 'east #t)
                                      (cell (posn 1 0) 'bar 'east #t)
                                      (cell (posn 2 0) 'elbow 'south #t)
                                      (cell (posn 0 1) 'tee 'east #t)
                                      (power-source (posn 1 1) 'elbow 'south #t)
                                      (cell (posn 2 1) 'end 'north #t)
                                      (cell (posn 0 2) 'end 'north #t)
                                      (cell (posn 1 2) 'elbow 'north #t)
                                      (cell (posn 2 2) 'end 'west #t))
                                3 (posn 1 1) 8 4))

  ;; Tests on processing boards

  ;; Adds lost board properly
  (check-equal? (add-to-stat-set 2x2-stat-set 2x2-lost #f)
                (stat-set 6 3 1 #f))
  ;; Won't increase perfects if board isn't won
  (check-equal? (add-to-stat-set 2x2-stat-set 2x2-gave-up #f)
                (stat-set 6 3 1 #f))
  ;; Adds won board properly
  (check-equal? (add-to-stat-set 2x2-stat-set 2x2-won #f)
                (stat-set 6 4 1 #f))
  ;; Adds perfect board properly
  (check-equal? (add-to-stat-set 2x2-stat-set 2x2-perfect #f)
                (stat-set 6 4 2 #f))
  ;; Adds super perfect board properly
  (check-equal? (add-to-stat-set 2x2-stat-set 2x2-super-perfect #f)
                (stat-set 6 4 2 #f))

  ;; Adds stats for correct size
  (check-equal? (add-to-stats 2-and-3 2x2-won #f)
                (list (sized-set 2 (stat-set 6 4 1 #f))
                      (sized-set 3 3x3-stat-set)))
  (check-equal? (add-to-stats 2-and-3 3x3-won #f)
                (list (sized-set 2 2x2-stat-set)
                      (sized-set 3 (stat-set 8 5 1 #f))))
  ;; Adds new stat set
  (check-equal? (add-to-stats 3-only 2x2-won #f)
                (list (sized-set 3 3x3-stat-set)
                      (sized-set 2 (stat-set 1 1 0 #f))))
  (check-equal? (add-to-stats no-stats 2x2-perfect #f)
                (list (sized-set 2 (stat-set 1 1 1 #f))))

  
  ;; Tests on statistics functions

  (check-equal? (total-plays '()) 0)
  (check-equal? (total-plays 3-only) 7)
  (check-equal? (total-plays 2-and-3) 12)

  (check-equal? (sized-plays (sized-set 3 3x3-stat-set)) 7)

  (check-equal? (plays 3x3-stat-set) 7)
  
  (check-equal? (total-win-percent '()) 100)
  (check-equal? (total-win-percent 3-only) 57)
  (check-equal? (total-win-percent 2-and-3) 58)
  
  (check-equal? (sized-win-percent (sized-set 3 (stat-set 3 0 0 #f))) 0)

  (check-equal? (win-percent (stat-set 3 0 0 #f)) 0) ; zero
  (check-equal? (win-percent (stat-set 3 3 3 #f)) 100) ; 100
  (check-equal? (win-percent (stat-set 4 2 1 #f)) 50) ; works for hole numbers
  (check-equal? (win-percent 3x3-stat-set) 57) ; rounds for 57.1
  (check-equal? (win-percent (stat-set 11 7 4 #f)) 63) ; 63.6 - truncates

  (check-equal? (total-perfect-percent '()) 100)
  (check-equal? (total-perfect-percent 3-only) 14)
  (check-equal? (total-perfect-percent 2-and-3) 16)

  (check-equal? (sized-perfect-percent (sized-set 3 3x3-stat-set)) 14)

  (check-equal? (perfect-percent (stat-set 3 1 0 #f)) 0)
  (check-equal? (perfect-percent (stat-set 3 3 3 #f)) 100)
  (check-equal? (perfect-percent (stat-set 3 2 1 #f)) 33)
  
  ;; Will add properly
  (check-equal? (stats-add (list (sized-set 2 2x2-stat-set))
                           (list (sized-set 2 (stat-set 1 1 1 #f))))
                (list (sized-set 2 (stat-set 6 4 2 #f))))
  ;; Will handle adding number times to #f
  (check-equal? (stats-add (list (sized-set 2 2x2-stat-set))
                           (list (sized-set 2 (stat-set 1 1 1 20))))
                (list (sized-set 2 (stat-set 6 4 2 20))))
  (check-equal? (stats-add (list (sized-set 2 (stat-set 1 1 1 20)))
                           (list (sized-set 2 2x2-stat-set)))
                (list (sized-set 2 (stat-set 6 4 2 20))))
  ;; Will handle factoring new averages
  (check-equal? (stats-add (list (sized-set 2 (stat-set 4 3 1 20)))
                           (list (sized-set 2 (stat-set 1 1 0 40))))
                (list (sized-set 2 (stat-set 5 4 1 25))))
  ;; Will combine properly with two different sizes
  (check-equal? (stats-add (list (sized-set 2 2x2-stat-set))
                           (list (sized-set 3 3x3-stat-set)))
                (list (sized-set 2 2x2-stat-set)
                      (sized-set 3 3x3-stat-set)))
  ;; Will add properly with multipe sizes in first
  (check-equal? (stats-add (list (sized-set 2 2x2-stat-set)
                                 (sized-set 3 3x3-stat-set))
                           (list (sized-set 2 (stat-set 1 1 1 #f))))
                (list (sized-set 2 (stat-set 6 4 2 #f))
                      (sized-set 3 3x3-stat-set)))
  ;; Will add properly with multiple sizes in both in different orders
  (check-equal? (stats-add (list (sized-set 2 2x2-stat-set)
                                 (sized-set 3 3x3-stat-set))
                           (list (sized-set 3 (stat-set 1 1 1 #f))
                                 (sized-set 2 (stat-set 1 1 1 #f))))
                (list (sized-set 2 (stat-set 6 4 2 #f))
                      (sized-set 3 (stat-set 8 5 2 #f))))

  (check-equal? (weighted-avg 2 1 2 1) 2)
  (check-equal? (weighted-avg 20 3 40 1) (/ (+ 20 20 20 40) 4))
  (check-equal? (weighted-avg #f 3 4 1) 4)
  (check-equal? (weighted-avg 5 3 #f 3) 5)
  (check-equal? (weighted-avg #f 5 #f 1) #f)


  ;; Tests on loading from file
  
  (check-equal? (unmarshal-stats '("3" "2" "1" "20"))
                (stat-set 3 2 1 20))
  (check-equal? (unmarshal-stats '("7" "4" "2" "#f"))
                (stat-set 7 4 2 #f))

  (check-equal? (unmarshal-line "2:1,1,1,#f")
                (sized-set 2 (stat-set 1 1 1 #f)))

  (check-equal? (unmarshal-lines '())
                '())
  (check-equal? (unmarshal-lines (list "2:1,1,1,#f"))
                (list (sized-set 2 (stat-set 1 1 1 #f))))
  (check-equal? (unmarshal-lines (list "2:1,1,1,#f" "3:3,2,0,#f"))
                (list (sized-set 2 (stat-set 1 1 1 #f))
                      (sized-set 3 (stat-set 3 2 0 #f))))
  (check-equal? (unmarshal-lines (list "3:3,2,0,#f" "2:1,1,1,#f"))
                (list (sized-set 3 (stat-set 3 2 0 #f))
                      (sized-set 2 (stat-set 1 1 1 #f))))

  (check-equal? (let ([file "test.txt"]
                      [data "2:1,1,1,#f\n3:3,2,0,#f"])
                  (display-to-file data file #:mode 'text #:exists 'replace)
                  (define stats (load-stats-from-file file))
                  (delete-file file)
                  stats)
                (list (sized-set 2 (stat-set 1 1 1 #f))
                      (sized-set 3 (stat-set 3 2 0 #f))))
  ;; Blank file 
  (check-equal? (let ([file "test.txt"]
                      [data ""])
                  (display-to-file data file #:mode 'text #:exists 'replace)
                  (define stats (load-stats-from-file file))
                  (delete-file file)
                  stats)
                '())

  
  ;; Tests saving to file
  
  (check-equal? (let ([file "test.txt"]
                      [stats (list (sized-set 2 2x2-stat-set)
                                   (sized-set 3 3x3-stat-set))])
                  (save-stats-to-file stats file)
                  (define lines (file->lines file))
                  (delete-file file)
                  lines)
                (list "2:5,3,1,#f" "3:7,4,1,#f"))
  (check-equal? (let ([file "test.txt"]
                      [stats (list (sized-set 3 3x3-stat-set)
                                   (sized-set 2 2x2-stat-set))])
                  (save-stats-to-file stats file)
                  (define lines (file->lines file))
                  (delete-file file)
                  lines)
                (list "2:5,3,1,#f" "3:7,4,1,#f"))
  
  (check-equal? (marshal-stats '()) "")
  (check-equal? (marshal-stats 3-only) "3:7,4,1,#f\n")
  (check-equal? (marshal-stats 2-and-3) "2:5,3,1,#f\n3:7,4,1,#f\n")
  
  (check-equal? (marshal-sized-set (sized-set 3 (stat-set 5 4 3 2)))
                "3:5,4,3,2")

  (check-equal? (marshal-stat-set (stat-set 3 2 1 #f))
                "3,2,1,#f")
  (check-equal? (marshal-stat-set (stat-set 5 4 3 2))
                "5,4,3,2")

  (check-equal? (marshal-average-time 12) "12")
  (check-equal? (marshal-average-time 1234) "1234")
  (check-equal? (marshal-average-time #f) "#f")

  
  "all tests run")

