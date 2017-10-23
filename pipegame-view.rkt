#lang racket

(require 2htdp/universe
         2htdp/image)
(require "pipegame.rkt"
         "pipegame-statistics.rkt")


#|
This file provides the view for the pipe game.

Event handling (controller) is done in pipegame.rkt

Run and evaluate (main) to launch a menu to get started. 
|#


;;; Data definitions for Board, Cell, and Orientation can be found in
;;; pipegame.rkt

(struct board-state [board time stats] #:transparent)
;; A BoardState is a (board-state Board Time Statistics) where:
;; -- board is the current board
;; -- time is the amount of time that has elapsed for the current board
;; -- stats is the current statistics for this board state

;; A Time is a Number
;; Represents the number of seconds since the game was started


;; =============================================================================
;; Graphics constants
;; =============================================================================
(define MENU-BUTTON-SIZE 45)
(define MENU-TEXT-HEIGHT 12)
(define MENU-PAD-SIZE 5)
(define MENU-PAD (square MENU-PAD-SIZE "outline" (color 0 0 0 0))) ; invisible
(define CELL-SIZE 30)
(define PAD-X 0)
(define PAD-Y 30)
(define SHOW-STATS
  (let ([width (+ MENU-BUTTON-SIZE MENU-PAD-SIZE MENU-BUTTON-SIZE)])
    (place-image (text "View Stats" MENU-TEXT-HEIGHT "black")
                 (/ width 2) (/ MENU-BUTTON-SIZE 2)
                 (rectangle width MENU-BUTTON-SIZE "solid" "lightblue"))))
(define STAT-DIVIDER (text "---------" MENU-TEXT-HEIGHT "black"))
(define MENU-BUTTONS (hash 0 (range 6 11) 1 (range 11 16)))
(define END-IMG (bitmap/file "images/end.png"))
(define ELBOW-IMG (bitmap/file "images/elbow.png"))
(define BAR-IMG (bitmap/file "images/bar.png"))
(define TEE-IMG (bitmap/file "images/tee.png"))
(define CROSS-IMG (bitmap/file "images/cross.png"))
(define PS-END-IMG (bitmap/file "images/end-power-source.png"))
(define PS-ELBOW-IMG (bitmap/file "images/elbow-power-source.png"))
(define PS-BAR-IMG (bitmap/file "images/bar-power-source.png"))
(define PS-TEE-IMG (bitmap/file "images/tee-power-source.png"))
(define PS-CROSS-IMG (bitmap/file "images/cross-power-source.png"))
(define END-CONN-IMG (bitmap/file "images/end-connected.png"))
(define ELBOW-CONN-IMG (bitmap/file "images/elbow-connected.png"))
(define BAR-CONN-IMG (bitmap/file "images/bar-connected.png"))
(define TEE-CONN-IMG (bitmap/file "images/tee-connected.png"))
(define CROSS-CONN-IMG (bitmap/file "images/cross-connected.png"))



;; =============================================================================
;;; MAIN
;; =============================================================================

;; main : -> Statistics
;; Starts up the game
(define (main)
  (big-bang '()
             [name "Pipe Game Menu"]
             [to-draw menu-render]
             [on-mouse menu-mouse-handler]))


;; =============================================================================
;;; MENU
;; =============================================================================

;; -------------------------------------
;; RENDERING
;; -------------------------------------

;; menu-render : _ -> Image
;; Renders the menu
(define (menu-render _)
  (above
   (text "Select a board" MENU-TEXT-HEIGHT "black")
   (text "size to begin." MENU-TEXT-HEIGHT "black")
   MENU-PAD
   (beside (menu-column (reverse (hash-ref MENU-BUTTONS 0)))
           MENU-PAD
           (menu-column (reverse (hash-ref MENU-BUTTONS 1))))
   MENU-PAD
   SHOW-STATS))

;; menu-column : [List-of Number] -> Image
;; Creates a column of menu buttons with the given numerical values
(define (menu-column vals)
  (for/fold [(image empty-image)]
            [(i vals)]
    (above (menu-button i)
           MENU-PAD
           image)))

;; menu-button : Number -> Image
;; Creates a menu button of the given numerical value
(define (menu-button size)
  (place-image (text (number->string size) MENU-TEXT-HEIGHT "black")
               (/ MENU-BUTTON-SIZE 2)
               (/ MENU-BUTTON-SIZE 2)
               (square MENU-BUTTON-SIZE "solid" "green")))


;; -------------------------------------
;; INPUT HANDLING
;; -------------------------------------

;; menu-mouse-handler : _ Number Number MouseEvent -> Void
;; Launches a board if a button has been pressed, otherwise does nothing.
(define (menu-mouse-handler _ x y mouse-event)
  (define button (board-button-pressed? x y mouse-event))
  (cond [button (start-board button)]
        [(stats-button-pressed? x y mouse-event) (show-stats)]
        [else (void)]))


;; board-button-pressed? : Number Number MouseEvent -> [Maybe Number]
;; Determines the board button that has been pressed, if any
(define (board-button-pressed? x y mouse-event)
  (define size-and-pad (+ MENU-BUTTON-SIZE MENU-PAD-SIZE))
  
  (define button-x (quotient x size-and-pad))
  (define button-y (quotient (- y (* 2 MENU-TEXT-HEIGHT)) size-and-pad))
  
  ;; -> Boolean
  ;; Determines if the x coordinate is on a button
  (define (in-x?)
    (and (not (and (> x MENU-BUTTON-SIZE)
                   (< x size-and-pad)))
         (hash-has-key? MENU-BUTTONS button-x)))
  
  ;; -> Boolean
  ;; Determines if the y coordinate is on a button
  (define (in-y?)
    (and (not (< (modulo (max 0 (- y (* 2 MENU-TEXT-HEIGHT))) size-and-pad)
                 MENU-PAD-SIZE))
         (< button-y (length (hash-ref MENU-BUTTONS button-x)))))
  
  (cond
    [(not (and (in-x?) (in-y?))) #false]
    [(string=? mouse-event "button-down")
     (list-ref (hash-ref MENU-BUTTONS button-x) button-y)]
    [else #false]))

;; stats-button-pressed? : Number Number MouseEvent -> Boolean
;; Determines if the view stats button was pressed
(define (stats-button-pressed? x y mouse-event)

  ;; -> Boolean
  ;; Determines if the x coordinate is within the stats button
  (define (in-x?)
    (and (> x MENU-PAD-SIZE)
         (< x (+ MENU-PAD-SIZE
                 MENU-BUTTON-SIZE MENU-PAD-SIZE MENU-BUTTON-SIZE))))

  ;; -> Boolean
  ;; Determines if the y coordinate is within the stats button
  (define (in-y?)
    (define y-top (+ (* 2 MENU-TEXT-HEIGHT)
                     MENU-PAD-SIZE
                     (* (length (hash-ref MENU-BUTTONS 0))
                        (+ MENU-PAD-SIZE MENU-BUTTON-SIZE))
                     MENU-PAD-SIZE))
    (and (> y y-top)
         (< y (+ y-top MENU-BUTTON-SIZE))))

  (and (string=? mouse-event "button-down") in-x? (in-y?)))


;; =============================================================================
;;; STATS
;; =============================================================================

;; show-stats : -> Void
;; Launches a window that shows play statistics
(define (show-stats)
  (big-bang (load-stats)
            [name "Pipegame Statistics"]
            [to-draw render-stats])
  (void))

;; render-stats : Statistics -> Image
;; Displays play statistics
(define (render-stats stats)
  (define base (render-base-stats stats))
  (define inner (above base
                       (render-stats-inner stats)))
    
  (define inner-width (image-width inner))
  (define inner-height (image-height inner))
  (define scene-width (max 200 (+ MENU-TEXT-HEIGHT inner-width)))
  (define scene-height (max 200 (+ MENU-TEXT-HEIGHT inner-height)))
  (place-image inner (/ scene-width 2) (/ scene-height 2)
               (empty-scene scene-width scene-height)))


;; render-base-stats : Statistics -> Image
;; Renders the base statistics that apply across all sizes (totals and such)
(define (render-base-stats stats)
  (cond [(empty? stats) (text "No statistics yet!" MENU-TEXT-HEIGHT "black")]
        [else (above (text (string-append "Total plays: "
                                          (number->string (total-plays stats)))
                           MENU-TEXT-HEIGHT "black")
                     (text (string-append
                            "Total win percent: "
                            (number->string (total-win-percent stats))
                            "%")
                           MENU-TEXT-HEIGHT "black")
                     (text (string-append
                            "Total perfect percent: "
                            (number->string (total-perfect-percent stats))
                            "%")
                           MENU-TEXT-HEIGHT "black"))]))

;; render-stats-inner : Statistics -> Image
;; Renders the internal details on the individual sized sets onto the given base
(define (render-stats-inner stats)
  (cond [(empty? stats) empty-image]
        [else (above STAT-DIVIDER
                     (render-sized-set (first stats))
                     (render-stats-inner (rest stats)))]))

;; render-sized-set : SizedStatSet -> Image
;; Renders the sized stat set
(define (render-sized-set set)
  (above (text (number->string (sized-set-size set)) MENU-TEXT-HEIGHT "black")
         STAT-DIVIDER
         (render-stat-set (sized-set-stats set))))

;; render-stat-set : StatSet -> Image
;; Renders the specific data on this stat set
(define (render-stat-set set)
  (above (text (string-append "Plays: " (number->string (plays set)))
               MENU-TEXT-HEIGHT "black")
         (text (string-append "Win percent: "
                              (number->string (win-percent set))
                              "%")
               MENU-TEXT-HEIGHT "black")
         (text (string-append "Perfect percent: "
                              (number->string (perfect-percent set))
                              "%")
               MENU-TEXT-HEIGHT "black")
         (if (average-time set)
             (text (string-append "Average time: "
                                  (time->string (average-time set)))
                   MENU-TEXT-HEIGHT "black")
             empty-image)))
                              


;; =============================================================================
;;; BOARD
;; =============================================================================

;; start-board : Number -> Void
;; Runs a game board of size size and returns updated statistics
(define (start-board size)
  (define ending-state
    (big-bang (board-state (generate-board size) 0 '())
              [name "Pipe Game"]
              [to-draw render]
              [on-tick add-time 1]
              [on-mouse send-to-mouse-handler]
              [on-key key-handler]))
  (define game-stats
    (add-to-stats (board-state-stats ending-state)
                  (board-state-board ending-state)
                  (board-state-time ending-state)))
  (save-stats game-stats))

;; add-time : BoardState -> BoardState
;; Advances the time played for this board if the game is not over
(define (add-time state)
  (if (all-connected? (board-state-board state))
      state
      (board-state (board-state-board state)
                   (+ 1 (board-state-time state))
                   (board-state-stats state))))

;; key-handler : BoardState KeyEvent -> BoardState
;; Starts a new game of same size if key 'n' is pressed
(define (key-handler state key)
  (cond [(key=? key "n")
         (board-state (generate-board (board-size (board-state-board state)))
                      0
                      (add-to-stats (board-state-stats state)
                                    (board-state-board state)
                                    (board-state-time state)))]
        [else state]))

;; send-to-mouse-handler : BoardState Number Number MouseEvent -> BoardState
;; Prepares the coordinates to send the mouse-handler in pipegame.rkt
(define (send-to-mouse-handler state x y mouse-event)
  (define cell-coords (image-posn->cell-posn (posn x y)))
  (board-state (mouse-handler (board-state-board state)
                              (posn-x cell-coords) (posn-y cell-coords)
                              mouse-event)
               (board-state-time state)
               (board-state-stats state)))



;; =============================================================================
;;; RENDER
;; =============================================================================

;; render : BoardState -> Image
;; Draws the current state of the board
(define (render board-state)
  (define board (board-state-board board-state))
  (place-image/align (show-status board
                                  (board-state-time board-state))
                     0 0 "left" "top"
                     (draw-list-of-cells (board-cells board)
                                         (board-size board))))


;; draw-list-of-cells : [Listof Cell] Number Number -> Image
;; Draws the list of cells onto grid of size width, height
(define (draw-list-of-cells cells size)
  (cond [(empty? cells) (empty-scene (+ PAD-X (* CELL-SIZE size))
                                     (+ PAD-Y (* CELL-SIZE size)))]
        [else
         (define cell-coords (cell-posn->image-posn (cell-posn (first cells))))
         (place-image (draw-cell (first cells))
                      (posn-x cell-coords) (posn-y cell-coords)
                      (draw-list-of-cells (rest cells) size))]))


;; draw-cell : Cell Boolean Boolean -> Image
;; Draws the cell
(define (draw-cell cell)
  (define t (cell-type cell))
  (define o (cell-orientation cell))
  (define c (cell-connected cell))
  (define p (power-source? cell))
  (cond [(symbol=? t 'end)
           (cond [p (rotate-cell-image PS-END-IMG o)]
                 [c (rotate-cell-image END-CONN-IMG o)]
                 [else (rotate-cell-image END-IMG o)])]
          [(symbol=? t 'elbow)
           (cond [p (rotate-cell-image PS-ELBOW-IMG o)]
                 [c (rotate-cell-image ELBOW-CONN-IMG o)]
                 [else (rotate-cell-image ELBOW-IMG o)])]
          [(symbol=? t 'bar)
           (cond [p (rotate-cell-image PS-BAR-IMG o)]
                 [c (rotate-cell-image BAR-CONN-IMG o)]
                 [else (rotate-cell-image BAR-IMG o)])]
          [(symbol=? t 'tee)
           (cond [p (rotate-cell-image PS-TEE-IMG o)]
                 [c (rotate-cell-image TEE-CONN-IMG o)]
                 [else (rotate-cell-image TEE-IMG o)])]
          [(symbol=? t 'cross)
           (cond [p (rotate-cell-image PS-CROSS-IMG o)]
                 [c (rotate-cell-image CROSS-CONN-IMG o)]
                 [else (rotate-cell-image CROSS-IMG o)])]))


;; rotate-cell-image : Image Orientation -> Image
;; Rotates the cell image to the appropriate orientation.
(define (rotate-cell-image image o)
  (cond [(symbol=? o 'north) image]
        [(symbol=? o 'east) (rotate 270 image)]
        [(symbol=? o 'south) (rotate 180 image)]
        [(symbol=? o 'west) (rotate 90 image)]))

;; show-status : Board Time -> Image
;; Shows the status of the game (score)
(define (show-status board time)
  (define size (board-size board))
  (define display-string
    (string-append
     (if (all-connected? board)
         (cond [(= (board-turns board) (board-perfect board)) "Perfect!  "]
               [(> (board-turns board) (board-perfect board)) "Complete!  "]
               [else "Incredible!  "])
         "")
     (number->string (board-turns board))  " / "
     (number->string (board-perfect board)) " moves"))
  (define display-time
    (text (string-append "Time: " (time->string time)) 12 "black"))
  (define top (above (text display-string 12 "black") display-time))
  (place-image/align
   top
   (/ (+ PAD-X (* CELL-SIZE size)) 2) (/ PAD-Y 2)
   "center" "center"
   (empty-scene (+ PAD-X (* CELL-SIZE size)) PAD-Y)))



;; =============================================================================
;;; UTILS
;; =============================================================================

;; cell-posn->image-posn : Posn -> Posn
;; Gives the position of the center of the given cell posn in terms of pixels 
(define (cell-posn->image-posn p)
  (define half-cell (/ CELL-SIZE 2))
  (posn (+ PAD-X half-cell (* CELL-SIZE (posn-x p)))
        (+ PAD-Y half-cell (* CELL-SIZE (posn-y p)))))


;; image-posn->cell-posn : Posn -> Posn
;; Determine the cell position at the given pixel coordinates
(define (image-posn->cell-posn p)
  (posn (floor (/ (- (posn-x p) PAD-X) CELL-SIZE))
        (floor (/ (- (posn-y p) PAD-Y) CELL-SIZE))))

;; time->string : Time -> String
;; Convert the given time into a string m:ss. Doesn't go into hours.
;; If time is not an integer, will truncate
(define (time->string time)
  (define minutes (number->string (quotient (floor time) 60)))
  (define seconds (number->string (modulo (floor time) 60)))
  (define formatted-seconds (if (= (string-length seconds) 1)
                                (string-append "0" seconds)
                                seconds))
  (string-append minutes ":" formatted-seconds))


;; =============================================================================
;;; TESTS
;; =============================================================================
(module+ test 
  
  (require rackunit rackunit/text-ui)

  ;;; Testing constants
  (define 2x2-board (board (list (power-source (posn 0 0) 'end 'east #t)
                                 (cell (posn 1 0) 'elbow 'south #t)
                                 (cell (posn 0 1) 'end 'east #t)
                                 (cell (posn 1 1) 'elbow 'west #t))
                           2 (posn -1 -1) 0 0))
  (define 3x3-board-conn (board (list (cell (posn 0 0) 'elbow 'east #t)
                                      (cell (posn 1 0) 'bar 'east #t)
                                      (cell (posn 2 0) 'elbow 'south #t)
                                      (cell (posn 0 1) 'tee 'east #t)
                                      (power-source
                                       (posn 1 1) 'elbow 'south #t)
                                      (cell (posn 2 1) 'end 'north #t)
                                      (cell (posn 0 2) 'end 'north #t)
                                      (cell (posn 1 2) 'elbow 'north #t)
                                      (cell (posn 2 2) 'end 'west #t))
                                3 (posn -1 -1) 0 0))
  (define 3x3-board-discon (board (list (cell (posn 0 0) 'elbow 'north #f)
                                        (cell (posn 1 0) 'bar 'north #t)
                                        (cell (posn 2 0) 'elbow 'north #f)
                                        (cell (posn 0 1) 'tee 'north #f)
                                        (power-source
                                         (posn 1 1) 'elbow 'north #t)
                                        (cell (posn 2 1) 'end 'north #f)
                                        (cell (posn 0 2) 'end 'north #f)
                                        (cell (posn 1 2) 'elbow 'north #f)
                                        (cell (posn 2 2) 'end 'north #f))
                                  3 (posn -1 -1) 0 6))

  (define 3-stat-set (stat-set 7 4 1 10))
  (define 6-stat-set (stat-set 4 2 2 #f))
  (define 3-sized-set (sized-set 3 3-stat-set))
  (define 3-stats (list 3-sized-set))
  (define long-stats (list (sized-set 3 3-stat-set)
                           (sized-set 4 (stat-set 8 4 1 #f))
                           (sized-set 5 (stat-set 6 3 2 #f))
                           (sized-set 6 6-stat-set)))
  


  ;; tests for menu
  (check-equal? (menu-render 0)
                (above (text "Select a board" MENU-TEXT-HEIGHT "black")
                       (text "size to begin." MENU-TEXT-HEIGHT "black")
                       MENU-PAD
                       (beside (menu-column (reverse (range 6 11)))
                               MENU-PAD
                               (menu-column (reverse (range 11 16))))
                       MENU-PAD
                       SHOW-STATS))
  
  (check-equal? (menu-column (range 6 9))
                (above (menu-button 8)
                       MENU-PAD
                       (menu-button 7)
                       MENU-PAD
                       (menu-button 6)
                       MENU-PAD))
  
  (check-equal? (menu-button 12)
                (place-image (text "12" 12 "black")
                             (/ MENU-BUTTON-SIZE 2)
                             (/ MENU-BUTTON-SIZE 2)
                             (square MENU-BUTTON-SIZE "solid" "green")))

  (check-equal? (board-button-pressed? 0 0 "button-down") #false)
  (check-equal? (board-button-pressed? 30 60 "button-up") #false)
  (check-equal? (board-button-pressed? 30 60 "button-down") 6)
  (check-equal? (board-button-pressed? 90 30 "button-down") 11)
  (check-equal? (board-button-pressed? 30 290 "button-down") #f)

  (check-equal? (stats-button-pressed? 0 0 "button-down") #false)
  (check-equal? (stats-button-pressed? 30 60 "button-up") #false)
  (check-equal? (stats-button-pressed? 30 290 "button-down") #t)


  ;; tests for rendering stats
  (check-equal? (render-stats '())
                (place-image
                 (text "No statistics yet!" MENU-TEXT-HEIGHT "black")
                 100 100 (empty-scene 200 200)))
  (check-equal? (render-stats 3-stats)
                (place-image
                 (above (render-base-stats 3-stats)
                        (render-stats-inner 3-stats))
                 100 100
                 (empty-scene 200 200)))
  (check-equal? (render-stats long-stats)
                (place-image
                 (above (render-base-stats long-stats)
                        (render-stats-inner long-stats))
                 100 174
                 (empty-scene 200 348)))

  (check-equal? (render-base-stats '())
                (text "No statistics yet!" MENU-TEXT-HEIGHT "black"))
  (check-equal? (render-base-stats 3-stats)
                (above (text "Total plays: 7" MENU-TEXT-HEIGHT "black")
                       (text "Total win percent: 57%"
                             MENU-TEXT-HEIGHT "black")
                       (text "Total perfect percent: 14%"
                             MENU-TEXT-HEIGHT "black")))

  (check-equal? (render-stats-inner 3-stats)
                (above empty-image
                       STAT-DIVIDER
                       (render-sized-set 3-sized-set)))
  (check-equal? (render-stats-inner long-stats)
                (above STAT-DIVIDER
                       (render-sized-set (sized-set 3 3-stat-set))
                       STAT-DIVIDER
                       (render-sized-set (sized-set 4 (stat-set 8 4 1 #f)))
                       STAT-DIVIDER
                       (render-sized-set (sized-set 5 (stat-set 6 3 2 #f)))
                       STAT-DIVIDER
                       (render-sized-set (sized-set 6 6-stat-set))))

  (check-equal? (render-sized-set 3-sized-set)
                (above (text "3" MENU-TEXT-HEIGHT "black")
                       STAT-DIVIDER
                       (render-stat-set 3-stat-set)))

  (check-equal? (render-stat-set 3-stat-set)
                (above (text "Plays: 7" MENU-TEXT-HEIGHT "black")
                       (text "Win percent: 57%" MENU-TEXT-HEIGHT "black")
                       (text "Perfect percent: 14%" MENU-TEXT-HEIGHT "black")
                       (text "Average time: 0:10" MENU-TEXT-HEIGHT "black")))
  (check-equal? (render-stat-set 6-stat-set)
                (above (text "Plays: 4" MENU-TEXT-HEIGHT "black")
                       (text "Win percent: 50%" MENU-TEXT-HEIGHT "black")
                       (text "Perfect percent: 50%" MENU-TEXT-HEIGHT "black")))

  
  
  ;; test for mouse-handler preparation
  (check-equal? (let ([b (board (list (power-source
                                       (posn 0 0) 'end 'north #t)
                                      (cell (posn 1 0) 'elbow 'south #f)
                                      (cell (posn 0 1) 'end 'east #f)
                                      (cell (posn 1 1) 'elbow 'west #f))
                                2 (posn -1 -1) 0 0)])
                  (send-to-mouse-handler (board-state b 5 '())
                                         (+ PAD-X 15)
                                         (+ PAD-Y 15)
                                         "button-down")
                  b)
                (board (board-cells 2x2-board) 2 (posn 0 0) 1 0))

  ;; Tests for keyhandler
  (check-equal? (key-handler (board-state 2x2-board 5 '()) "x")
                (board-state 2x2-board 5 '()))
  (check-not-equal? (key-handler (board-state 2x2-board 0 '()) "n")
                    (board-state 2x2-board 0 '()))
  (check-equal? (length (board-state-stats
                         (key-handler (board-state 2x2-board 0 '()) "n")))
                1)
  (check-equal? (board-size (board-state-board
                             (key-handler (board-state 2x2-board 0 '()) "n")))
                2)
  (check-equal? (board-state-time
                 (key-handler (board-state 2x2-board 17 '()) "n"))
                0)

  (check-equal? (board-state-stats
                 (key-handler (board-state 2x2-board 17 '()) "n"))
                (list (sized-set 2 (stat-set 1 1 1 17))))

  ;; Tests for add-time
  (check-equal? (add-time (board-state 3x3-board-discon 0 '()))
                (board-state 3x3-board-discon 1 '()))
  (check-equal? (add-time (board-state 3x3-board-discon 7 '()))
                (board-state 3x3-board-discon 8 '()))
  (check-equal? (add-time (board-state 3x3-board-conn 10 '()))
                (board-state 3x3-board-conn 10 '()))

  ;; Tests for renderer
  (check-equal? (render (board-state 3x3-board-discon 5 '()))
                (place-image/align (show-status 3x3-board-discon 5)
                                   0 0 "left" "top"
                                   (draw-list-of-cells
                                    (board-cells 3x3-board-discon) 3)))
  (check-equal? (render (board-state 3x3-board-conn 7 '()))
                (place-image/align
                 (show-status 3x3-board-conn 7)
                 0 0 "left" "top"
                 (draw-list-of-cells (board-cells 3x3-board-conn) 3)))
  
  (check-equal? (draw-list-of-cells '() 0)
                (empty-scene PAD-X PAD-Y))
  (check-equal? (draw-list-of-cells (board-cells 3x3-board-discon) 3)
                (foldr (λ (cell image)
                         (define cell-coords
                           (cell-posn->image-posn (cell-posn cell)))
                         (place-image (draw-cell cell)
                                      (posn-x cell-coords) (posn-y cell-coords)
                                      image))
                       (empty-scene (+ PAD-X (* CELL-SIZE 3))
                                    (+ PAD-Y (* CELL-SIZE 3)))
                       (board-cells 3x3-board-discon)))
  
  (check-equal? (draw-cell (cell (posn 0 0) 'end 'north #f))
                END-IMG)
  (check-equal? (draw-cell (power-source (posn 2 1) 'tee 'south #t))
                (rotate 180 PS-TEE-IMG))
  (check-equal? (draw-cell (cell (posn 1 2) 'bar 'east #t))
                (rotate 270 BAR-CONN-IMG))
  (check-equal? (show-status 2x2-board 2)
                (place-image/align
                 (above (text "Perfect!  0 / 0 moves" 12 "black")
                        (text "Time: 0:02" 12 "black"))
                 (/ (+ PAD-X (* CELL-SIZE 2)) 2)
                 (/ PAD-Y 2)
                 "center"
                 "center"
                 (empty-scene (+ PAD-X (* CELL-SIZE 2)) PAD-Y)))
  (check-equal? (show-status 2x2-board 10)
                (place-image/align
                 (above
                  (text (string-append "Perfect!  0 / 0 moves")
                        12 "black")
                  (text "Time: 0:10" 12 "black"))
                 (/ (+ PAD-X (* CELL-SIZE 2)) 2) (/ PAD-Y 2)
                 "center" "center"
                 (empty-scene (+ PAD-X (* CELL-SIZE 2)) PAD-Y)))
  
  
  ;; Tests for utils
  (check-equal? (cell-posn->image-posn (posn 0 0))
                (posn (+ (/ CELL-SIZE 2) (* CELL-SIZE 0) PAD-X)
                      (+ (/ CELL-SIZE 2) (* CELL-SIZE 0) PAD-Y)))
  (check-equal? (cell-posn->image-posn (posn 1 2))
                (posn (+ (/ CELL-SIZE 2) (* CELL-SIZE 1) PAD-X)
                      (+ (/ CELL-SIZE 2) (* CELL-SIZE 2) PAD-Y)))
  
  (check-equal? (image-posn->cell-posn (posn 25 50))
                (posn (floor (/ (- 25 PAD-X) CELL-SIZE))
                      (floor (/ (- 50 PAD-Y) CELL-SIZE))))
  (check-equal? (image-posn->cell-posn (posn 0 30))
                (posn 0 0))

  (check-equal? (time->string 10) "0:10")
  (check-equal? (time->string 60) "1:00")
  (check-equal? (time->string 111) "1:51")
  (check-equal? (time->string 0) "0:00")
  (check-equal? (time->string 3700) "61:40")
  (check-equal? (time->string 241/5) "0:48")
  (check-equal? (time->string 244/5) "0:48")

  "all tests run")
  