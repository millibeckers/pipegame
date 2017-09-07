#lang racket

(require 2htdp/image 2htdp/universe)

(provide (struct-out board)
         (struct-out cell)
         (struct-out power-source)
         (struct-out posn)
         mouse-handler
         key-handler
         generate-board
         all-connected?)

#|

Help! I was trying to make a game in racket, but all of my computer's circuits
got scrambled! I need you to help me unscramble all of the tiles and connect the
circuits back up again so that I can keep on Racketing. Will you accept the
challenge?

In order to help me out, open and run the pipegame-view.rkt file and run (main)
in the interactions window


FUTURE IDEAS:
- Implement graphics with racket/gui (not likely)
- Add a timer
- Save statistics in a file that will persist across plays

|#

;; =============================================================================
;;; DATA DEFINITIONS
;; =============================================================================

(struct board (cells
               size
               [turned-cell #:mutable]
               [turns #:mutable]
               perfect) #:transparent)
;; A Board is a (board [Listof Cell] Number Posn Number Number) where:
;; -- cells is the list of cells in the board
;; -- size is the size of one side (the board is square)
;; -- turned-cell is the position of the most recently turned cell or
;;    (posn -1 -1) if no cell has been turned yet
;; -- turns is the number of turns the user has made
;; -- perfect is the number of turns needed to return the board to the layout
;;    created at board generation (occasionally there exists more than one
;;    to a board and in this case it is possible to solve the board in fewer
;;    turns. 
;; There must be one and only one PowerCell in cells 

(struct cell (posn
              type
              [orientation #:mutable]
              [connected #:mutable]) #:transparent)
;; A Cell is a (cell Posn CellType Orientation Boolean) where:
;; -- posn is the position of the cell
;; -- type is the shape of the cell
;; -- orientation is the orientation of the cell
;; -- connected determines if the cell is connected to the power souce as
;;    determined by the most recent call to update-connected!

(struct power-source cell () #:transparent)
;; A PowerCell is a (power-source Posn CellType Orientation Boolean)
;; See data defintion of Cell for description of fields
;; This represents the power source
;; The PowerCell is always connected 

;; A CellType is one of:
;; - 'end
;; - 'elbow
;; - 'bar
;; - 'tee
;; - 'cross

;; An Orientation is one of:
;; - 'north
;; - 'east
;; - 'south
;; - 'west

(struct posn (x y) #:transparent)
;; A Posn is a (posn Number Number)
;; Represents a two-dimensional point. 

#|

THE DIFFERENT CELL SHAPES IN 'north POSITION 

   'end      'elbow      'bar  
    _ _        _ _        _ _   
   | | |      | |_|      | | | 
   |_ _|      |_ _|      |_|_| 

        'tee      'cross
         _ _        _ _
        |_|_|      |_|_|
        |_ _|      |_|_|

|#


;; An Edge is a (edge Posn Posn)
(struct edge [weight from to] #:transparent)
;; Where from is the location of the cell it is going from and to is the
;; location of the cell that it is going to
;; Used in cell generation only 


;; =============================================================================
;;; INPUT HANDLERS
;; =============================================================================

;; mouse-handler : Board Number Number MouseEvent -> Board
;; Receives a mouse-event and coordinates in cell form and dispatches to
;; appropriate function
(define (mouse-handler board x y mouse-event)
  (unless (all-connected? board)
    (define cell-at-coords (cell-at (posn x y)
                                    (board-cells board)))
    (when (and (mouse=? mouse-event "button-down") cell-at-coords) 
      (rotate-cell! (posn x y) board)
      (update-connected! (board-cells board))))
  board)


;; key-handler : Board KeyEvent -> Board
;; Starts a new game of same size if key 'n' is pressed
(define (key-handler board key)
  (cond [(key=? key "n") (generate-board (board-size board))]
        [else board]))



;; =============================================================================
;;; BOARD
;; =============================================================================

;; all-connected? : Board -> Boolean
;; Determines if all cells are connected (according to the connnected field)
(define (all-connected? board)
  (andmap cell-connected (board-cells board)))


;; =============================================================================
;;; BOARD GENERATION
;; =============================================================================

;; generate-board : Number -> Board
;; Generates a square board with the dimensions of size
(define (generate-board size)
  (scramble (board
             (edges->cells
              (kruskals
               (sort (assign-edges size)
                     (λ (e1 e2) (< (edge-weight e1) (edge-weight e2))))
               size)
              size)
            size (posn -1 -1) 0 0)))


;; assign-edges : Size [Listof Edge] -> [Listof Edge]
;; Create edges to connect all cells, giving them random weights
(define (assign-edges size)

  ;; Edge Number Number Number Number -> Boolean
  ;; Determines if the given edge connects to the cell in the direction
  ;; given in the form of offets
  (define (connected-to? e x y off-x off-y)
    (or (and (equal? (edge-from e) (posn x y))
             (equal? (edge-to e) (posn (+ off-x x) (+ off-y y))))
        (and (equal? (edge-to e) (posn x y))
             (equal? (edge-from e) (posn (+ off-x x) (+ off-y y))))))

  ;; Number Number Number Number -> Edge
  ;; Creates an edge with the given starting point and an ending point
  ;; that is determined by the offset
  (define (make-new-edge x y off-x off-y)
    (edge (random (* size size)) (posn x y) (posn (+ off-x x) (+ off-y y))))
  
  (for*/fold ([edges '()])
             ([x (range size)]
              [y (range size)])
    (define temp '())
    (unless (or (= x 0)
                (memf (λ (e) (connected-to? e x y -1 0)) edges))
      (set! temp (cons (make-new-edge x y -1 0) temp)))
    (unless (or (= x (sub1 size))
                (memf (λ (e) (connected-to? e x y 1 0)) edges))
      (set! temp (cons (make-new-edge x y 1 0) temp)))
    (unless (or (= y 0)
                (memf (λ (e) (connected-to? e x y 0 -1)) edges))
      (set! temp (cons (make-new-edge x y 0 -1) temp)))
    (unless (or (= y (sub1 size))
                (memf (λ (e) (connected-to? e x y 0 1)) edges))
      (set! temp (cons (make-new-edge x y 0 1) temp)))
    
    (append temp edges)))


;; kruskals : [Listof Edge] Number -> [Listof Edge]
;; Use Kruskal's algorithm to find the minimum spanning tree given the edges
;; Edges must be sorted in increasing order 
(define (kruskals edges size)
  ;; Initialize the hashmap where every posn points to itself
  (define working-trees
    (make-hash
     (for*/fold ([l '()])
                ([x (range size)]
                 [y (range size)])
       (cons (cons (posn x y) (posn x y)) l))))
  
  (define accepted-edges '())
  
  (for ([e edges]
        #:when (not (equal? (find working-trees (edge-from e))
                            (find working-trees (edge-to e))))
        [n (range (sub1 (* size size)))])
    (unless (member e accepted-edges) ; TODO find a non-janky-hack for this
      (set! accepted-edges (cons e accepted-edges))) ; loop situation
    (union! working-trees
            (find working-trees (edge-from e))
            (find working-trees (edge-to e))))
  
  ;; Return the edges for the minimum spanning tree
  accepted-edges)

;; -------------------------------------
;;; Utility functions for Kruskal's algorithm
;; -------------------------------------

;; union! : [Hashmap Posn Posn] Posn Posn -> Void
;; union the two trees together in the hashmap
(define (union! tree-map tree1 tree2)
  (define root1 (find tree-map tree1))
  (define root2 (find tree-map tree2))
  (hash-set! tree-map root2 root1))


;; find : [Hashmap Posn Posn] Posn -> Posn
;; find the root node that the target is connected to
(define (find tree-map target)
  (define value (hash-ref tree-map target))
  (if (equal? value target)
      target
      (find tree-map value)))


;; edges->cells : [Listof Edge] Number -> [Listof Cell]
;; Converts the given list of Edges into a list of Cells
(define (edges->cells edges size)
  (define power-source-posn (posn (random size) (random size)))
  (for*/fold ([cells '()])
             ([x (range size)]
              [y (range size)])
    (cons (edges->cell (filter (λ (e) (or (equal? (edge-from e) (posn x y))
                                          (equal? (edge-to e) (posn x y))))
                               edges)
                       (posn x y)
                       (if (equal? power-source-posn (posn x y))
                           power-source
                           cell))
          cells)))
      

;; edges->cell : [Listof Edge] Posn [-> Cell]-or-[-> PowerCell] -> Cell
;; Create a Cell using constructor at Posn posn given all the Edges that
;; connect to that Cell
(define (edges->cell edges posn constructor)
  (cond [(= (length edges) 1)
         (define other-posn (if (equal? (edge-to (first edges)) posn)
                                (edge-from (first edges))
                                (edge-to (first edges))))
         (constructor posn 'end (posn-neighbor? posn other-posn) #t)]
        [(= (length edges) 2) 
         (define connection-points
           (connection-points-from-edges edges posn))
         (cond [(equal? (opposite-orientation (first connection-points)) 
                        (second connection-points)) ; 'bar
                (constructor posn 'bar (first connection-points) #t)]
               [(equal? (next-orientation (first connection-points))
                        (second connection-points)) ; 'elbow
                (constructor posn 'elbow (first connection-points) #t)]
               [else ; also 'elbow
                (constructor posn 'elbow (second connection-points) #t)])]
        [(= (length edges) 3)
         (define connection-points
           (connection-points-from-edges edges posn))
         (constructor posn
                      'tee
                      (opposite-orientation
                       (first (memf (λ (e)
                                      (not (member e connection-points)))
                                    '(north south east west))))
                      #t)]
        [else (constructor posn 'cross 'north #t)]))


;; connection-points-from-edges : [Listof Edge] Posn -> [Listof Orientation]
;; Determine the connection points for the given Posn given all the Edges that
;; connect to it
(define (connection-points-from-edges edges posn)
  (cond [(empty? edges) '()]
        [else (cons (posn-neighbor? posn
                                    (if (equal? (edge-to (first edges)) posn)
                                        (edge-from (first edges))
                                        (edge-to (first edges))))
                    (connection-points-from-edges (rest edges) posn))]))


;; scramble : Board -> Board
;; Rotates the pieces of a board
(define (scramble b)
  (define new-board b)
  (for ([c (board-cells b)])
    (define original-orientation (cell-orientation c))
    (scramble-cell! c new-board)
    (set! new-board (if (symbol=? original-orientation (cell-orientation c))
                        new-board
                        (board (board-cells new-board)
                               (board-size new-board)
                               (board-turned-cell new-board)
                               (board-turns new-board)
                               (add1 (board-perfect new-board))))))
  (set-board-turns! new-board 0)
  (set-board-turned-cell! new-board (posn -1 -1))
  (update-connected! (board-cells new-board))
  new-board)


;; scramble-cell! : Cell Board -> Void
;; Rotates the cell a random number of times clockwise and sets turns to -1
;; Only occurs at board generation, therefore any cells being scrambled in this
;; way will need to be turned to get the solution.
(define (scramble-cell! c board)
  (for ([i (range (random 4))])
    (rotate-cell! (cell-posn c) board))) ; previous-turn doesn't matter here



;; =============================================================================
;;; CELLS
;; =============================================================================

;; update-connected! : [Listof Cell] -> Void
;; Updates the cell-connected field if the cell is connected to the power source
(define (update-connected! cells)
  (define power-source (findf power-source? cells))
  (for ([c cells])
    (set-cell-connected! c #f))
  ;; Uses a breadth-first-search approach to alter the connected feild of all
  ;; cells connected to the power source
  (define (bfs! cells pivot)
    (define pivot-neighbors (connected-neighbors pivot cells))
    (cond [(empty? pivot-neighbors) (void)]
          [else
           (set-cell-connected! pivot #t)
           (for ([c pivot-neighbors])
             (set-cell-connected! c #t)
             (bfs! (remove pivot cells) c))]))
  (if power-source
      (bfs! cells power-source)
      (void)))


;; get-neighbors: Cell [Listof Cell] -> [Listof Cell]
;; Returns the up to four direct neighbors of the given cell
(define (get-neighbors cell cells)
  (filter (λ (c)
            (or (and (= 1 (abs (- (posn-x (cell-posn cell))
                                  (posn-x (cell-posn c)))))
                     (= (posn-y (cell-posn cell)) (posn-y (cell-posn c))))
                (and (= 1 (abs (- (posn-y (cell-posn cell))
                                  (posn-y (cell-posn c)))))
                     (= (posn-x (cell-posn cell)) (posn-x (cell-posn c))))))
          cells))


;; is-neighbor? : Cell Cell -> Orientation-or-False
;; Determines if cell2 is directly north, south, east, or west of cell1, or
;; false if they are not neighbors
(define (is-neighbor? cell1 cell2)
  (posn-neighbor? (cell-posn cell1) (cell-posn cell2)))


;; connected-neighbors : Cell [Listof Cell] -> [Listof Cell]
;; Returns all the neighbors connected to this cell
(define (connected-neighbors cell cells)
  (filter (λ (c) (connected? cell c)) (get-neighbors cell cells)))


;; connected? : Cell Cell -> Boolean
;; Determines if the two cells are connected
(define (connected? c1 c2)
  (define cp1 (connection-points c1))
  (define cp2 (connection-points c2))
  (define is-n (is-neighbor? c1 c2))
  (cond [(symbol=? is-n 'north) (and (member 'north cp1)
                                     (member 'south cp2) #t)] ; extra #t to make
        [(symbol=? is-n 'east) (and (member 'east cp1)        ; 'and' return #t 
                                    (member 'west cp2) #t)]
        [(symbol=? is-n 'south) (and (member 'south cp1)
                                     (member 'north cp2) #t)]
        [(symbol=? is-n 'west) (and (member 'west cp1)
                                    (member 'east cp2) #t)]
        [else #f]))


;; rotate-cell! : Posn Board -> Void
;; Rotates the cell at position p once clockwise, update number of turns in
;; board if needed
(define (rotate-cell! posn board)
  (define c (cell-at posn (board-cells board)))
  (define previous-turn (board-turned-cell board))
  (cond [(symbol=? 'cross (cell-type c)) c] ; 'cross does not rotate
        [(symbol=? 'bar (cell-type c)) ; 'bar will only be 'north or 'east
         (cond [(symbol=? (next-orientation (cell-orientation c)) 'north)
                (set-cell-orientation! c 'north)]
               [(symbol=? (next-orientation (cell-orientation c)) 'east)
                (set-cell-orientation! c 'east)]
               [(symbol=? (next-orientation (cell-orientation c)) 'south)
                (set-cell-orientation! c 'north)]
               [else ; 'west
                (set-cell-orientation! c 'east)])
         (unless (equal? (cell-posn c) previous-turn)
           (set-board-turns! board (add1 (board-turns board))))]
        [else 
         (set-cell-orientation! c (next-orientation (cell-orientation c)))
         (unless (equal? (cell-posn c) previous-turn)
           (set-board-turns! board (add1 (board-turns board))))])
  (set-board-turned-cell! board (cell-posn c)))



;; =============================================================================
;;; UTILS
;; =============================================================================

;; posn-neighbor? : Posn Posn -> [Maybe Orientation]
;; Determines if p2 is directly north, south, east, or west of p1, or
;; false if they are not neighbors
(define (posn-neighbor? p1 p2)
  (define x1 (posn-x p1))
  (define y1 (posn-y p1))
  (define x2 (posn-x p2))
  (define y2 (posn-y p2))
  (cond [(= 1 (- y1 y2)) 'north]
        [(= -1 (- y1 y2)) 'south]
        [(= 1 (- x1 x2)) 'west]
        [(= -1 (- x1 x2)) 'east]
        [else #f]))


;; cell-at : Posn [Listof Cell] -> [Maybe Cell]
;; Returns the cell at the given coordinates or #f if there is none 
(define (cell-at p cells)
  (findf (λ (c) (equal? (cell-posn c) p)) cells))


;; opposite-orientation : Orientation -> Orientation
;; Determines the opposite of the given orientation
(define (opposite-orientation o)
  (cond [(symbol=? o 'north) 'south]
        [(symbol=? o 'east) 'west]
        [(symbol=? o 'south) 'north]
        [(symbol=? o 'west) 'east]))


;; next-orientation : Orientation -> Orientation
;; Determines the orientation that would result from a clockwise rotation
(define (next-orientation o)
  (cond [(symbol=? o 'north) 'east]
        [(symbol=? o 'east) 'south]
        [(symbol=? o 'south) 'west]
        [(symbol=? o 'west) 'north]))


;; connection-points : Cell -> [Listof Orientation]
;; Determines which directions there are open connections for this cell
(define (connection-points cell)
  (define t (cell-type cell))
  (cond [(symbol=? t 'end) (list (cell-orientation cell))]
        [(symbol=? t 'elbow) (elbow-connection-points (cell-orientation cell))]
        [(symbol=? t 'bar) (bar-connection-points (cell-orientation cell))]
        [(symbol=? t 'tee) (tee-connection-points (cell-orientation cell))]
        [(symbol=? t 'cross) (list 'north 'east 'south 'west)]))


;; elbow-connection-points : Orientation -> [Listof Orientation]
;; Determines which directions are open connections for an elbow in the given
;; orientation
(define (elbow-connection-points o)
  (cond [(symbol=? o 'north) (list 'north 'east)]
        [(symbol=? o 'east) (list 'east 'south)]
        [(symbol=? o 'south) (list 'south 'west)]
        [(symbol=? o 'west) (list 'north 'west)]))


;; bar-connection-points : Orientation -> [Listof Orientation]
;; Determines which directions are open connections for an elbow in the given
;; orientation
(define (bar-connection-points o)
  (cond [(or (symbol=? o 'north) (symbol=? o 'south))
         (list 'north 'south)]
        [else (list 'east 'west)]))


;; tee-connection-points : Orientation -> [Listof Orientation]
;; Determines which directions are open connections for an tee in the given
;; orientation
(define (tee-connection-points o)
  (remove (opposite-orientation o) (list 'north 'east 'south 'west)))




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
  
  (define 4x4-board-cycle (board (list (cell (posn 0 0) 'end 'south #t)
                                       (cell (posn 1 0) 'elbow 'east #t)
                                       (cell (posn 2 0) 'elbow 'south #t)
                                       (cell (posn 3 0) 'end 'south #f)
                                       (cell (posn 0 1) 'elbow 'north #t)
                                       (power-source
                                        (posn 1 1) 'cross 'north #t)
                                       (cell (posn 2 1) 'tee 'south #t)
                                       (cell (posn 3 1) 'end 'north #f)
                                       (cell (posn 0 2) 'bar 'east #f)
                                       (cell (posn 1 2) 'elbow 'north #t)
                                       (cell (posn 2 2) 'tee 'north #t)
                                       (cell (posn 3 2) 'elbow 'south #t)
                                       (cell (posn 0 3) 'elbow 'west #f)
                                       (cell (posn 1 3) 'end 'east #f)
                                       (cell (posn 2 3) 'end 'west #f)
                                       (cell (posn 3 3) 'end 'north #t))
                                 4 (posn 2 3) 1 11))
  
  ;; Tests for user input
  
  ;; Will turn a cell and update cell's turn count
  (check-equal? (let ([b (board (list (power-source
                                       (posn 0 0) 'end 'north #t)
                                      (cell (posn 1 0) 'elbow 'south #f)
                                      (cell (posn 0 1) 'end 'east #f)
                                      (cell (posn 1 1) 'elbow 'west #f))
                                2 (posn -1 -1) 0 0)])
                  (mouse-handler b 0 0 "button-down")
                  b)
                (board (board-cells 2x2-board) 2 (posn 0 0) 1 0))
  ;; Will rotate a cell without updating turn count
  (check-equal? (let ([b (board (list (power-source (posn 0 0) 'end 'east #t)
                                      (cell (posn 1 0) 'elbow 'south #f)
                                      (cell (posn 0 1) 'end 'north #t)
                                      (cell (posn 1 1) 'elbow 'west #t))
                                2 (posn 0 1) 1 1)])
                  (mouse-handler b 0 1 "button-down")
                  b)
                (board (list (power-source (posn 0 0) 'end 'east #t)
                                      (cell (posn 1 0) 'elbow 'south #t)
                                      (cell (posn 0 1) 'end 'east #t)
                                      (cell (posn 1 1) 'elbow 'west #t))
                       2 (posn 0 1) 1 1))
  ;; Won't rotate a cell if the board is all connected
  (check-equal? (let ([b (board (list (power-source (posn 0 0) 'end 'east #t)
                                      (cell (posn 1 0) 'elbow 'south #t)
                                      (cell (posn 0 1) 'end 'north #t)
                                      (cell (posn 1 1) 'elbow 'west #t))
                                2 (posn 0 1) 1 1)])
                  (mouse-handler b 0 1 "button-down")
                  b)
                (board (list (power-source (posn 0 0) 'end 'east #t)
                                      (cell (posn 1 0) 'elbow 'south #t)
                                      (cell (posn 0 1) 'end 'north #t)
                                      (cell (posn 1 1) 'elbow 'west #t))
                       2 (posn 0 1) 1 1))
  ;; Won't rotate a cell out of bounds
  (check-equal? (let ([b 2x2-board])
                  (mouse-handler b 100 100 "button-down")
                  b)
                2x2-board)
  ;; Won't rotate a cell if its not button-down
  (check-equal? (let ([b 2x2-board])
                  (mouse-handler b 10 10 "button-up")
                  b)
                2x2-board)

  (check-equal? (key-handler 2x2-board "x") 2x2-board)
  (check-not-equal? (key-handler 2x2-board "n") 2x2-board)
  (check-equal? (board-size (key-handler 2x2-board "n")) 2)

  ;; Tests on board operations
  (check-true (all-connected? 3x3-board-conn))
  (check-false (all-connected? 3x3-board-discon))
              

  ;; Tests for board generation 
  (check-equal? (length (assign-edges 2)) 4)
  (check-equal? (length (assign-edges 4)) 24)
  (check-equal? (kruskals (list (edge 0 (posn 0 1) (posn 1 1))
                                (edge 1 (posn 0 0) (posn 0 1))
                                (edge 2 (posn 1 0) (posn 0 0))
                                (edge 3 (posn 0 0) (posn 1 0))) 2)
                (list (edge 2 (posn 1 0) (posn 0 0))
                      (edge 1 (posn 0 0) (posn 0 1))
                      (edge 0 (posn 0 1) (posn 1 1))))
  (check-equal? (kruskals (list (edge 0 (posn 0 0) (posn 1 0))
                                (edge 1 (posn 0 0) (posn 0 1))
                                (edge 2 (posn 1 0) (posn 1 1))
                                (edge 3 (posn 0 1) (posn 1 1))
                                (edge 4 (posn 1 0) (posn 2 0))
                                (edge 5 (posn 2 0) (posn 2 1))
                                (edge 6 (posn 1 1) (posn 2 1))
                                (edge 7 (posn 0 1) (posn 0 2))
                                (edge 8 (posn 1 1) (posn 1 2))
                                (edge 9 (posn 2 1) (posn 2 2))
                                (edge 10 (posn 0 2) (posn 1 2))
                                (edge 11 (posn 1 2) (posn 2 2))) 3)
                (reverse (list (edge 0 (posn 0 0) (posn 1 0))
                               (edge 1 (posn 0 0) (posn 0 1))
                               (edge 2 (posn 1 0) (posn 1 1))
                               (edge 4 (posn 1 0) (posn 2 0))
                               (edge 5 (posn 2 0) (posn 2 1))
                               (edge 7 (posn 0 1) (posn 0 2))
                               (edge 8 (posn 1 1) (posn 1 2))
                               (edge 9 (posn 2 1) (posn 2 2)))))
  (check-equal?
   (edges->cell (list (edge 7 (posn 0 1) (posn 0 2))) (posn 0 2) cell)
   (cell (posn 0 2) 'end 'north #t))
  (check-equal? (edges->cell (list (edge 1 (posn 0 0) (posn 0 1))
                                   (edge 7 (posn 0 1) (posn 0 2)))
                             (posn 0 1) power-source)
                (power-source (posn 0 1) 'bar 'north #t))
  (check-equal? (edges->cell (list (edge 0 (posn 0 0) (posn 1 0))
                                  (edge 1 (posn 0 0) (posn 0 1)))
                            (posn 0 0) cell)
                (cell (posn 0 0) 'elbow 'east #t))
  (check-equal? (edges->cell (list (edge 9 (posn 2 1) (posn 2 2))
                                  (edge 11 (posn 1 2) (posn 2 2)))
                            (posn 2 2) cell)
                (cell (posn 2 2) 'elbow 'west #t))
  (check-equal? (edges->cell (list (edge 0 (posn 0 0) (posn 1 0))
                                  (edge 2 (posn 1 0) (posn 1 1))
                                  (edge 4 (posn 1 0) (posn 2 0)))
                            (posn 1 0) cell)
                (cell (posn 1 0) 'tee 'south #t))
  (check-equal? (edges->cell (list (edge 2 (posn 1 0) (posn 1 1))
                                  (edge 3 (posn 0 1) (posn 1 1))
                                  (edge 6 (posn 1 1) (posn 2 1))
                                  (edge 8 (posn 1 1) (posn 1 2)))
                            (posn 1 1) cell)
                (cell (posn 1 1) 'cross 'north #t))

  (check-equal? (length (edges->cells (list (edge 0 (posn 0 0) (posn 1 0))
                                            (edge 1 (posn 0 0) (posn 0 1))
                                            (edge 2 (posn 1 0) (posn 1 1))
                                            (edge 4 (posn 1 0) (posn 2 0))
                                            (edge 5 (posn 2 0) (posn 2 1))
                                            (edge 7 (posn 0 1) (posn 0 2))
                                            (edge 8 (posn 1 1) (posn 1 2))
                                            (edge 9 (posn 2 1) (posn 2 2))) 3))
                        9)
  (check-true (andmap (λ (c) (cell? c))
                      (edges->cells (list (edge 0 (posn 0 0) (posn 1 0))
                                          (edge 1 (posn 0 0) (posn 0 1))
                                          (edge 2 (posn 1 0) (posn 1 1))
                                          (edge 4 (posn 1 0) (posn 2 0))
                                          (edge 5 (posn 2 0) (posn 2 1))
                                          (edge 7 (posn 0 1) (posn 0 2))
                                          (edge 8 (posn 1 1) (posn 1 2))
                                          (edge 9 (posn 2 1) (posn 2 2))) 3)))
  (check-true
   (let ([b (scramble (board (list (cell (posn 0 0) 'elbow 'north #f)
                         (cell (posn 1 0) 'bar 'north #t)
                         (cell (posn 2 0) 'elbow 'north #f)
                         (cell (posn 0 1) 'tee 'north #f)
                         (power-source (posn 1 1) 'elbow 'north #t)
                         (cell (posn 2 1) 'end 'north #f)
                         (cell (posn 0 2) 'end 'north #f)
                         (cell (posn 1 2) 'elbow 'north #f)
                         (cell (posn 2 2) 'end 'north #f))
                   3 (posn -1 -1) 0 0))])
   (equal? (foldr (λ (cell total)
                    (+ total (if (symbol=? (cell-orientation cell) 'north)
                                 0
                                 1)))
                  0
                  (board-cells b))
           (board-perfect b))))
  
  ;; Tests on cell operations
  (check-equal? (get-neighbors (power-source (posn 1 1) 'elbow 'north #t)
                               (board-cells 3x3-board-discon))
                (list (cell (posn 1 0) 'bar 'north #t)
                      (cell (posn 0 1) 'tee 'north #f)               
                      (cell (posn 2 1) 'end 'north #f)
                      (cell (posn 1 2) 'elbow 'north #f)))
  (check-equal? (get-neighbors (cell (posn 0 3) 'elbow 'west #t)
                               (board-cells 4x4-board-cycle))
                (list (cell (posn 0 2) 'bar 'east #f)
                      (cell (posn 1 3) 'end 'east #f)))
  
  (check-equal? (is-neighbor? (power-source (posn 1 1) 'elbow 'north #t)
                              (cell (posn 1 0) 'bar 'north #t))
                'north)
  (check-equal? (is-neighbor? (cell (posn 2 0) 'elbow 'north #f)
                              (cell (posn 2 1) 'end 'north #f))
                'south)
  (check-false (is-neighbor? (cell (posn 2 2) 'end 'north #f)
                             (cell (posn 0 0) 'elbow 'north #f)))
  
  (check-equal? (connected? (power-source (posn 1 1) 'elbow 'north #t)
                            (cell (posn 2 1) 'tee 'north #f))
                #t)
  (check-equal? (connected? (power-source (posn 1 1) 'elbow 'north #t)
                            (cell (posn 0 1) 'tee 'north #f))
                #f)
  (check-equal? (connected? (cell (posn 0 0) 'elbow 'north #f)
                            (cell (posn 1 0) 'bar 'north #t))
                #f)
  
  (check-equal? (let ([b (list (cell (posn 0 0) 'elbow 'north #t)
                               (cell (posn 1 0) 'bar 'north #f)
                               (cell (posn 2 0) 'elbow 'north #f)
                               (cell (posn 0 1) 'tee 'north #f)
                               (power-source (posn 1 1) 'elbow 'north #t)
                               (cell (posn 2 1) 'end 'north #f)
                               (cell (posn 0 2) 'end 'north #f)
                               (cell (posn 1 2) 'elbow 'north #f)
                               (cell (posn 2 2) 'end 'north #f))])
                  (update-connected! b)
                  b)
                (board-cells 3x3-board-discon))
  ;; Test to determine that it won't get stuck in an infinate loop if there is
  ;; a cycle on the board
  (check-equal? (let ([b (list (cell (posn 0 0) 'end 'south #t)
                               (cell (posn 1 0) 'elbow 'east #t)
                               (cell (posn 2 0) 'elbow 'south #t)
                               (cell (posn 3 0) 'end 'south #f)
                               (cell (posn 0 1) 'elbow 'north #t)
                               (power-source (posn 1 1) 'cross 'north #t)
                               (cell (posn 2 1) 'tee 'south #f)
                               (cell (posn 3 1) 'end 'north #f)
                               (cell (posn 0 2) 'bar 'east #f)
                               (cell (posn 1 2) 'elbow 'north #f)
                               (cell (posn 2 2) 'tee 'north #f)
                               (cell (posn 3 2) 'elbow 'south #t)
                               (cell (posn 0 3) 'elbow 'west #f)
                               (cell (posn 1 3) 'end 'east #f)
                               (cell (posn 2 3) 'end 'west #f)
                               (cell (posn 3 3) 'end 'north #t))])
                  (update-connected! b)
                  b)
                (board-cells 4x4-board-cycle))
  
  (check-equal? (let ([b (board (list (power-source (posn 0 0) 'end 'east #t)
                               (cell (posn 1 0) 'elbow 'south #t)
                               (cell (posn 0 1) 'end 'east #t)
                               (cell (posn 1 1) 'elbow 'west #t))
                         2 (posn -1 -1) 0 0)])
                  (rotate-cell! (posn 0 0) b)
                  b)
                (board (list (power-source (posn 0 0) 'end 'south #t)
                               (cell (posn 1 0) 'elbow 'south #t)
                               (cell (posn 0 1) 'end 'east #t)
                               (cell (posn 1 1) 'elbow 'west #t))
                         2 (posn 0 0) 1 0))
  (check-equal? (let ([b (board (list (power-source (posn 0 0) 'end 'east #t)
                                      (cell (posn 1 0) 'elbow 'south #t)
                                      (cell (posn 0 1) 'end 'east #t)
                                      (cell (posn 1 1) 'elbow 'west #t))
                         2 (posn 1 1) 1 0)])
                  (rotate-cell! (posn 1 1) b)
                  b)
                (board (list (power-source (posn 0 0) 'end 'east #t)
                                      (cell (posn 1 0) 'elbow 'south #t)
                                      (cell (posn 0 1) 'end 'east #t)
                                      (cell (posn 1 1) 'elbow 'north #t))
                         2 (posn 1 1) 1 0))

  (check-false (let ([b (board (list (cell (posn 0 0) 'elbow 'east #t)
                                    (cell (posn 1 0) 'bar 'east #t)
                                    (cell (posn 2 0) 'elbow 'south #t)
                                    (cell (posn 0 1) 'tee 'east #t)
                                    (power-source (posn 1 1) 'elbow 'south #t)
                                    (cell (posn 2 1) 'end 'north #t)
                                    (cell (posn 0 2) 'end 'north #t)
                                    (cell (posn 1 2) 'elbow 'north #t)
                                    (cell (posn 2 2) 'end 'west #t))
                              3 (posn -1 -1) 0 0)])
                (scramble b)
                (all-connected? b))) ;; Statistically unlikely to fail
 
  
  ;; Tests for utils
  (check-equal? (cell-at (posn 0 0) (board-cells 2x2-board))
                (power-source (posn 0 0) 'end 'east #t))
  (check-equal? (cell-at (posn 3 3) (board-cells 2x2-board))
                #f)
  
  (check-equal? (opposite-orientation 'north) 'south)
  (check-equal? (opposite-orientation 'east) 'west)
  
  (check-equal? (next-orientation 'north) 'east)
  (check-equal? (next-orientation 'west) 'north)
  
  (check-equal? (connection-points
                 (power-source (posn 1 1) 'elbow 'north #t))
                (list 'north 'east))
  (check-equal? (connection-points (cell (posn 2 0) 'elbow 'south #t))
                (list 'south 'west))
  (check-equal? (connection-points (cell (posn 0 0) 'tee 'west #f))
                (list 'north 'south 'west))
  (check-equal? (connection-points (cell (posn 1 0) 'bar 'north #t))
                (list 'north 'south))
  (check-equal? (connection-points (cell (posn 0 0) 'end 'west #f))
                (list 'west))
  (check-equal? (connection-points (cell (posn 2 2) 'cross 'south #t))
                (list 'north 'east 'south 'west))
  
  "all tests run")