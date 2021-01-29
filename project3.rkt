
#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

;; We'll use the same version of Some and Optional that we used on Homework 5.
(define-struct (Some X)
  ([value : X]))

(define-type (Optional X)
  (U 'none (Some X)))

;; The game has two players, who we'll call 'black and 'white. You can choose
;; any color or pattern you would like for the pieces, but we'll still call
;; the players by these names.
(define-type Player (U 'empty 'black 'white))

;; (Pos row col) represents a position on the game board. The lower-left corner
;; is at row 0, column 0. The lower-right corner is at row 0, column 6. The
;; upper-left corner is at row 5, column 0. The upper-right corner is at row 5,
;; column 6.
(define-struct Pos
  ([row : Integer]   ;; integer between 0 and 5 inclusive
   [col : Integer])) ;; integer between 0 and 6 inclusive

;; (Stack height pieces) represents one full column of the game board. The
;; integer height is the number of pieces currently in that column. The
;; list pieces is a list of the pieces in the column (or more precisely, a list
;; of the players who placed the pieces). The last element of the list is the
;; bottom piece in the stack, while the first element of the list is the top
;; piece in the stack (so far). The value of height should alway match the
;; length of the list pieces.
(define-struct Stack
  ([height : Integer]
   [pieces : (Listof Player)]))

;; (Board stacks) represents a game board. The list stacks will always have
;; seven elements, representing the seven columns of the game board. The first
;; element of stacks represents the leftmost column of the board.
(define-struct Board
  ([stacks : (Listof Stack)]))

;; (Game board next) represents the state of the game at a given moment of time.
;; The current state of the game board is saved in board. The player whose turn
;; it is currently is stored in next.
(define-struct Game
  ([board : Board]
   [next : Player]))

;; If a player has won the game by creating a line of four, then
;; (Winning-Line player start end) can be used to keep track of which player
;; created the line of four, and where that line is (it goes from start to end).
;; Generally, if a player's winning move creates more than one line of four, we
;; won't care which one gets represented.
(define-struct Winning-Line
  ([player : Player]
   [start : Pos]
   [end : Pos]))

(: new-game : Game)
;; game representing empty board with 'black as next player
(define new-game (Game (Board (list (Stack 0 '()) (Stack 0 '()) (Stack 0 '())
                                    (Stack 0 '()) (Stack 0 '()) (Stack 0 '())
                                    (Stack 0 '())))
                       'black))

;; A type of a function which takes in the current game and returns a column
;; to play in as the next move
(define-type Strategy (Game -> Integer))

(: always-choose : Integer -> Strategy)
(define (always-choose col)
  (lambda ([game : Game]) col))

(: always-choose-center : Strategy)
(define always-choose-center (always-choose 3))

(: first-available : Strategy)
;; A strategy which picks the first column from the left that isn't full
(define (first-available g)
  (local
    {(: first-stack : (Listof Stack) Integer -> Integer)
     ;; Takes in a list of stacks and returns the index for the first stack
     ;; which is non-empty
     (define (first-stack lst i)
       (cond
         [(= i 7) (error "board is full")]
         [(< (Stack-height (list-ref lst i)) 6) i]
         [else (first-stack lst (+ i 1))]))}
    (match g
      [(Game brd pl)
       (first-stack (Board-stacks brd) 0)])))

;; Stacks/boards for testing
(define a (Stack 3 (list 'black 'white 'black)))
(define b (Stack 5 (list 'black 'black 'black 'white 'black)))
(define c (Stack 1 (list 'white)))
(define d (Stack 3 (list 'white 'white 'black)))
(define e (Stack 0 '()))
(define f (Stack 3 (list 'black 'white 'black)))
(define g (Stack 6 (list 'black 'white 'black 'black 'white 'black)))
(define d1 (Stack 4 (list 'black 'white 'white 'black)))
(define d2 (Stack 5 (list 'black 'white 'white 'white 'white)))
(define b1 (Stack 6 (list 'white 'black 'black 'black 'white 'black)))

(define random (Board (list a b c d e f g)))
(define random1 (Board (list a b c d1 e f g)))
(define random2 (Board (list a b c d2 e f g)))
(define random3 (Board (list a b1 c d e f g)))

(define game1 (Game (Board (list g g g g b a c)) 'black))
(define game2 (Game (Board (list d c b a g d c)) 'black))
(define game3 (Game (Board (list g g g g g g g)) 'black))
(define game4 (Game (Board (list e e e c e e e)) 'black))

(check-expect (first-available game1) 4)
(check-expect (first-available game2) 0)
(check-error (first-available game3) "board is full")

(: get-piece : Integer Integer Board -> Player)
;; takes in the row and column of a slot and returns the player in that slot
(define (get-piece r c brd)
  (match brd
    [(Board stacks)
     (list-ref (Stack-pieces (list-ref stacks c))
               (- (Stack-height (list-ref stacks c)) r 1))]))

(check-expect (get-piece 2 3 random) 'white)
(check-expect (get-piece 5 6 random) 'black)

(: filled? : Integer Integer Board -> Boolean)
;; takes in the row and column of a slot, the board, and returns true if the
;; slot is filled with a piece from either player
(define (filled? r c brd)
  (< r (Stack-height (list-ref (Board-stacks brd) c))))

(check-expect (filled? 2 1 random) #t)
(check-expect (filled? 0 4 random) #f)

(: board-ref : Board Pos -> (Optional Player))
;; returns (Some pl) if there is a piece belonging to player pl at position,
;; and 'none otherwise
(define (board-ref brd position)
  (match position
    [(Pos r c)
     (if (filled? r c brd)
         (Some (get-piece r c brd))
         'none)]))

(check-expect (board-ref random (Pos 2 1)) (Some 'black))
(check-expect (board-ref random (Pos 2 2)) 'none)
(check-expect (board-ref random (Pos 1 6)) (Some 'white))

(: valid-move? : Game Player Integer -> Boolean)
;; takes in a game, player, and column number, and returns true if it is that
;; players turn, the column is not full, and the column number is valid
(define (valid-move? g pl c)
  (match g
    [(Game (Board stacks) next)
     (and (symbol=? next pl)
          (and (>= c 0) (<= c 6))
          (< (Stack-height (list-ref stacks c)) 6))]))

(check-expect (valid-move? (Game random 'black) 'black 3) #t)
(check-expect (valid-move? (Game random 'black) 'white 2) #f)
(check-expect (valid-move? (Game random 'black) 'black 8) #f)
(check-expect (valid-move? (Game random 'black) 'black 6) #f)

(: replace-col : (Listof Stack) Stack Integer Integer -> (Listof Stack))
;; takes in a stack stk, a column c, a list of stacks, and an index 0, and
;; returns the list of stacks with column c replaced with stk
(define (replace-col stacks stk c i)
  (match stacks
    [(cons x xr)
     (if (= c i)
         (cons stk (replace-col xr stk c (+ i 1)))
         (cons x (replace-col xr stk c (+ i 1))))]
    [_ '()]))

(check-expect (replace-col (list a b c d e f g) d1 3 0) (list a b c d1 e f g))


(: update-col : Stack Player -> Stack)
;; takes in a stack and the player who is making a move and returns the
;; updated stack with the new list and height + 1
(define (update-col stk p)
  (match stk
    [(Stack height lst)
     (Stack (+ height 1) ((inst cons Player) p (Stack-pieces stk)))]))

(check-expect (update-col b 'white)
              (Stack 6 (list 'white 'black 'black 'black 'white 'black)))
(check-expect (update-col e 'black) (Stack 1 (list 'black))) 

(: switch : Player -> Player)
;; returns 'black if input is 'white, 'white if input is 'black
(define (switch pl)
  (if (symbol=? pl 'black)
      'white
      'black))

(check-expect (switch 'black) 'white)
(check-expect (switch 'white) 'black)

(: apply-move : Game Player Integer -> Game)
;; takes in a game, player, and column number, and updates the game state
;; if the move is valid and returns the same game if not
(define (apply-move g pl c)
  (if (not (valid-move? g pl c))
      g
      (match g
        [(Game (Board stacks) _)
         (Game (Board
                (replace-col stacks (update-col (list-ref stacks c) pl) c 0))
               (switch pl))])))

(check-expect (apply-move (Game random 'black) 'black 3) (Game random1 'white))
(check-expect (apply-move (Game random 'white) 'white 1) (Game random3 'black))


(: valid-index? : Integer Integer -> Boolean)
;; returns true if the row and column index are valid
(define (valid-index? r c)
  (and (>= r 0) (<= r 5) (>= c 0) (<= c 6)))

(check-expect (valid-index? 1 -1) #f)
(check-expect (valid-index? 1 3) #t)


;; check1, check2, check3, check4 each take in the position of the piece and the
;; board, and return true if the 3 slots that form a line in the 4 directions
;; pointing away from the given piece form a winning line, and return false if
;; there are less than 3 slots in that direction or if any of the 3 slots are
;; empty.
;; check1 checks the 3 pieces directly above, check2 diagonally going to the
;; top right, check3 to the right, check4 diagonally going to the bottom left

(: check1 : Pos Board -> Boolean)
(define (check1 p brd)
  (match p
    [(Pos r c)
     (and
      (and (valid-index? (+ r 1) c) (filled? (+ r 1) c brd)
           (valid-index? (+ r 2) c) (filled? (+ r 2) c brd)
           (valid-index? (+ r 3) c) (filled? (+ r 3) c brd))             
      (local
        {(: piece : Player)
         ;; the player whose piece is in position p
         (define piece (get-piece r c brd))}
        (and (symbol=? piece (get-piece (+ r 1) c brd))
             (symbol=? piece (get-piece (+ r 2) c brd))
             (symbol=? piece (get-piece (+ r 3) c brd)))))]))
         
(check-expect (check1 (Pos 2 3) random) #f)
(check-expect (check1 (Pos 2 6) random) #f)
(check-expect (check1 (Pos 0 3) random2) #t)


(: check2 : Pos Board -> Boolean)
(define (check2 p brd)
  (match p
    [(Pos r c)
     (and
      (and (valid-index? (+ r 1) (+ c 1)) (filled? (+ r 1) (+ c 1) brd)
           (valid-index? (+ r 2) (+ c 2)) (filled? (+ r 2) (+ c 2) brd)
           (valid-index? (+ r 3) (+ c 3)) (filled? (+ r 3) (+ c 3) brd))             
      (local
        {(: piece : Player)
         ;; the player whose piece is in position p
         (define piece (get-piece r c brd))}
        (and (symbol=? piece (get-piece (+ r 1) (+ c 1) brd))
             (symbol=? piece (get-piece (+ r 2) (+ c 2) brd))
             (symbol=? piece (get-piece (+ r 3) (+ c 3) brd)))))]))

(define a2 (Stack 1 (list 'white)))
(define b2 (Stack 2 (list 'white 'black)))
(define c2 (Stack 3 (list 'white 'black 'black)))
(define g2 (Stack 4 (list 'white 'white 'black 'black)))
(define check2-board (Board (list a2 b2 c2 g2 e e e)))
(check-expect (check2 (Pos 3 1) random) #f)
(check-expect (check2 (Pos 0 0) check2-board) #t)


(: check3 : Pos Board -> Boolean)
(define (check3 p brd)
  (match p
    [(Pos r c)
     (and
      (and (valid-index? r (+ c 1)) (filled? r (+ c 1) brd)
           (valid-index? r (+ c 2)) (filled? r (+ c 2) brd)
           (valid-index? r (+ c 3)) (filled? r (+ c 3) brd))             
      (local
        {(: piece : Player)
         ;; the player whose piece is in position p
         (define piece (get-piece r c brd))}
        (and (symbol=? piece (get-piece r (+ c 1) brd))
             (symbol=? piece (get-piece r (+ c 2) brd))
             (symbol=? piece (get-piece r (+ c 3) brd)))))]))

(define a3 (Stack 1 (list 'white)))
(define b3 (Stack 1 (list 'white)))
(define c3 (Stack 1 (list 'white)))
(define g3 (Stack 1 (list 'white)))
(define check3-board (Board (list a3 b3 c3 g3 e e e)))
(check-expect (check3 (Pos 3 1) random) #f)
(check-expect (check3 (Pos 0 0) check3-board) #t)


(: check4 : Pos Board -> Boolean)
(define (check4 p brd)
  (match p
    [(Pos r c)
     (and
      (and (valid-index? (- r 1) (+ c 1)) (filled? (- r 1) (+ c 1) brd)
           (valid-index? (- r 2) (+ c 2)) (filled? (- r 2) (+ c 2) brd)
           (valid-index? (- r 3) (+ c 3)) (filled? (- r 3) (+ c 3) brd))             
      (local
        {(: piece : Player)
         ;; the player whose piece is in position p
         (define piece (get-piece r c brd))}
        (and (symbol=? piece (get-piece (- r 1) (+ c 1) brd))
             (symbol=? piece (get-piece (- r 2) (+ c 2) brd))
             (symbol=? piece (get-piece (- r 3) (+ c 3) brd)))))]))

(define a4 (Stack 4 (list 'white 'black 'white 'black)))
(define b4 (Stack 3 (list 'white 'black 'white)))
(define c4 (Stack 2 (list 'white 'white)))
(define g4 (Stack 1 (list 'white)))
(define check4-board (Board (list a4 b4 c4 g4 e e e)))
(check-expect (check4 (Pos 3 1) random) #f)
(check-expect (check4 (Pos 3 0) check4-board) #t)

(: check-pos : Pos Board -> Integer)
;; takes in a position p and the board and returns the direction (1-4) of the 
;; winning-line if a winning line starts at p, -1 if the slot is empty,
;; and -2 if the slot is full but no winning-line starts from p
(define (check-pos p brd)
  (match p
    [(Pos r c)
     (cond
       [(or (not (filled? r c brd)) (symbol=? 'empty (get-piece r c brd))) -1]
       [(check1 p brd) 1]
       [(check2 p brd) 2]
       [(check3 p brd) 3]
       [(check4 p brd) 4]
       [else -2])]))

(check-expect (check-pos (Pos 0 3) random2) 1)
(check-expect (check-pos (Pos 1 4) random) -1)
(check-expect (check-pos (Pos 1 3) random) -2)


(: end-pos : Integer Integer Integer -> Pos)
;; Takes in the row and column of the piece and the direction of the
;; winning-line and returns the end position of the line
;; As before, direction 1 is top, 2 top-right, 3 right, 4 bottom-right
(define (end-pos row col dir)
  (local
    {(define dir-map
       (list (Pos (+ row 3) col) (Pos (+ row 3) (+ col 3)) 
             (Pos row (+ col 3)) (Pos (- row 3) (+ col 3))))}
    (list-ref dir-map (- dir 1))))

(check-expect (end-pos 2 4 1) (Pos 5 4))
(check-expect (end-pos 3 1 4) (Pos 0 4))
 

(: check-win : Board Integer Integer Boolean -> (U Winning-Line 'tie 'ongoing))
;; Takes in a board brd, indexes r and c starting at 0 to keep track of the 
;; current piece being checked, and a boolean full? representing whether all the
;; slots checked so far have been filled.
;; Returns Winning-Line with the correct parameters if there is a win,
;; 'tie if all slots are filled and there is no winner, and 'ongoing otherwise.
(define (check-win brd r c full?)
  (local
    {(: current : Integer)
     ;; stores the value of check-pos called on the current piece being checked
     (define current (check-pos (Pos r c) brd))}
    (cond
      [(and (= r 5) (= c 6))
       (cond
         [(= current -1) 'ongoing]
         [else (if full? 'tie 'ongoing)])]
      [(= r 5)
       (cond
         [(= current -1) (check-win brd 0 (+ c 1) #f)]
         [(= current -2) (check-win brd 0 (+ c 1) full?)]
         [else (Winning-Line (get-piece r c brd)
                             (Pos r c)
                             (end-pos r c current))])]
      [else
       (cond
         [(= current -1) (check-win brd (+ r 1) c #f)]
         [(= current -2) (check-win brd (+ r 1) c full?)]
         [else (Winning-Line (get-piece r c brd)
                             (Pos r c)
                             (end-pos r c current))])])))

;; Since the outcome function below simply outputs the check-win function
;; called on the game board and the starting indexes of row/column 0 and full?
;; as #t, I will provide tests only for the outcome function

(: outcome : Game -> (U Winning-Line 'tie 'ongoing))
;; takes in a game and returns Winning-line with the correct information if
;; a player has made 4 in a row, returns 'tie if all the slots are used and
;; no one has won, and 'ongoing otherwise
(define (outcome g)
  (check-win (Game-board g) 0 0 #t))

;; More stacks/boards/games for testing
(define t1 (Stack 6 (list 'black 'white 'black 'white 'black 'white)))
(define t2 (Stack 6 (list 'white 'black 'white 'black 'white 'black)))
(define tied-board (Board (list t1 t2 t2 t1 t1 t2 t1)))
(define d3 (Stack 5 (list 'black 'white 'black 'white 'white)))
(define d4 (Stack 2 (list 'black 'white)))
(define black-win (Board (list a b d3 d1 a d4 c)))
(define white-win (Board (list a t1 t2 c g d3 d2)))
(define randomg (Game random 'black))
(define whiteg1 (Game random2 'black))
(define tiedg (Game tied-board 'black))
(define blackg1 (Game black-win 'black))
(define whiteg2 (Game white-win 'black))
(define whiteg3 (Game check4-board 'black))

(check-expect (outcome randomg) 'ongoing)
(check-expect (outcome tiedg) 'tie)
(check-expect (outcome whiteg1)
              (Winning-Line 'white (Pos 0 3) (Pos 3 3)))
(check-expect (outcome blackg1)
              (Winning-Line 'white (Pos 1 0) (Pos 1 3)))
(check-expect (outcome whiteg2)
              (Winning-Line 'white (Pos 0 6) (Pos 3 6)))
(check-expect (outcome whiteg3)
              (Winning-Line 'white (Pos 3 0) (Pos 0 3)))

;; Visual Component

(: padding : Integer -> Integer)
;; Takes in the distance between the centers of adjacent circles and returns
;; the padding between adjacent circles
(define (padding spacing)
  (floor (* 1/5 spacing)))

(check-expect (padding 70) 14)

(: radius : Integer -> Integer)
;; Takes in the distance between the centers of adjacent circles and returns
;; the radius of the circles
(define (radius spacing)
  (floor (* 2/5 spacing)))

(check-expect (radius 70) 28)

(: background : Integer Integer -> Image)
;; Takes in the distance between the centers of adjacent pieces and returns
;; a border for the game board
(define (background padding radius)
  (rectangle (+ (* 14 radius) (* 8 padding))
             (+ (* 12 radius) (* 7 padding)) "solid" "black"))

(: empty-slots : Integer Integer Integer -> Image)
;; Takes in the number of empty slots in a column, radius of the pieces,
;; and padding between pieces and returns an image of the slots
(define (empty-slots num padding radius)
  (if (= num 0)
      empty-image
      (above (circle radius "solid" "gray")
             (rectangle 1 padding 0 "white")
             (empty-slots (- num 1) padding radius))))
     

(: draw-pieces : Integer Integer (Listof Player) -> Image)
;; Takes in the padding between pieces, radius of the pieces,
;; and the list of pieces in a stack and returns an image of the pieces
(define (draw-pieces padding radius lst)
  (match lst
    [(cons x xr)
     (if (symbol=? x 'black)
         (above (circle radius "solid" "red")
                (rectangle 1 padding 0 "white")
                (draw-pieces padding radius xr))
         (above (circle radius "solid" "blue")
                (rectangle 1 padding 0 "white")
                (draw-pieces padding radius xr)))]
    ['() empty-image]))

   
(: draw-stack : Integer Integer Stack -> Image)
;; Takes in the padding between pieces, radius of the pieces,
;; and a stack and returns an image of the pieces in that stack
(define (draw-stack padding radius stk)
  (match stk
    [(Stack height pieces)
     (above (rectangle 1 padding 0 "white")
            (empty-slots (- 6 height) padding radius)
            (draw-pieces padding radius pieces))]))

     
(: draw-stacks : Integer Integer (Listof Stack) -> Image)
;; Takes in the padding between pieces, radius of the pieces, and a list of
;; stacks and returns an image of the stacks next to each other with the 
;; correct spacing
(define (draw-stacks padding radius lst)
  (match lst
    [(cons x xr)
     (beside (rectangle padding 1 0 "white")
             (draw-stack padding radius x)
             (draw-stacks padding radius xr))]
    ['() (beside (rectangle padding 1 0 "white") empty-image)]))


(: board-image : Board Integer -> Image)
;; Takes in a board brd, the distance between the centers of
;; adjacent spaces, and returns an image of the board in the
;; current game state
(define (board-image brd spacing)
  (overlay (draw-stacks (padding spacing) (radius spacing) (Board-stacks brd))
           (background (padding spacing) (radius spacing))))

(: draw-line : Pos Pos Integer Integer -> Image)
;; Takes in the position of the start and end of the winning pieces, padding
;; between pieces p and radius of pieces r, and draws a line from the start
;; to the end
(define (draw-line start end p r)
  (match* (start end)
    [((Pos r1 c1) (Pos r2 c2))
     (add-line (rectangle (+ (* 14 r) (* 8 p))
                          (+ (* 12 r) (* 7 p)) 0 "white")
               (+ (* 2 r c1) r (* (+ c1 1) p))
               (+ (* 2 (- 5 r1) r) r (* (- 5 r1) p) p)
               (+ (* 2 r c2) r (* (+ c2 1) p))
               (+ (* 2 (- 5 r2) r) r (* (- 5 r2) p) p)
               (pen "green" 10 "solid" "round" "round"))]))

(: int-to-byte : Integer -> Byte)
;; returns input integer as a byte, throws an error if the input is not a byte
(define (int-to-byte num)
  (if (byte? num) num (error "invalid number")))

(: win-banner : Player Integer Integer Controller Controller -> Image)
;; Takes in the winning player pl, padding between pieces p and radius of pieces  
;; r, controllers, and draws a banner indicating who has won the game
(define (win-banner pl p r c1 c2)
  (local
    {(: banner-width : Real)
     (define banner-width (+ (* 14 r) (* 8 p)))}
    (if (symbol=? pl 'black)
        (overlay (text (string-append (get-name c1) " wins!")
                       (int-to-byte (- (* r 2) 5)) "olive")
                 (rectangle banner-width (* 1/7 banner-width) 50 "gray"))
        (overlay (text (string-append (get-name c2) " wins!")
                       (int-to-byte (- (* r 2) 5)) "olive")
                 (rectangle banner-width (* 1/7 banner-width) 50 "gray")))))

(: tie-banner : Integer Integer -> Image)
;; Takes in the padding between pieces p and radius of pieces  
;; r, and draws a banner saying that it is a tie
(define (tie-banner p r)
  (overlay (text "It's a tie!" (int-to-byte (- (* r 2) 5)) "olive")
           (rectangle (+ (* 14 r) (* 8 p)) (* 1/7 (+ (* 14 r) (* 8 p)))
                      50 "gray")))

(: ongoing-banner : Player Integer Integer Controller Controller -> Image)
;; Takes in the next player pl, padding between pieces p and radius of pieces r,
;; controllers, and draws a banner indicating who's turn it is
(define (ongoing-banner pl p r c1 c2)
  (local
    {(: banner-width : Real)
     (define banner-width (+ (* 14 r) (* 8 p)))}
    (if (symbol=? pl 'black)
        (overlay (text (string-append (get-name c1) "'s Turn")
                       (int-to-byte (- (* r 2) 5)) "olive")
                 (rectangle banner-width (* 1/7 banner-width) 50 "gray"))                         
        (overlay (text (string-append (get-name c2) "'s Turn")
                       (int-to-byte (- (* r 2) 5)) "olive")
                 (rectangle banner-width (* 1/7 banner-width) 50 "gray")))))


(define-struct Human
  ([name : (U String Symbol)]))

(define-struct Bot
  ([name : (U String Symbol)]
   [strategy : Strategy]))

(define-type Controller (U Human Bot))

(define-struct World
  ([game : Game]
   [black-player : Controller]
   [white-player : Controller]
   [spacing : Integer]
   [valid-move : Boolean]
   [correct-turn : Boolean]
   [game-over : Boolean]))


(: get-name : Controller -> String)
;; Takes in a Controller and returns the controller's name as a string
(define (get-name c)
  (if (Human? c)
      (if (symbol? (Human-name c))
          (symbol->string (Human-name c))
          (Human-name c))
      (if (symbol? (Bot-name c)) (symbol->string (Bot-name c)) (Bot-name c))))

(check-expect (get-name (Human "jeff")) "jeff")
(check-expect (get-name (Human 'jeff)) "jeff")
(check-expect (get-name (Bot "bot" first-available)) "bot")
(check-expect (get-name (Bot 'bot first-available)) "bot")


(: game-image : Game Integer Controller Controller -> Image)
;; Takes in a game, the spacing between the centers of adjacent pieces, the
;; controllers, and produces an image of the board. Draws a line over the
;; winning pieces if a player has won, and displays text of who has won, and
;; displays text saying it's a tie if the game is tied with all slots filled.
;; If no one has won, displays a banner saying who's turn it is.
(define (game-image g spacing c1 c2)
  (local
    {(: game-state : (U Winning-Line 'tie 'ongoing))
     (define game-state (outcome g))
     (: r : Integer)
     (define r (radius spacing))
     (: p : Integer)
     (define p (padding spacing))}
    (match g
      [(Game brd next)
       (cond
         [(and (not (Winning-Line? game-state)) (symbol=? game-state 'tie))
          (above (tie-banner p r) (board-image brd spacing))]
         [(and (not (Winning-Line? game-state)) (symbol=? game-state 'ongoing))
          (above (ongoing-banner next p r c1 c2) (board-image brd spacing))]
         [else
          (match game-state
            [(Winning-Line winner start end)
             (above
              (win-banner winner p r c1 c2)
              (overlay
               (draw-line start end p r)
               (board-image brd spacing)))])])])))


(: col-full-message : Integer Integer -> Image)
;; Takes in the padding between and radius of circles and returns an image 
;; of a message that the column is full
(define (col-full-message p r)
  (local
    {(: width : Real)
     (define width (+ (* 14 r) (* 8 p)))}
    (overlay (text "column full!" (int-to-byte (- (* r 2) 5)) "olive")
             (rectangle width (* 1/7 width) 50 "gray"))))

(: empty-message : Integer Integer -> Image)
;; Takes in the padding between and radius of circles and returns an image
;; of same dimension as col-full-message but without any text
(define (empty-message p r)
  (local
    {(: width : Real)
     (define width (+ (* 14 r) (* 8 p)))}
    (rectangle width (* 1/7 width) 50 "gray")))

(: wrong-turn-message : Integer Integer -> Image)
;; Takes in the padding between and radius of circles and returns a message
;; of same dimension as col-full-message saying it isn't the player's turn
(define (wrong-turn-message p r)
  (local
    {(: width : Real)
     (define width (+ (* 14 r) (* 8 p)))}
    (overlay (text "Not your turn!" (int-to-byte (- (* r 2) 5)) "olive")
             (rectangle width (* 1/7 width) 50 "gray"))))


(: draw : World -> Image)
;; Draws the current World including the message box at the bottom of the board.
;; Draws a different message depending on the fields in the World. If
;; valid-move is false : col-full-message is displayed
;; correct-turn is false : wrong-turn-message is displayed
;; otherwise : empty message box is displayed
(define (draw w)
  (match w
    [(World game c1 c2 spacing move turn _)
     (cond
       [(not turn)
        (above (game-image game spacing c1 c2)
               (wrong-turn-message (padding spacing) (radius spacing)))]
       [move
        (above (game-image game spacing c1 c2)
               (empty-message (padding spacing) (radius spacing)))]
       [else
        (above (game-image game spacing c1 c2)
               (col-full-message (padding spacing) (radius spacing)))])]))

(: column : Integer Integer Integer -> Integer)
;; Takes in the x coordinate of the mouse, padding between circles, radius
;; of circles and returns the column the mouse is in if the column is not full
(define (column x padding radius)
  (local
    {(: width : Real)
     (define width (+ (* 14 radius) (* 8 padding)))}
    (cond
      [(> x (* 6/7 width)) 6]
      [(> x (* 5/7 width)) 5]
      [(> x (* 4/7 width)) 4]
      [(> x (* 3/7 width)) 3]
      [(> x (* 2/7 width)) 2]
      [(> x (* 1/7 width)) 1]
      [else 0])))      
                                   
(check-expect (column 300 14 28) 4)
(check-expect (column 40 14 28) 0)

(: game-over? : Game -> Boolean)
;; Takes in a World and returns true if someone has won or a tie has occurred
(define (game-over? g)
  (local    
    {(: game-state : (U Winning-Line 'tie 'ongoing))
     (define game-state (outcome g))}
    (or (Winning-Line? game-state) (symbol=? game-state 'tie))))

(check-expect (game-over? game1) #t)
(check-expect (game-over? tiedg) #t)
(check-expect (game-over? game4) #f)


(: react-to-mouse : World Integer Integer Mouse-Event -> World)
;; If a human clicks on a column that is not full, returns an updated World
;; with the player's piece in the column. If the move ends the game, returns the
;; updated World with the game-over field as true. If the column is full,
;; returns the same World with the valid-move field as false. If the human
;; clicks when it is the bot's turn, returns the same World with the
;; correct-turn field as false.
;; If over? is true, does not respond to interactions
;;
;; Arguments: w - world, x - x-coordinate, m - Mouse-Event
(define (react-to-mouse w x y e)
  (match w
    [(World g c1 c2 spacing _ _ over?)
     (local
       {(: col : Integer)
        ;; stores the column that the mouse is in
        (define col (column x (padding spacing) (radius spacing)))}
       (if (and (mouse=? e "button-down") (not over?))
           (cond
             [(or (and (symbol=? (Game-next g) 'black) (Bot? c1))
                  (and (symbol=? (Game-next g) 'white) (Bot? c2)))
              (World g c1 c2 spacing #t #f #f)]
             [(filled? 5 col (Game-board g))
              (World g c1 c2 spacing #f #t #f)]
             [else
              (World (apply-move g (Game-next g) col) c1 c2 spacing #t #t
                     (game-over? (apply-move g (Game-next g) col)))])         
           w))]))

        
(define world1 (World game2 (Human "Tim") (Human "Arthur") 70 #t #t #f))
(define new-game2 (Game (Board
                         (list
                          (Stack 4 '(black white white black)) c b a g d c))
                        'white))
(define new-world1 (World new-game2 (Human "Tim") (Human "Arthur") 70 #t #t #t))
(define world2 (World game4
                      (Bot "A" first-available) (Human "Tim") 70 #t #t #f))
(define game5 (Game (Board
                     (list (Stack 6 '(black black white white black white))
                           e e e e e e)) 'black))
(define world3 (World game3 (Human "Tim") (Human "Arthur") 70 #t #t #f))
(define world4 (World game2 (Human "Tim") (Human "Arthur") 70 #t #t #t))
                      
(check-expect (react-to-mouse world1 50 50 "button-down") new-world1)
(check-expect (World-correct-turn
               (react-to-mouse world2 300 50 "button-down")) #f)
(check-expect (World-valid-move (react-to-mouse world3 50 50 "button-down")) #f)
(check-expect (react-to-mouse world4 300 50 "button-down") world4)


(: bot-move : World -> World)
;; Takes in a World and returns the updated World with the move made by the bot
;; if it is the bot's turn, returns the input World otherwise. If the move ends
;; the game, returns the updated World with the game-over? field as true.
;; If over? is true, simply returns the input world. 
(define (bot-move w)
  (match w
    [(World g c1 c2 spacing _ _ over?)
     (cond
       [(and (symbol=? (Game-next g) 'black) (Bot? c1) (not over?))
        (local
          {(: updated-game : Game)
           (define updated-game (apply-move g (Game-next g)
                                            ((Bot-strategy c1) g)))}
          (World updated-game c1 c2 spacing #t #t (game-over? updated-game)))]
       [(and (symbol=? (Game-next g) 'white) (Bot? c2) (not over?))
        (local
          {(: updated-game : Game)
           (define updated-game (apply-move g (Game-next g)
                                            ((Bot-strategy c2) g)))}
          (World updated-game c1 c2 spacing #t #t (game-over? updated-game)))]      
       [else w])]))


(define game6 (Game (Board (list (Stack 1 '(black)) e e e e e e)) 'white))
(define world5 (World game6 
                      (Human "Tim") (Bot "A" first-available) 70 #t #t #f))
(define game7 (Game
               (Board (list (Stack 2 '(white black)) e e e e e e)) 'black))
(define world6 (World game7 (Bot "A" first-available) (Human "Tim")
                      70 #t #t #f))
(define game8 (Game
               (Board
                (list (Stack 3 '(black white black)) e e e e e e)) 'white))
(define world7 (World game6 
                      (Bot "A" first-available) (Human "Tim") 70 #t #t #f))

(define tied-world (World tiedg (Bot "A" first-available) (Human "Tim")
                          70 #t #t #t))

(check-expect (World-game (bot-move world5)) game7)
(check-expect (World-game (bot-move world6)) game8)
(check-expect (World-game-over (bot-move world5)) #f)
(check-expect (bot-move tied-world) tied-world)
(check-expect (bot-move world7) world7)

(: play : Controller Controller Integer -> World)
;; Begins the game with a big bang
(define (play c1 c2 spacing)
  (cond
    [(and (Human? c1) (Human? c2))
     (big-bang (World new-game c1 c2 spacing #t #t #f) : World
       [to-draw draw]
       [on-mouse react-to-mouse])]
    [(or (Human? c1) (Human? c2))
     (big-bang (World new-game c1 c2 spacing #t #t #f) : World
       [to-draw draw]
       [on-mouse react-to-mouse]
       [on-tick bot-move 1])]
    [else
     (big-bang (World new-game c1 c2 spacing #t #t #f) : World
       [to-draw draw]
       [on-tick bot-move 1])]))

;;(play (Human "Tim") (Human "Arthur") 10)
;;(play (Human "Tim") (Human "Arthur") 70)
;;(play (Human 'Tim) (Human 'Arthur) 60)
;;(play (Human "Tim") (Bot "Marvin" first-available) 60)
;;(play (Bot "Marvin" first-available) (Human "Arthur") 70)
;;(play (Bot "Marvin" first-available) (Bot "HAL" first-available) 60)



;; In order to implement the heuristic function, I changed the definition of
;; the player type to (U 'empty 'black 'white). This should not affect 
;; previously written code since all boards will still be the same. However, if
;; I am testing a hypothetical board with a "hovering" piece for a winning-line,
;; the stack with the hovering piece will be adjusted to contain 'empty pieces
;; underneath the hovering piece. This allows me to reuse my old code to check
;; for a winning-line.

(define-type Heuristic (Game -> Integer))

(define x1 (Stack 1 (list 'white)))
(define x3 (Stack 1 (list 'white)))
(define x4 (Stack 6 (list 'black 'white 'black 'black 'black 'white)))
(define x5 (Stack 5 (list 'black 'white 'white 'black 'white)))
(define x6 (Stack 4 (list 'black 'white 'black 'black)))
(define y (Board (list x1 e x3 x4 x5 x6 e)))
(define y2 (Board (list x3 e e e x5 x4 e)))
(define z (Game y 'black))

(: empty-pieces : Integer (Listof Player) -> (Listof Player))
;; Takes in the number of 'empty slots and a list of players and returns
;; a list of players with the number of 'empty slots added to the front of the
;; list
(define (empty-pieces n lst)
  (if (> n 0)
      (cons 'empty (empty-pieces (- n 1) lst))
      lst))

(check-expect (empty-pieces 3 (list 'black 'white 'black))
              (list 'empty 'empty 'empty 'black 'white 'black))
(check-expect (empty-pieces 0 (list 'black 'black 'white))
              (list 'black 'black 'white))

(: apply-move-pos : Board Player Integer Integer -> Board)
;; Takes in a board, player, row and column number, and returns a board with the
;; player's added to the given row and column. If the piece is "hovering", adds
;; the necessary 'empty pieces in the column. 
(define (apply-move-pos brd pl r c)
  (local
    {(: add-piece : Stack Integer Player -> Stack)
     ;; Takes in stack, row, and player, and returns a stack with the player's
     ;; piece in the row, with the appropriate number of 'empty pieces in
     ;; between 
     (define (add-piece stk r pl)
       (match stk
         [(Stack height lst)
          (Stack (+ r 1)
                 ((inst cons Player) pl (empty-pieces (- r height) lst)))]))}
    (match brd
      [(Board stacks)
       (Board (replace-col stacks (add-piece (list-ref stacks c) r pl) c 0))])))

(check-expect (apply-move-pos (Board (list e e x1 e e e e)) 'black 4 2)
              (Board
               (list e e (Stack 5 (list 'black 'empty 'empty 'empty 'white))
                     e e e e)))

(: pos-value : Board Integer Integer -> Integer)
;; Takes in the board and a row and column on the board and returns 0 if that
;; position results in both a white and black win or neither, 1 if only black,
;; -1 if only white 
(define (pos-value brd r c)
  (local
    {(: b : (U Winning-Line 'tie 'ongoing))
     ;; Result of calling check-win on the board after applying the black move
     ;; to the given position
     (define b (check-win (apply-move-pos brd 'black r c) 0 0 #t))
     (: w : (U Winning-Line 'tie 'ongoing))
     ;; Same as b except applied to the white player
     (define w (check-win (apply-move-pos brd 'white r c) 0 0 #t))}
    (cond
      [(and (Winning-Line? b) (Winning-Line? w)) 0]
      [(Winning-Line? b) 1]
      [(Winning-Line? w) -1]
      [else 0])))

(check-expect (pos-value y 1 2) 1)
(check-expect (pos-value y 3 2) 1)
(check-expect (pos-value y 1 6) 0)
(check-expect (pos-value y 2 6) 1)
(check-expect (pos-value y 0 1) -1)
(check-expect (pos-value y 5 2) -1)

(: counter : Board Integer Integer Integer -> Integer)
;; Takes in a board, and iterates through the board starting from r=0 and c=0
;; and returns the value that should be returned by count-winning-positions
;; assuming the game is still ongoing
(define (counter brd r c sum)
  (cond
    [(= c 7) sum]
    [(and (= r 5) (filled? r c brd))
     (counter brd 0 (+ c 1) sum)]
    [(and (= r 5) (not (filled? r c brd)))
     (counter brd 0 (+ c 1) (+ sum (pos-value brd r c)))]
    [(filled? r c brd)
     (counter brd (+ r 1) c sum)]
    [else
     (counter brd (+ r 1) c (+ sum (pos-value brd r c)))]))

(check-expect (counter y 0 0 0) 1)
(check-expect (counter random2 0 0 0) 0)
(check-expect (counter y2 0 0 0) 0)
     
(: count-winning-positions : Heuristic)
;; Returns the number of positions on the board that give black a win, minus
;; the number of positions that give white a win, and returns 1000 if black
;; has won, -1000 if white has won
(define (count-winning-positions g)
  (match g
    [(Game brd _)
     (local
       {(: state : (U Winning-Line 'tie 'ongoing))
        (define state (check-win brd 0 0 #t))}
       (cond
         [(and (Winning-Line? state)
               (symbol=? (Winning-Line-player state) 'black))
          1000]
         [(and (Winning-Line? state)
               (symbol=? (Winning-Line-player state) 'white))
          -1000]
         [(and (not (Winning-Line? state)) (symbol=? state 'tie)) 0]
         [else (counter brd 0 0 0)]))]))

(define b-win-brd
  (Board (list e e e (Stack 4 (list 'black 'black 'black 'black)) e e e)))
(define b-win (Game b-win-brd 'white))

(check-expect (count-winning-positions z) 1)
(check-expect (count-winning-positions b-win) 1000)
(check-expect (count-winning-positions whiteg2) -1000)
(check-expect (count-winning-positions tiedg) 0)

;; Boards/Games for testing


(define win-in-1-board
  (Board (list (Stack 3 (list 'black 'black 'black))
               (Stack 3 (list 'white 'white 'white))
               e e e e e)))
(define b-win-in-1 (Game win-in-1-board 'black))
(define w-win-in-1 (Game win-in-1-board 'white))
(define b-win-in-2-board
  (Board (list (Stack 2 (list 'white 'white))
               (Stack 2 (list 'black 'black))
               (Stack 2 (list 'black 'black))
               (Stack 2 (list 'black 'black))
               e 
               (Stack 2 (list 'white 'white))
               (Stack 2 (list 'white 'white)))))
(define b-win-in-2 (Game b-win-in-2-board 'black))
(define w-win-in-2-board
  (Board (list (Stack 2 (list 'black 'black))
               (Stack 2 (list 'white 'white))
               (Stack 2 (list 'white 'white))
               (Stack 2 (list 'white 'white))
               e 
               (Stack 2 (list 'black 'black))
               (Stack 2 (list 'black 'black)))))
(define w-win-in-2 (Game w-win-in-2-board 'white))

(: traverse-nodes1 : Heuristic Game Integer Integer Integer -> Integer)
;; Takes in the heuristic function, current game state, index i=0, an impossibly
;; small value for max, and the ply, and returns the value of the heuristic
;; evaluated at that node if the next player is black 
(define (traverse-nodes1 h state i max ply)
  (local
    {(: board-state : (U Winning-Line 'ongoing 'tie))
     (define board-state (check-win (Game-board state) 0 0 #t))
     (: traverse-nodes1-helper : Heuristic Game
        Integer Integer Integer Integer -> Integer)
     ;; function to help avoid nested if in the case that the current child node
     ;; being evaluated is a valid move
     (define (traverse-nodes1-helper h state i max ply current-node)
       (if (> current-node max)
           (traverse-nodes1 h state (+ i 1) current-node ply)
           (traverse-nodes1 h state (+ i 1) max ply)))}
    (cond
      [(or (= ply 0) (Winning-Line? board-state)
           (and (not (Winning-Line? board-state)) (symbol=? 'tie board-state)))
       (h state)]
      [(= i 7)
       max]
      [(valid-move? state 'black i)
       (local
         {(: current-node : Integer)
          ;; value of current node being evaluated
          (define current-node
            (traverse-nodes2 h (apply-move state 'black i)
                             0 2000 (- ply 1)))}
         (traverse-nodes1-helper h state i max ply current-node))]
      [else 
       (traverse-nodes1 h state (+ i 1) max ply)])))

(check-expect (traverse-nodes1 count-winning-positions z 0 -2000 0) 1)
(check-expect (traverse-nodes1 count-winning-positions b-win 0 -2000 2) 1000)
(check-expect (traverse-nodes1 count-winning-positions whiteg2 0 -2000 2) -1000)
(check-expect
 (traverse-nodes1 count-winning-positions b-win-in-1 0 -2000 2) 1000)
(check-expect
 (traverse-nodes1 count-winning-positions b-win-in-2 0 -2000 2) 1000)


(: traverse-nodes2 : Heuristic Game Integer Integer Integer -> Integer)
;; Takes in the heuristic function, current game state, index i=0, an impossibly
;; large value for min, and the ply, and returns the value of the heuristic
;; evaluated at that node if the next player is white
(define (traverse-nodes2 h state i min ply)
  (local
    {(: board-state : (U Winning-Line 'ongoing 'tie))
     (define board-state (check-win (Game-board state) 0 0 #t))
     (: traverse-nodes2-helper : Heuristic Game
        Integer Integer Integer Integer -> Integer)
     ;; function to help avoid nested if in the case that the current child node
     ;; being evaluated is a valid move
     (define (traverse-nodes2-helper h state i min ply current-node)
       (if (< current-node min)
           (traverse-nodes2 h state (+ i 1) current-node ply)
           (traverse-nodes2 h state (+ i 1) min ply)))}
    (cond
      [(or (= ply 0) (Winning-Line? board-state)
           (and (not (Winning-Line? board-state)) (symbol=? 'tie board-state)))
       (h state)]
      [(= i 7)
       min]
      [(valid-move? state 'white i)
       (local
         {(: current-node : Integer)
          ;; value of current-node being evaluated
          (define current-node
            (traverse-nodes1 h (apply-move state 'white i)
                             0 -2000 (- ply 1)))}
         (traverse-nodes2-helper h state i min ply current-node))]
      [else (traverse-nodes2 h state (+ i 1) min ply)])))

(check-expect (traverse-nodes2 count-winning-positions z 0 2000 0) 1)
(check-expect (traverse-nodes2 count-winning-positions b-win 0 2000 2) 1000)
(check-expect (traverse-nodes2 count-winning-positions whiteg2 0 2000 2) -1000)
(check-expect
 (traverse-nodes2 count-winning-positions w-win-in-1 0 2000 2) -1000)
(check-expect
 (traverse-nodes2 count-winning-positions w-win-in-2 0 2000 2) -1000)

(: minimax-eval : Heuristic Integer Game -> Integer)
;; Takes in a heuristic function, the ply, a game state, and assigns a score
;; using the given heuristic function
(define (minimax-eval h ply state)
  (if (symbol=? (Game-next state) 'black)
      (traverse-nodes1 h state 0 -2000 ply)
      (traverse-nodes2 h state 0 2000 ply)))

(check-expect (minimax-eval count-winning-positions 0 z) 1)
(check-expect (minimax-eval count-winning-positions 3 w-win-in-1) -1000)
(check-expect (minimax-eval count-winning-positions 3 b-win-in-1) 1000)
(check-expect (minimax-eval count-winning-positions 3 w-win-in-2) -1000)
(check-expect (minimax-eval count-winning-positions 3 b-win-in-2) 1000)
(check-expect (minimax-eval count-winning-positions 3 b-win) 1000)
(check-expect (minimax-eval count-winning-positions 3 whiteg2) -1000)

(: choose-move1 : Game Heuristic Integer Integer Integer Integer -> Integer)
;; Takes in the game state, the heuristic function, the ply, an index i=0,
;; the index for the max node (initially set to 0), and a max impossibly small,
;; and returns the index of the max child node, if the next player is black
(define (choose-move1 state h ply i max-i max)
  (local
    {(: choose-move1-helper : Game Game Heuristic Integer
        Integer Integer Integer Integer -> Integer)
     ;; function to help avoid nested if 
     (define (choose-move1-helper state updated-state h ply i max-i max value)
       (cond
         [(> value max)
          (choose-move1 state h ply (+ i 1) i value)]
         [(and (= value 1000)
               (Winning-Line? (check-win (Game-board updated-state) 0 0 #t)))
          (choose-move1 state h ply (+ i 1) i value)]
         [else
          (choose-move1 state h ply (+ i 1) max-i max)]))}         
    (cond
      [(= i 7) max-i]
      [(valid-move? state 'black i)
       (local
         {(: updated-state : Game)
          (define updated-state (apply-move state 'black i))
          (: value : Integer)
          (define value (minimax-eval h ply updated-state))}
         (choose-move1-helper state updated-state h ply i max-i max value))]
      [else (choose-move1 state h ply (+ i 1) max-i max)])))

(check-expect (choose-move1 b-win-in-1 count-winning-positions 3 0 0 -2000) 0)
(check-expect (choose-move1 b-win-in-2 count-winning-positions 3 0 0 -2000) 4)

(: choose-move2 : Game Heuristic Integer Integer Integer Integer -> Integer)
;; Takes in the game state, the heuristic function, the ply, an index i=0,
;; the index for the min node (initially set to 0), and a min impossibly large,
;; and returns the index of the min child node, if the next player is white
(define (choose-move2 state h ply i min-i min)
  (local
    {(: choose-move2-helper : Game Game Heuristic Integer
        Integer Integer Integer Integer -> Integer)
     ;; function to help avoid nested in the case that the current child node
     ;; being evaluated is a valid move
     (define (choose-move2-helper state updated-state h ply i min-i min value)
       (cond
         [(< value min)
          (choose-move2 state h ply (+ i 1) i value)]
         [(and (= value -1000)
               (Winning-Line? (check-win (Game-board updated-state) 0 0 #t)))
          (choose-move2 state h ply (+ i 1) i value)]
         [else
          (choose-move2 state h ply (+ i 1) min-i min)]))}
    (cond
      [(= i 7) min-i]
      [(valid-move? state 'white i)
       (local
         {(: updated-state : Game)
          (define updated-state (apply-move state 'white i))
          (: value : Integer)
          (define value (minimax-eval h ply updated-state))}
         (choose-move2-helper state updated-state h ply i min-i min value))]
      [else (choose-move2 state h ply (+ i 1) min-i min)])))

(check-expect (choose-move2 w-win-in-1 count-winning-positions 3 0 0 2000) 1)
(check-expect (choose-move2 w-win-in-2 count-winning-positions 3 0 0 2000) 4)

(: make-minimax-strategy : Heuristic Integer -> Strategy)
;; Takes in the heuristic function and the ply, and returns a strategy based
;; on the given parameters
(define (make-minimax-strategy h ply)
  (local
    {(: minimax-strategy : Strategy)
     (define (minimax-strategy g)
       (cond
         [(Winning-Line? (check-win (Game-board g) 0 0 #t))
          (error "Game is already over")]
         [(symbol=? (Game-next g) 'black) (choose-move1 g h ply 0 0 -2000)]
         [else (choose-move2 g h ply 0 0 2000)]))}
    minimax-strategy))

(check-expect ((make-minimax-strategy count-winning-positions 3) b-win-in-1) 0)
(check-expect ((make-minimax-strategy count-winning-positions 3) b-win-in-2) 4)
(check-expect ((make-minimax-strategy count-winning-positions 3) w-win-in-1) 1)
(check-expect ((make-minimax-strategy count-winning-positions 3) w-win-in-2) 4)
(check-error
 ((make-minimax-strategy count-winning-positions 3) b-win)
 "Game is already over")
#|                
(play (Human "Basil")
     (Bot "Marvin" (make-minimax-strategy count-winning-positions 3))
      60)                
|#
(test)





















