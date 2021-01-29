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
(define-type Player (U 'black 'white))

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
                                    (Stack 0 '()) (Stack 0 '()) (Stack 0 '())))
                       'black))


;; stacks/boards for testing
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


(: filled? : Integer Integer Board -> Boolean)
;; takes in the row and column of a slot, the board, and returns true if the
;; slot is filled with a piece from either player
(define (filled? r c brd)
  (< r (Stack-height (list-ref (Board-stacks brd) c))))

(check-expect (filled? 2 1 random) #t)
(check-expect (filled? 0 4 random) #f)

(: get-piece : Integer Integer Board -> Player)
;; takes in the row and column of a slot and returns the player in that slot
(define (get-piece r c brd)
  (match brd
    [(Board stacks)
     (list-ref (Stack-pieces (list-ref stacks c))
               (- (Stack-height (list-ref stacks c)) r 1))]))

(check-expect (get-piece 2 3 random) 'white)
(check-expect (get-piece 5 6 random) 'black)

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
;; if the move is valid and returns an error otherwise
(define (apply-move g pl c)
  (if (not (valid-move? g pl c))
      (error "invalid move")
      (match g
        [(Game (Board stacks) _)
         (Game (Board (replace-col stacks (update-col (list-ref stacks c) pl) c 0))
               (switch pl))])))

(check-expect (apply-move (Game random 'black) 'black 3) (Game random1 'white))
(check-expect (apply-move (Game random 'white) 'white 1) (Game random3 'black))
;; wrong player's turn 
(check-error (apply-move (Game random 'black) 'white 2) "invalid move")
;; stack is full
(check-error (apply-move (Game random 'black) 'black 6) "invalid move")
;; invalid index
(check-error (apply-move (Game random 'black) 'black 10) "invalid move")

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
       [(not (filled? r c brd)) -1]
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

(draw-stack 10 20 (Stack 3 (list 'black 'white 'black)))
     
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

;(board-image random 80)


(: draw-line : Pos Pos Integer Integer -> Image)
;; Takes in the position of the start and end of the winning pieces, padding
;; between pieces p and radius of pieces r, and draws a line from the start
;; to the end
(define (draw-line start end p r)
  (match* (start end)
    [((Pos r1 c1) (Pos r2 c2))
     (add-line (rectangle (+ (* 14 r) (* 8 p)) (+ (* 12 r) (* 7 p)) 0 "white")
               (+ (* 2 r c1) r (* (+ c1 1) p)) (+ (* 2 (- 5 r1) r) r (* (- 5 r1) p) p)
               (+ (* 2 r c2) r (* (+ c2 1) p)) (+ (* 2 (- 5 r2) r) r (* (- 5 r2) p) p)
               (pen "green" 10 "solid" "round" "round"))]))

(overlay (draw-line (Pos 2 2) (Pos 5 5) 14 28) (board-image random 70))

(: int-to-byte : Integer -> Byte)
;; returns input integer as a byte, throws an error if the input is not a byte
(define (int-to-byte num)
  (if (byte? num) num (error "invalid number")))

(: win-banner : Player Integer Integer -> Image)
;; Takes in the winning player pl, padding between pieces p and radius of pieces  
;; r, and draws a banner saying that pl has won the game
(define (win-banner pl p r)
  (if (symbol=? pl 'black)
      (overlay (text "Red wins!" (int-to-byte (- (* r 2) 5)) "olive")
               (rectangle (+ (* 14 r) (* 8 p)) (* 1/7 (+ (* 14 r) (* 8 p)))
                          50 "gray"))
      (overlay (text "Blue wins!" (int-to-byte (- (* r 2) 5)) "olive")
               (rectangle (+ (* 14 r) (* 8 p)) (* 1/7 (+ (* 14 r) (* 8 p)))
                          50 "gray"))))

(win-banner 'black 14 28)

(: tie-banner : Integer Integer -> Image)
;; Takes in the padding between pieces p and radius of pieces  
;; r, and draws a banner saying that it is a tie
(define (tie-banner p r)
  (overlay (text "It's a tie!" (int-to-byte (- (* r 2) 5)) "olive")
           (rectangle (+ (* 14 r) (* 8 p)) (* 1/7 (+ (* 14 r) (* 8 p)))
                      50 "gray")))

(tie-banner 14 28)

(: ongoing-banner : Player Integer Integer -> Image)
;; Takes in the next player pl, padding between pieces p and radius of pieces r,
;; and draws a banner indicating who's turn it is
(define (ongoing-banner pl p r)
  (if (symbol=? pl 'black)
      (overlay (text "Red's Turn" (int-to-byte (- (* r 2) 5)) "olive")
               (rectangle (+ (* 14 r) (* 8 p)) (* 1/7 (+ (* 14 r) (* 8 p)))
                          50 "gray"))
      (overlay (text "Blue's Turn" (int-to-byte (- (* r 2) 5)) "olive")
               (rectangle (+ (* 14 r) (* 8 p)) (* 1/7 (+ (* 14 r) (* 8 p)))
                          50 "gray"))))

(: game-image : Game Integer -> Image)
;; Takes in a game, the spacing between the centers of adjacent pieces, and
;; produces an image of the board. Draws a line over the winning pieces if
;; a player has won, and displays text of who has won, and displays text saying
;; it's a tie if the game is tied with all slots filled. 
;; If no one has won, displays a banner saying who's turn it is.
(define (game-image g spacing)
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
          (above (ongoing-banner next p r) (board-image brd spacing))]
         [else
          (match game-state
            [(Winning-Line winner start end)
             (above
              (win-banner winner p r)
              (overlay
               (draw-line start end p r)
               (board-image brd spacing)))])])])))
             
(game-image whiteg2 70)
(game-image tiedg 70)
(game-image randomg 70)
(game-image blackg1 70)
(game-image whiteg3 70)
  

(test)














