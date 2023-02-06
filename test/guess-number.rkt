#lang racket

(module+ main)
(module+ test (require rackunit))

; A game where the program picks a number from 0 to 9 and the user guesses it

(require "../main.rkt")
(define-syntax-rule (check-expect actual expected) (module+ test (check-equal? actual expected)))

(define MIN 0)
(define MAX 9)

(define-struct game-state [num-guesses target last-guess] #:transparent)
; A GameState is a (make-game-state Integer Guess MaybeGuess) where
; num-guesses is how many guesses the user has made so far
; target is the number the user is trying to guess
; last-guess is the most recent guess. #f means the user hasn't guessed yet
; Represents the state of the user trying to guess the target number
(define state0 (make-game-state 0 7 #f))
(define state-low (make-game-state 1 7 5))
(define state-high (make-game-state 2 7 9))
(define state-win (make-game-state 3 7 7))

; A MaybeGuess is one of
; A Guess
; #f
; Represents a guess that may not be present

; A Guess is an integer between and including MIN and MAX
; Represents a number that may be guessed

(module+ main
  (main #f))

#;(Any -> GameState)
; Run the game with a random target
(define (main _)
  (big-bang-ui (make-initial-game-state (random-target #f))
    [on-action handle-guess]
    [to-draw draw-game-state]))

#;(Integer -> GameState)
; Make an initial game state with the given target
(define (make-initial-game-state target)
  (make-game-state 0 target #f))

#;(Any -> Guess)
; Randomly generate a target number
(define (random-target _)
  (+ MIN (random (add1 (- MAX MIN)))))

#;(Guess GameState -> GameState)
; Handle the given guess
(define (handle-guess guess gs)
  (make-game-state (add1 (game-state-num-guesses gs))
                   (game-state-target gs)
                   guess))
(check-expect (handle-guess 5 state0) state-low)
(check-expect (handle-guess 9 state-low) state-high)
(check-expect (handle-guess 7 state-high) state-win)

#;(GameState -> GUI)
; render the game state, including a text box and the game's status if the game is not over
(define (draw-game-state gs)
  (if (and (number? (game-state-last-guess gs))
           (= (game-state-target gs) (game-state-last-guess gs)))
      (draw-game-status gs)
      (above (draw-game-status gs) guess-box)))
(check-expect (draw-game-state state0) (above (text "Guess!") guess-box))
(check-expect (draw-game-state state-low) (above (text "Nope, higher.") guess-box))
(check-expect (draw-game-state state-high) (above (text "Nope, lower.") guess-box))
(check-expect (draw-game-state state-win) (above (text "You've won!") (text "3")))

#;(GameState -> GUI)
; Render the status of the user's guessing
(define (draw-game-status gs)
  (cond
    [(boolean? (game-state-last-guess gs)) (text "Guess!")]
    [(< (game-state-last-guess gs) (game-state-target gs)) (text "Nope, higher.")]
    [(< (game-state-target gs) (game-state-last-guess gs)) (text "Nope, lower.")]
    [(= (game-state-target gs) (game-state-last-guess gs))
     (above (text "You've won!")
            (text (number->string (game-state-num-guesses gs))))]))
(check-expect (draw-game-status state0) (text "Guess!"))
(check-expect (draw-game-status state-low) (text "Nope, higher."))
(check-expect (draw-game-status state-high) (text "Nope, lower."))
(check-expect (draw-game-status state-win) (above (text "You've won!") (text "3")))

(define guess-box (text-input "Submit your guess" string->number))
