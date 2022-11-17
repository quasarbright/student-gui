#lang racket

; module interface

(module+ test (require rackunit))
(provide big-bang-ui)

; dependencies

(require racket/gui/easy
         (prefix-in easy: racket/gui/easy)
         (for-syntax syntax/parse))

; data definitions

(struct text [str] #:transparent)
(struct button [label action] #:transparent)
(struct beside% [guis] #:transparent)
; A GUI is one of
#;(text string?)
#;(button string? any/c)
#;(beside (listof GUI))

(define (beside . guis) (beside% guis))

; A WorldState is the global state of the application. It is a student-defined data type like in big-bang

; An Action is a representation of a user action. It is also a student-defined data type

; functionality

(define-syntax big-bang-ui
  (syntax-parser
    #:datum-literals (to-draw on-action)
    [(_ initial-state:expr
        (~alt (~once [to-draw draw-world:expr])
              (~optional [on-action handle-action:expr] #:defaults ([handle-action #'(lambda (_ ws) ws)])))
        ...)
     #'(big-bang-ui/proc initial-state draw-world handle-action)]))

#;(WorldState (WorldState -> GUI) (Action WorldState -> WorldState) -> WorldState)
; display a gui with the given renderer and action handler
(define (big-bang-ui/proc initial-state draw-world handle-action)
  (define @world-state (obs initial-state))
  (define @world-states (obs-map @world-state list))
  (render (window
           #:min-size '(#f 100)
           ; use list view just to get
           (list-view @world-states #:key identity (λ (ws @ws) (gui->view (draw-world ws) @world-state handle-action)))))
  ; TODO return final state
  )

#;(GUI obs? (Action WorldState -> WorldState) (is-a/c view<%>))
(define (gui->view gui @world-state handle-action)
  (match gui
    [(button label action) (easy:button label (λ () (obs-update! @world-state (λ (ws) (handle-action action ws)))))]
    [(text str) (easy:text str)]
    [(beside% guis) (apply hpanel (for/list ([gui guis])
                                   (gui->view gui @world-state handle-action)))]))

; testing

; counter
(big-bang-ui 1
             [to-draw (λ (n) (beside (text (number->string n)) (button "+" 'add1)))]
             [on-action (λ (action n) (match action ['add1 (add1 n)]))])

(module+ test)
