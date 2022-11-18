#lang racket

; module interface

(module+ test (require rackunit))
(module+ main)
(provide big-bang-ui
         (contract-out
          ; GUI components
          [text (-> string? gui?)]
          [button (-> string? any/c gui?)]
          [text-input (-> string? (-> string? any/c) gui?)]
          [beside (->* () #:rest (listof gui?) gui?)]
          [above (->* () #:rest (listof gui?) gui?)]
          ; GUI operations
          [gui? (-> any/c boolean?)]
          [gui=? (-> any/c any/c boolean?)]))

; dependencies

(require racket/gui/easy
         ; So I can shadow `button` and use easy button to render it
         (prefix-in easy: racket/gui/easy)
         (for-syntax syntax/parse))

; data definitions

(struct text [str] #:transparent)
(struct button [label action] #:transparent)
(struct text-input [label on-submit] #:transparent)
(struct beside% [guis] #:transparent)
(struct above% [guis] #:transparent)
; A GUI is one of
#;(text string?)
#;(button string? Action)
#;(text-input string? (string -> Action))
#;(beside GUI ...)
#;(above GUI ...)

; convenience constructors for list-like guis
(define (beside . guis) (beside% guis))
(define (above . guis) (above% guis))

; Predicate for GUI
(define gui? (or/c text? button? text-input? beside%? above%?))

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
; display a gui with the given renderer and optional action handler
(define (big-bang-ui/proc initial-state draw-world handle-action)
  (define @world-state (obs initial-state))
  (define @world-states (obs-map @world-state list))
  (render (window
           #:min-size '(200 100)
           ; use list view just to get
           (list-view @world-states #:key identity (λ (ws @ws) (gui->view (draw-world ws) @world-state handle-action)))))
  ; TODO figure out how to return final state
  )

#;(GUI obs? (Action WorldState -> WorldState) (is-a/c view<%>))
; render a GUI as a gui-easy view so it can be displayed
(define (gui->view gui @world-state handle-action)
  (define (recur gui) (gui->view gui @world-state handle-action))
  (define (dispatch action) (obs-update! @world-state (λ (ws) (handle-action action ws))))
  (match gui
    [(button label action) (easy:button label (λ () (dispatch action)))]
    [(text-input label on-submit)
     (define @input-value (obs ""))
     (hpanel (input @input-value
                    (λ (type input-value)
                      (when (eq? type 'return) (dispatch (on-submit input-value)))
                      ; not sure if this is necessary
                      (obs-update! @input-value (λ (_) input-value)))
                    #:label label)
             (easy:button "submit" (λ () (dispatch (on-submit (obs-peek @input-value))))))]
    [(text str) (easy:text str)]
    [(beside% guis) (apply hpanel (map recur guis))]
    [(above% guis) (apply vpanel (map recur guis))]))

#;(GUI GUI -> boolean?)
; are the two GUIs the same?
; Ignores behavior, just looks at structure and and content.
(define (gui=? gui1 gui2)
  ; TODO flatten trees of beside, above? Should (beside (beside a b) c) be the same as (beside a b c)?
  (match* (gui1 gui2)
    ; match will assert that the two labels are equal?
    [((button label _) (button label _)) #t]
    [((text-input label _) (text-input label _)) #t]
    [((text str) (text str)) #t]
    [((beside% guis1) (beside% guis2)) (guis=? guis1 guis2)]
    [((above% guis1) (above% guis2)) (guis=? guis1 guis2)]
    [(_ _) #f]))

#;((listof GUI) (listof GUI) -> boolean?)
; are the two lists of GUIs the same? (according to gui=?)
(define (guis=? guis1 guis2)
  (and (= (length guis1) (length guis2))
       (for/and ([gui1 guis1]
                 [gui2 guis2])
         (gui=? gui1 gui2))))

; main

(module+ main
  ; counter
  ; A WorldState is a Natural
  ; An Action is 'add1
  #;
  (big-bang-ui 0
               [to-draw (λ (n) (above (text "counter")
                                      (beside (text (number->string n)) (button "+" 'add1))))]
               [on-action (λ (action n) (match action ['add1 (add1 n)]))])
  ; input updates label
  ; A WorldState is a String for the current label.
  ; An Action is a String that replaces the WorldState
  (big-bang-ui "initial"
               [to-draw (λ (str) (above (text str)
                                        (text-input "enter a string" (λ (new-str) new-str))))]
               [on-action (λ (new-str old-str) new-str)]))

; tests

(module+ test
  (check-true (gui=? (button "foo" #t) (button "foo" #f)))
  (check-false (gui=? (button "foo" #t) (button "bar" #f)))
  (check-false (gui=? (button "foo" #f) (text "foo")))
  (check-true (gui=? (text "foo") (text "foo")))
  (check-false (gui=? (text "foo") (text "bar")))
  (check-true (gui=? (text-input "foo" (const #f))
                     (text-input "foo" (const #f))))
  (check-false (gui=? (text-input "foo" (const #f))
                     (text-input "bar" (const #f))))
  (check-false (gui=? (text-input "foo" (const #f))
                      (text "foo")))
  (check-true (gui=? (beside (text "foo") (text "bar"))
                     (beside (text "foo") (text "bar"))))
  (check-false (gui=? (beside (text "foo") (text "bar"))
                     (beside (text "bar") (text "foo"))))
  (check-false (gui=? (beside (text "foo") (text "bar"))
                      (text "bar")))
  (check-false (gui=? (beside (text "foo") (text "bar"))
                      (beside (text "foo") (text "bar") (text "baz"))))
  (check-true (gui=? (beside) (beside)))
  (check-true (gui=? (above) (above)))
  (check-true (gui=? (above (text "foo") (text "bar"))
                     (above (text "foo") (text "bar"))))
  (check-false (gui=? (above (text "foo") (text "bar"))
                      (beside (text "foo") (text "bar")))))
