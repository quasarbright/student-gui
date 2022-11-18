#lang racket

; TODO make this file ASL, not racket

; A to-do list app using the student GUI library. Supports creating and finishing tasks.

(module+ main)

(require "../main.rkt" #;lang/htdp-advanced)
(define-syntax-rule (check-expect e ...) (void))

(define-struct to-do-item [description finished?])
; A ToDoItem is a (make-to-do-item String Boolean) and represents a task where
; 'description' is a description of the task
; 'finished?' is whether the task has been completed

; Examples:
(define todo1 (make-to-do-item "finish fundies hw" #f))
(define todo2 (make-to-do-item "go to fundies lecture" #t))
(define todo1-finished (make-to-do-item "finish fundies hw" #t))
(define todo3 (make-to-do-item "sleep" #f))

; A WorldState is a [List-of ToDoItem] and represents a to-do list containing tasks where the most recently created item is first.
; CONSTRAINT: No two items may have the same description.
(define empty-todos '())
(define todos1 (list todo1 todo2))
(define todos1-finish (list todo1-finished todo2))
(define todos1-finish-add (list todo3 todo1-finished todo2))

(define-struct on-create [item])
(define-struct on-finish [item])
; An Action is one of:
; (make-on-create ToDoItem)
; (make-on-finish ToDoItem)
; And represents an action a user of the to-do app can perform.

; Examples:
(define action1 (make-on-create todo1))
(define action2 (make-on-finish todo1))
(define action3 (make-on-create todo3))

; WorldState -> WorldState
; Runs the ToDo app with the given initial state
(define (main initial-state)
  (big-bang-ui initial-state
   [to-draw draw-todo-list]
   [on-action on-action]))

; WorldState -> GUI
; Render the to-do list with interactivity for adding and finishing items.
; expected result of drawing todos1-finish-add:
#|
finish fundies hw [finish]
go to fundies lecture (done)
_____________________ [submit]
|#
(define (draw-todo-list items)
  (apply above (cons (draw-create-input #f) (map draw-item items))))

;; META COMMENT: Not sure how testing should work, maybe copy jest. Can't do image comparison like big-bang.
;; Or just have them expect a bogus GUI with dummy handlers for inputs and the check would ignore handlers.
;; If DOM is just a pure, free representation, it can be structurally compared. Jest is probably too complex for
;; fundies students anyway.

;; META COMMENT: Not sure if it will make sense to define GUI constants like images. I'll try to make that work.
;; I guess they can just be pure DOM and big-bang just does DOM -> real gui automatically.

; Any -> GUI
; Render the input to create a new to-do item
#|
______________ [submit]
|#
(define (draw-create-input _)
  (text-input "new item" on-create-input-submit))

; String -> Action
; Dispatch an on-create action when the user submits this text box
(check-expect (on-create-input-submit "finish fundies hw") action1)
(define (on-create-input-submit description) (make-on-create (make-to-do-item description #f)))

; ToDoItem -> GUI
; Render a to-do item and a button to finish it, if it is not already finished.
; Expected result on todo1:
#|
finish fundies hw [finish]
|#
; Expected result on todo2:
#|
go to fundies lecture (done)
|#
(define (draw-item item)
  (beside (text (to-do-item-description item))
          (if (to-do-item-finished? item)
              (text "(done)")
              (button "finish" (make-on-finish item)))))

; Action WorldState -> WorldState
; Handle a user action
(check-expect (on-action action1 (list todo2)) todos1)
(check-expect (on-action action2 todos1) todos1-finish)
(check-expect (on-action action3 todos1-finish) todos1-finish-add)
(define (on-action action items)
  (cond
    [(on-create? action) (handle-on-create action items)]
    [(on-finish? action) (handle-on-finish action items)]))

; Action WorldState -> WorldState
; Add the created item to the front of the to-do list
(check-expect (handle-on-create action1 (list todo2)) todos1)
(check-expect (handle-on-create action3 todos1-finish) todos1-finish-add)
(define (handle-on-create action items)
  (cons (on-create-item action) items))

; Action WorldState -> WorldState
; Mark the to-do item as finished. If it is not found, do nothing.
(check-expect (handle-on-finish action2 todos1) todos1-finish)
(check-expect (handle-on-finish action2 (list todo2)) (list todo2))
(define (handle-on-finish action items)
  (let ([item-to-finish (on-finish-item action)])
    ; This will only finish at most one action since descriptions are unique.
    (map (lambda (item) (if (same-description? item item-to-finish) (finish-item item) item)) items)))

; ToDoItem ToDoItem -> Boolean
; Do the items have the same description?
(check-expect (same-description todo1 todo1-finish) #t)
(check-expect (same-description todo1 todo2) #f)
(define (same-description? item1 item2)
  (string=? (to-do-item-description item1) (to-do-item-description item2)))

; ToDoItem -> ToDoItem
; Mark the item as finished. Does nothing if it is already finished.
(check-expect (finish-item todo1) todos1-finish)
(check-expect (finish-item todo1-finish) todos1-finish)
(define (finish-item item)
  (make-to-do-item (to-do-item-description item) #t))


(module+ main
  (main '()))
