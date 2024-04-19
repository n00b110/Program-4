#lang racket

; Game state
(struct game-state (current-location inventory locations items))

; Location
(struct location (name description exits items-present))

; Item
(struct item (name description))

; Define locations
(define location-1 (location "Location 1"
                             "You are in a dimly lit room. There is a door to the east."
                             '(east)
                             '()))

(define location-2 (location "Location 2"
                             "You are in a narrow corridor. There is a door to the west and another to the south."
                             '(west south)
                             '(item-1)))

(define location-3 (location "Location 3"
                             "You are in a small chamber. There is a door to the north and another to the east."
                             '(north east)
                             '(item-2 item-3)))

(define location-4 (location "Location 4"
                             "You are in a large hall. There is a door to the west and another to the south."
                             '(west south)
                             '(item-4)))

; Define items
(define item-1 (item "Rusty Key" "A old, rusty key."))
(define item-2 (item "Torch" "A flickering torch, providing light."))
(define item-3 (item "Rope" "A long, sturdy rope."))
(define item-4 (item "Gem" "A shiny, valuable gem."))

; Initial game state
(define initial-game-state (game-state location-1 '() (list location-1 location-2 location-3 location-4) (list item-1 item-2 item-3 item-4)))

; Helper functions
(define (find-location locations name)
  (findf (lambda (loc) (string=? (location-name loc) name)) locations))

(define (find-item items name)
  (findf (lambda (item) (string=? (item-name item) name)) items))

(define (remove-item items item)
  (remf (lambda (i) (string=? (item-name i) (item-name item))) items))

(define (add-item items item)
  (cons item items))

(define (describe-location loc)
  (printf "~a\n~a\nExits: ~a\n" (location-name loc) (location-description loc) (location-exits loc)))

(define (describe-inventory inv)
  (printf "Inventory: ~a\n" (map item-name inv)))

; Game loop
(define (game-loop game-state)
  (describe-location (game-state-current-location game-state))
  (describe-inventory (game-state-inventory game-state))
  (printf "What would you like to do? (go <direction>, take <item>, drop <item>, search, help)\n")
  (let ([input (read-line)])
    (cond
      [(regexp-match? #rx"^go (.+)$" input)
       (let* ([match (regexp-match #rx"^go (.+)$" input)]
              [direction (string->symbol (cadr match))]
              [current-location (game-state-current-location game-state)]
              [next-location-name (findf (lambda (exit) (eq? direction exit)) (location-exits current-location))])
         (if next-location-name
             (let ([next-location (find-location (game-state-locations game-state) next-location-name)])
               (game-loop (game-state next-location
                                      (game-state-inventory game-state)
                                      (game-state-locations game-state)
                                      (game-state-items game-state))))
             (begin
               (printf "You can't go that way.\n")
               (game-loop game-state))))]
      [(regexp-match? #rx"^take (.+)$" input)
       (let* ([match (regexp-match #rx"^take (.+)$" input)]
              [item-name (cadr match)]
              [current-location (game-state-current-location game-state)]
              [item (find-item (location-items-present current-location) item-name)])
         (if item
             (let ([new-inventory (add-item (game-state-inventory game-state) item)]
                   [new-location-items (remove-item (location-items-present current-location) item)])
               (game-loop (game-state (location current-location
                                               (location-name current-location)
                                               (location-description current-location)
                                               (location-exits current-location)
                                               new-location-items)
                                      new-inventory
                                      (game-state-locations game-state)
                                      (game-state-items game-state))))
             (begin
               (printf "There is no such item in this location.\n")
               (game-loop game-state))))]
      [(regexp-match? #rx"^drop (.+)$" input)
       (let* ([match (regexp-match #rx"^drop (.+)$" input)]
              [item-name (cadr match)]
              [current-location (game-state-current-location game-state)]
              [item (find-item (game-state-inventory game-state) item-name)])
         (if item
             (let ([new-inventory (remove-item (game-state-inventory game-state) item)]
                   [new-location-items (add-item (location-items-present current-location) item)])
               (game-loop (game-state (location current-location
                                               (location-name current-location)
                                               (location-description current-location)
                                               (location-exits current-location)
                                               new-location-items)
                                      new-inventory
                                      (game-state-locations game-state)
                                      (game-state-items game-state))))
             (begin
               (printf "You don't have that item in your inventory.\n")
               (game-loop game-state))))]
      [(string=? input "search")
       (let ([current-location (game-state-current-location game-state)])
         (printf "You search the area and find: ~a\n" (map item-name (location-items-present current-location)))
         (game-loop game-state))]
      [(string=? input "help")
       (let ([current-location (game-state-current-location game-state)])
         (printf "You are currently in ~a.\n" (location-name current-location))
         (describe-inventory (game-state-inventory game-state))
         (printf "Available commands: go <direction>, take <item>, drop <item>, search, help\n")
         (game-loop game-state))]
      [else
       (printf "Invalid command. Type 'help' for a list of available commands.\n")
       (game-loop game-state)])))

; Start the game
(game-loop initial-game-state)
