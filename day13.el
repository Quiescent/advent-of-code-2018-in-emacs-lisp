;;; day13 --- My solution to day13 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day13

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(defun set-tile-to-neighbour (x y grid lines)
  "Set the tile at X, Y to what it would be without a cart in GRID.

Use LINES to determine neighbours."
  (let* ((dim-x    (length (nth 0 lines)))
         (dim-y    (length lines))
         (up       (if (>= (1- y) 0)     (aref (nth (1- y) lines) x)      nil))
         (right    (if (< (1+ x) dim-x) (aref (nth y      lines) (1+ x)) nil))
         (down     (if (< (1+ y) dim-y) (aref (nth (1+ y) lines) x)      nil))
         (left     (if (>= (1- x) 0)     (aref (nth y      lines) (1- x)) nil)))
    (if (and (eq left  ?-)
             (eq right ?-)
             (eq up    ?|)
             (eq down  ?|))
        (aset (aref grid y) x ?+)
        (if (and (or (eq up   ?|)
                     (eq down ?|)
                     (eq up   ?+)
                     (eq down ?+)
                     (eq up   ?\\)
                     (eq down ?\\)
                     (eq up   ?/)
                     (eq down ?/))
                 (not (or (eq left  ?+)
                          (eq right ?+))))
            (aset (aref grid y) x ?|)
            (when (and (or (eq left  ?-)
                           (eq left  ?/)
                           (eq left  ?\\)
                           (eq left  ?+)
                           (eq right ?-)
                           (eq right ?/)
                           (eq right ?\\)
                           (eq right ?+))
                       (not (or (eq up   ?+)
                                (eq down ?+))))
              (aset (aref grid y) x ?-))))))

(defun parse-carts (input-file)
  "Produce a hash map of every cart coord to it's current state in INPUT-FILE.

Also produce the map as an array of arrays."
  (let ((lines (split-string input-file "\n" t)))
    (cl-loop for line in lines
       with y     = 0
       with carts = '()
       with grid  = (make-vector (length lines) nil)
       do (aset grid y (cl-map 'vector #'identity line))
       do (cl-loop for tile across line
             with x = 0
             when (member tile '(?v ?^ ?< ?>))
               do (progn
                    (set-tile-to-neighbour x y grid lines)
                    (push (list x y 0 tile) carts))
             do (cl-incf x))
       do (cl-incf y)
       finally return (list (sort-carts-by-top-then-left carts) grid))))

(defun print-grid (grid)
  "Print GRID to stdout."
  (cl-loop for line across grid
     concat (concat (apply #'string (cl-map 'list #'identity line)) "\n")))

(require 'subr-x)

(defun sort-carts-by-top-then-left (carts)
  "Produce an ordered CARTS from top to bottom left to right."
  (thread-first (cl-sort carts #'< :key #'car)
    (cl-stable-sort #'< :key #'cadr)))

(defun move-cart (grid cart)
  "Using the rails in GRID, move CART on by one."
  (pcase cart
    (`(,x ,y ,state ,type)
      (pcase (list (aref (aref grid y) x) type state)
        (`(?-  ?< ,_) `(,(1- x) ,y      ,state ?<))
        (`(?-  ?> ,_) `(,(1+ x) ,y      ,state ?>))

        (`(?\\ ?> ,_) `(,x      ,(1+ y) ,state ?v))
        (`(?\\ ?< ,_) `(,x      ,(1- y) ,state ?^))
        (`(?\\ ?^ ,_) `(,(1- x) ,y      ,state ?<))
        (`(?\\ ?v ,_) `(,(1+ x) ,y      ,state ?>))

        (`(?|  ?^ ,_) `(,x      ,(1- y) ,state ?^))
        (`(?|  ?v ,_) `(,x      ,(1+ y) ,state ?v))

        (`(?/  ?> ,_) `(,x      ,(1- y) ,state ?^))
        (`(?/  ?< ,_) `(,x      ,(1+ y) ,state ?v))
        (`(?/  ?^ ,_) `(,(1+ x) ,y      ,state ?>))
        (`(?/  ?v ,_) `(,(1- x) ,y      ,state ?<))

        ('(?+  ?> 0)  `(,x      ,(1- y) 1      ?^))
        ('(?+  ?> 1)  `(,(1+ x) ,y      2      ?>))
        ('(?+  ?> 2)  `(,x      ,(1+ y) 0      ?v))

        ('(?+  ?v 0)  `(,(1+ x) ,y      1      ?>))
        ('(?+  ?v 1)  `(,x      ,(1+ y) 2      ?v))
        ('(?+  ?v 2)  `(,(1- x) ,y      0      ?<))

        ('(?+  ?< 0)  `(,x      ,(1+ y) 1      ?v))
        ('(?+  ?< 1)  `(,(1- x) ,y      2      ?<))
        ('(?+  ?< 2)  `(,x      ,(1- y) 0      ?^))

        ('(?+  ?^ 0)  `(,(1- x) ,y      1      ?<))
        ('(?+  ?^ 1)  `(,x      ,(1- y) 2      ?^))
        ('(?+  ?^ 2)  `(,(1+ x) ,y      0      ?>))))))

(defun tick (carts grid)
  "Advance the state of CARTS by one.

Use the GRID to decide where they can go."
  (thread-last (mapcar (apply-partially #'move-cart grid) carts)
    (sort-carts-by-top-then-left)))

(defun carts-collide (carts)
  "Produce the coordinate of collision if any cart collides with another in CARTS."
  (cl-loop for rest on carts
     for cart = (car rest)
     for x    = (car cart)
     for y    = (cadr cart)
     when (cl-find-if (lambda (other-cart)
                        (and (eq x (car  other-cart))
                             (eq y (cadr other-cart))))
                      (cdr rest))
       return (list x y)
     finally return nil))

(defun day13-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (pcase (parse-carts input-file)
    (`(,carts ,grid)
      (progn
        (cl-loop for l from 0 to 1000
           do (setq carts (tick carts grid))
           for collision = (carts-collide carts)
           when collision
             return (format "%s,%s" (car collision) (cadr collision)))))))

(let* ((test-input    "/->-\\        
|   |  /----\\
| /-+--+-\\  |
| | |  | v  |
\\-+-/  \\-+--/
  \\------/   
")
       (test-computed (day13-part-1 test-input))
       (test-ans      "7,3"))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; # PART 2:

(defun day13-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (pcase (parse-carts input-file)
    (`(,carts ,grid)
      (progn
        (cl-loop for l from 0 to 100000
           do (setq carts (tick carts grid))
           for collision = (carts-collide carts)
           do (message "%s" carts)
           do (cl-loop while collision
                 do (message "%s" collision)
                 do (setq carts (cl-remove-if
                                 (lambda (cart)
                                   (cl-search collision cart :start2 0 :end2 2))
                                   carts))
                 do (setq collision (carts-collide carts)))
           when (eq 1 (length carts))
             return (format "%s,%s" (caar carts) (cadar carts))
           finally return carts)))))

(let* ((test-input    "/>-<\\  
|   |  
| /<+-\\
| | | v
\\>+</ |
  |   ^
  \\<->/")
       (test-computed (day13-part-2 test-input))
       (test-ans      "6,4"))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day13-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day13-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day13-part-1 input-1))
      (message "Part 2: %s\n" (day13-part-2 input-2)))))

(provide 'day13)
;;; day13 ends here
