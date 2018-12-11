;;; day11 --- My solution to day11 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day11

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(defun rack-id (x)
  "Produce the rack id of X."
  (+ 10 x))

(defun initial-power-level (rack y)
  "Produce the initial power level of RACK and Y."
  (* rack y))

(defun with-grid-serial-number (initial serial-number)
  "Produce the power level with the grid SERIAL-NUMBER from INITIAL."
  (+ initial serial-number))

(defun increase-by-product-with-rack (with-serial rack)
  "Increaes the power level WITH-SERIAL using the RACK."
  (* with-serial rack))

(require 'subr-x)

(defun get-pos-2-or-0 (str)
  "Produce the char at idx 2 in STR or ?0."
  (let ((len (length str)))
    (if (>= 2 len)
        ?0
        (aref str (- len 3)))))

(defun keep-hundreds (power-product)
  "Produce the hundreds digit of POWER-PRODUCT."
  (thread-first (number-to-string power-product)
    (get-pos-2-or-0)
    (char-to-string)
    (string-to-number)))

(defun final-power (hundreds-digit)
  "Produce the final power given the HUNDREDS-DIGIT."
  (- hundreds-digit 5))

(defun compute-power (x y serial)
  "Compute the power of the coordinate X, Y using SERIAL."
  (let* ((rack (rack-id x))
         (init (initial-power-level rack y)))
    (thread-first (with-grid-serial-number init serial)
      (increase-by-product-with-rack rack)
      (keep-hundreds)
      (final-power))))

(compute-power 3    5  8)
(compute-power 122 79  57)
(compute-power 101 153 71)

(defun make-grid (n)
  "Make an N x N grid."
  (apply #'vector (cl-loop repeat n collect (make-vector n nil))))

(defun set-grid-at (x y value grid)
  "Set the value of GRID[X][Y] to VALUE."
  (aset (aref grid y) x value))

(defun get-grid-at (x y grid)
  "Get tha value of GRID at X Y."
  (aref (aref grid y) x))

(defun init-if-not-there (x y grid serial)
  "Initialise the value of X Y in GRID using SERIAL if it hasn't been.

Produce that value."
  (or (get-grid-at x y grid)
      (set-grid-at x y (compute-power x y serial) grid)))

(defun value-of-three-by-three-at (x y grid serial)
  "Produce the value of the 3 x 3 square at X Y in GRID.

Use SERIAL to initialise squares."
  (+ (init-if-not-there x       y       grid serial)
     (init-if-not-there (+ 1 x) y       grid serial)
     (init-if-not-there (+ 2 x) y       grid serial)
     (init-if-not-there x       (+ y 1) grid serial)
     (init-if-not-there (+ 1 x) (+ y 1) grid serial)
     (init-if-not-there (+ 2 x) (+ y 1) grid serial)
     (init-if-not-there x       (+ y 2) grid serial)
     (init-if-not-there (+ 1 x) (+ y 2) grid serial)
     (init-if-not-there (+ 2 x) (+ y 2) grid serial)))

(defun value-of-n-by-n-at (n x y grid)
  "Produce the value of the N x N square at X Y in GRID."
  (cl-loop for l from x below (+ x n)
     sum (cl-loop for L from y below (+ y n)
            sum (get-grid-at l L grid))))

(defun initialise-grid (grid serial)
  "Initialise GRID with SERIAL."
  (cl-loop for l from 0 below 300
     do (cl-loop for L from 0 below 300
           do (set-grid-at l L (compute-power l L serial) grid))
     finally return grid))

(defun day11-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop for l from 0 below 277
     with serial = (string-to-number input-file)
     with grid   = (initialise-grid (make-grid 300) serial)
     with best   = 0
     with best-x = 0
     with best-y = 0
     do (cl-loop for L from 0 below 277
           for next = (value-of-n-by-n-at 3 l L grid)
           when (> next best)
             do (setq best   next
                      best-x l
                      best-y L))
     finally return (format "%s,%s" best-x best-y)))

;; (let* ((test-input    "18")
;;        (test-computed (day11-part-1 test-input))
;;        (test-ans      "33,45"))
;;   (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; 243,16

;; # PART 2:

(defun sum-left-edge-values (n x grid)
  "Produce the sum of the values in a column at X of length N in GRID."
  (cl-loop for l from 0 below n
     sum (get-grid-at x l grid)))

(defun sum-right-edge-values (n x grid)
  "Produce the sum of the values in a column at X + (N - 1) length N in GRID."
  (cl-loop for l from 0 below n
     with x-adj = (+ x n)
     sum (get-grid-at x-adj l grid)))

(defun remove-left-edge-and-add-right (n x grid value)
  "Remove sum of the N values in a column at X in GRID from VALUE.

Add those which are N along, in a column to it."
  (+ (- value (sum-left-edge-values n x grid))
     (sum-right-edge-values n x grid)))

(defun sum-top-edge-values (n x y grid)
  "Produce the sum of the values in a row at X, Y of length N in GRID."
  (cl-loop for l from x below (+ n x)
     sum (get-grid-at l y grid)))

(defun sum-bottom-edge-values (n x y grid)
  "Produce the sum of the values in a row at X + (N - 1), Y length N in GRID."
  (cl-loop for l from x below (+ n x)
     with y-adj = (+ y n)
     sum (get-grid-at l y-adj grid)))

(defun remove-top-edge-and-add-bottom (n x y grid value)
  "Remove the sum of the N values in a row at X, Y in GRID of length N from VALUE.

Add those which are (N - 1) down to it."
  (+ (- value (sum-top-edge-values n x y grid)
        (sum-bottom-edge-values n x y grid))))

(defun use-cache-to-enlarge-by-one (x y n grid cache)
  "Use the cache of previous sizes to compute the value of one larger.

N is the larger size."
  (cl-loop for i from 0 below (1- n)
     with sum      = (gethash (list x y (1- n)) cache)
     with bottom-y = (+ y (1- n))
     with y-iter   = y
     with right-x  = (+ x (1- n))
     with x-iter   = x
     do (cl-incf sum
                 (+ (get-grid-at x-iter  bottom-y grid)
                    (get-grid-at right-x y-iter   grid)))
     do (cl-incf x-iter)
     do (cl-incf y-iter)
     finally return (progn
                      (cl-incf sum (get-grid-at right-x bottom-y grid))
                      (puthash (list x y n) sum cache))))

(defun init-cache (cache grid)
  "Initialise the CACHE using GRID."
  (cl-loop for l from 0 below 300
     do (cl-loop for L from 0 below 300
           do (puthash (list l L 1) (get-grid-at l L grid) cache))
     finally return cache))

(defun day11-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (cl-loop for n from 2 to 300
     with serial = (string-to-number input-file)
     with grid   = (initialise-grid (make-grid 300) serial)
     with cache  = (init-cache (make-hash-table :test #'equal) grid)
     with best   = 0
     with best-x = 0
     with best-y = 0
     with best-n = 0
     do (cl-loop for l from 0 below (- 300 n)
           do (cl-loop for L from 0 below (- 300 n)
                 for next = (use-cache-to-enlarge-by-one l L n grid cache)
                 when (> next best)
                   do (setq best   next
                            best-x l
                            best-y L
                            best-n n)))
     do (message "Done with %s" n)
     finally return (format "%s,%s,%s" best-x best-y best-n)))

;; (let* ((test-input    "7857")
;;        (test-computed (day11-part-2 test-input))
;;        (test-ans      "243,16"))
;;   (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day11-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day11-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day11-part-1 input-1))
      (message "Part 2: %s\n" (day11-part-2 input-2)))))

(provide 'day11)
;;; day11 ends here
