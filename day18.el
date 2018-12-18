;;; day18 --- My solution to day18 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day18

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(defun parse-grid (input-file)
  "Parse INPUT-FILE into a 2D matrix."
  (cl-map 'vector (lambda (line) (cl-map 'vector #'identity line))
          (split-string input-file "\n" t " ")))

(defun transform (map x y)
  "Produce the new square to place into MAP at X, Y."
  (let* ((surrounding (squares-around map x y)))
    (pcase (aref (aref map y) x)
      (?. (if (>= (cl-count ?| surrounding) 3) ?| ?.))
      (?| (if (>= (cl-count ?# surrounding) 3) ?# ?|))
      (?# (if (and (>= (cl-count ?# surrounding) 1)
                   (>= (cl-count ?| surrounding) 1))
              ?#
              ?.)))))

(defun efficient-transform (map x y)
  "Efficiently transform the square in MAP at X, Y."
  (cl-loop for x-iter from (1- x) to (1+ x)
     with dim-x        = (length (aref map 0))
     with dim-y        = (length map)
     with lumber-count = 0
     with mill-count   = 0
     do (cl-loop for y-iter from (1- y) to (1+ y)
           when (and (not (and (eq x-iter x)
                               (eq y-iter y)))
                     (not (or (>= x-iter dim-x)
                              (< x-iter 0)
                              (>= y-iter dim-y)
                              (< y-iter 0))))
             do (pcase (aref (aref map y-iter) x-iter)
                  (?| (cl-incf lumber-count))
                  (?# (cl-incf mill-count))))
     finally return (pcase (aref (aref map y) x)
                      (?. (if (>= lumber-count 3) ?| ?.))
                      (?| (if (>= mill-count   3) ?# ?|))
                      (?# (if (and (>= mill-count   1)
                                   (>= lumber-count 1))
                              ?#
                              ?.)))))

(defun tick-to (map dest)
  "Produce the new state for MAP.

Put the result into DEST."
  (cl-loop for y from 0 below (length map)
     for row = (aref map y)
     do (cl-loop for x from 0 below (length row)
           do (aset (aref dest y) x (efficient-transform map x y)))))

(defun print-map (map)
  "Print MAP."
  (cl-loop for row across map
     do (cl-loop for square across row
           do (princ (char-to-string square)))
     do (princ "\n")))

(defun day18-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let ((map  (parse-grid input-file))
        (dest (parse-grid input-file)))
    (cl-loop repeat 10
       do (tick-to map dest)
       for temp = map
       do (setq map  dest
                dest temp)
       finally return (progn
                        (print-map map)
                        (* (apply #'+ (cl-map 'list (lambda (row) (cl-count ?# row)) map))
                           (apply #'+ (cl-map 'list (lambda (row) (cl-count ?| row)) map)))))))

(let* ((test-input    ".#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.")
       (test-computed (day18-part-1 test-input))
       (test-ans      1147))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; # PART 2:

(defun count-resources (map)
  "Count the resources on MAP."
  (* (apply #'+ (cl-map 'list (lambda (row) (cl-count ?# row)) map))
     (apply #'+ (cl-map 'list (lambda (row) (cl-count ?| row)) map))))

(defun day18-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (let ((map  (parse-grid input-file))
        (dest (parse-grid input-file)))
    (cl-loop repeat 1000
       do (tick-to map dest)
       for temp = map
       do (setq map  dest
                dest temp)
       finally return (count-resources map))))

;; Too high: 221676
;; Correct: 215404

;; Protected for profiling...  Run the inner form.
(when nil
  (let* ((input-file ".#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.")
         (map  (parse-grid input-file))
         (dest (parse-grid input-file)))
    (cl-loop repeat 1000
       do (tick-to map dest)
       for temp = map
       do (setq map  dest
                dest temp)
       finally return (progn
                        (print-map map)
                        (* (apply #'+ (cl-map 'list (lambda (row) (cl-count ?# row)) map))
                           (apply #'+ (cl-map 'list (lambda (row) (cl-count ?| row)) map)))))
    (print-map map)))

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day18-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day18-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day18-part-1 input-1))
      (message "Part 2: %s\n" (day18-part-2 input-2)))))

(provide 'day18)
;;; day18 ends here
