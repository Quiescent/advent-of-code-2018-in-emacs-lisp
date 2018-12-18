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

(defun coordinates-around (x y)
  "Produce the coordinates around coordinate X, Y."
  `((,(1+ x) ,y)
    (,(1+ x) ,(1+ y))
    (,x      ,(1+ y))
    (,(1- x) ,(1+ y))
    (,(1- x) ,y)
    (,(1- x) ,(1- y))
    (,x      ,(1- y))
    (,(1+ x) ,(1- y))))

(require 'subr-x)

(defun squares-around (map x y)
  "Produce the squares in MAP around coordinate X, Y."
  (let ((dim-x (length (aref map 0)))
        (dim-y (length map)))
    (thread-last (cl-remove-if (pcase-lambda (`(,other-x ,other-y))
                                   (or (>= other-x dim-x)
                                       (< other-x 0)
                                       (>= other-y dim-y)
                                       (< other-y 0)))
                               (coordinates-around x y))
      (funcall (lambda (coords)
                 (cl-mapcar (pcase-lambda (`(,other-x ,other-y))
                                (aref (aref map other-y) other-x))
                            coords))))))

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

(require 'seq)

(defun to-matrix (map)
  "Turn MAP as list of lists into a matrix."
  (cl-map 'vector
          (lambda (row) (cl-map 'vector #'identity row))
          map))

(defun tick (map)
  "Produce the new state for MAP."
  (thread-last (seq-map-indexed (lambda (row y)
                                  (seq-map-indexed (lambda (_ x) (transform map x y))
                                                   row))
                                map)
    (to-matrix)))

(defun print-map (map)
  "Print MAP."
  (cl-loop for row across map
     do (cl-loop for square across row
           do (princ (char-to-string square)))
     do (princ "\n")))

(defun day18-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let ((map (parse-grid input-file)))
    (cl-loop repeat 10 do (setq map (tick map))
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

(defun day18-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (let ((map (parse-grid input-file)))
    (cl-loop repeat 1000000000 do (setq map (tick map))
       finally return (progn
                        (print-map map)
                        (* (apply #'+ (cl-map 'list (lambda (row) (cl-count ?# row)) map))
                           (apply #'+ (cl-map 'list (lambda (row) (cl-count ?| row)) map)))))))

(let* ((test-input    "")
       (test-computed (day18-part-2 test-input))
       (test-ans      0))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

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
