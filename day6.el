;;; day6 --- My solution to day6 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day6

;;; Code:

;; # PART 1:

(require 'cl-lib)
(require 'subr-x)

(defun parse-coords (input-file)
  "Produce a list of coordinates from INPUT-FILE."
  (mapcar (lambda (line) (thread-last (split-string line "[^0-9]" t " ")
                           (mapcar #'string-to-number)))
          (split-string input-file "\n" t " ")))

(defun closest-to (x y coords)
  "Produce the coord which is closest to X, Y in COORDS.

Produce -1 if there's a tie."
  (cl-loop for coord being the elements of (cdr coords)
     using (index i)
     with closest   = (car coords)
     with idx       = 0
     with dist      = (+ (abs (- x (car closest))) (abs (- y (cadr closest))))
     with tied      = nil
     for  next-dist = (+ (abs (- x (car coord)))   (abs (- y (cadr coord))))
     if (< next-dist dist)
       do (setq closest coord
                dist    next-dist
                tied    nil
                idx     (1+ i))
     else if (= next-dist dist)
            do (setq tied t)
     finally return (if tied -1 idx)))

(defconst min-coord 0)
(defconst max-coord 300)

(defun remove-boundary-labels (grid labels)
  "Remove from GRID, those LABELS which are on the edge."
  (cl-loop for i from min-coord to max-coord
     do (setq labels (remove (gethash (cons i min-coord) grid) labels))
     do (setq labels (remove (gethash (cons min-coord i) grid) labels))
     do (setq labels (remove (gethash (cons i max-coord) grid) labels))
     do (setq labels (remove (gethash (cons max-coord i) grid) labels))
     finally return labels))

(defun find-largest-area (grid labels)
  "Find the largest non-infinite area in GRID.

LABELS are the possible labels for areas."
  (let ((adjusted (remove-boundary-labels grid labels))
        (values   (hash-table-values grid)))
    (cl-loop for label in (cdr adjusted)
       with largest-label = (car adjusted)
       with largest-count = (cl-count largest-label values)
       for  next-count    = (cl-count label         values)
       when (> next-count largest-count)
         do (setq largest-count next-count
                  largest-label label)
       finally return largest-count)))

(defun day6-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let ((coords (parse-coords input-file)))
    (cl-loop for i from min-coord to max-coord
       with grid = (make-hash-table :test #'equal)
       do (cl-loop for j from min-coord to max-coord
             do (setf (gethash (cons i j) grid) (closest-to i j coords)))
       finally return (find-largest-area grid
                                         (cl-loop for i from 0 below (length coords)
                                            collect i)))))

;; # PART 2:

(defun manhattan-distance (i j x y)
  "Produce the manhattan distance between (I, J) and (X, Y)."
  (+ (abs (- i x)) (abs (- j y))))

(defun within-10000 (x y coords)
  "Produce t if (X, Y) is within a total of 10000 of all COORDS."
  (cl-loop for coord in coords
     with sum   = 0
     for  (i j) = coord
     do (cl-incf sum (manhattan-distance i j x y))
     when (>= sum 10000)
       return 0
     finally return 1))

(defun day6-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (let ((coords (parse-coords input-file)))
    (cl-loop for i from -15000 below 15000
       sum (cl-loop for j from -15000 below 15000
              sum (within-10000 i j coords)))))

;; Solution: 42250 (with grid version)

;; New version is faster and also gets: 42250

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (save-window-excursion
                   (with-temp-buffer
                     (find-file-literally "day6-part-1")
                     (buffer-substring (point-min)
                                       (point-max)))))
        (input-2 (save-window-excursion
                   (with-temp-buffer
                     (find-file-literally "day6-part-1")
                     (buffer-substring (point-min)
                                       (point-max))))))
    (message "Part 1: %s" (day6-part-1 input-1))
    (message "Part 2: %s\n" (day6-part-2 input-2))))

(provide 'day6)
;;; day6 ends here
