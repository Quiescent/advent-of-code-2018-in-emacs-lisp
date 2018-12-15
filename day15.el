;;; day15 --- My solution to day15 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day15

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

(defun parse-map-and-units (input-file)
  "Parse a triple of (map, elves, goblins) from INPUT-FILE."
  (let* ((lines (split-string input-file "\n" t " ")))
    (cl-loop for line being the elements of lines
       using (index y)
       with elves   = (make-hash-table :test 'equal)
       with goblins = (make-hash-table :test 'equal)
       with map     = (make-vector (length lines) nil)
       do (aset map y (make-vector (length line) t))
       do (cl-loop for char being the elements of line
             using (index x)
             do (pcase char
                  (?# (aset (aref map y) x t))
                  (?. (aset (aref map y) x nil))
                  (?G (progn
                        (aset (aref map y) x nil)
                        (puthash (cons x y) (cons 3 200) goblins)))
                  (?E (progn
                        (aset (aref map y) x nil)
                        (puthash (cons x y) (cons 3 200) elves)))))
       finally return (list map elves goblins))))

(require 'subr-x)

(defun health-of-units (units)
  "Produce the remaining health of UNITS."
  (apply #'+ (mapcar #'cdr (hash-table-values units))))

(defun manhattan-distance (this that)
  "Produce the manhattan distance between THIS and THAT."
  (pcase (cons this that)
    (`((,this-x . ,this-y) . (,that-x . ,that-y))
      (+ (abs (- this-x that-x))
         (abs (- this-y that-y))))))

(defun sort-coordinates (coordinates)
  "Produce the given COORDINATES sorted by top to bottom left to right."
  (thread-first (cl-sort coordinates #'< :key #'car)
    (cl-stable-sort #'< :key #'cdr)))

(defun closest-enemy (position enemy-positions enemy-units)
  "Produce the position of the closest enemy to POSITION in ENEMY-POSITIONS.

Ties are broken at distance of 1 by health of unit in ENEMY-UNITS."
  (let* ((sorted-positions (sort-coordinates enemy-positions)))
    (cl-loop for enemy-position in (cdr sorted-positions)
       with closest          = (car sorted-positions)
       with closest-distance = (manhattan-distance position closest)
       for  next-distance    = (manhattan-distance position enemy-position)
       when (and (eq next-distance    1)
                 (eq closest-distance 1)
                 (< (cdr (gethash enemy-position enemy-units))
                         (cdr (gethash closest enemy-units))))
         do (setq closest enemy-position
                  closest-distance next-distance)
       when (< next-distance closest-distance)
         do (setq closest          enemy-position
                  closest-distance next-distance)
       finally return closest)))

(defun attack (position units)
  "Attack the unit at POSITION in UNITS.

Assume attack power of 3."
  (pcase (gethash position units)
    (`(,x . ,health)
      (if (<= health 3)
          (remhash position units)
          (puthash position (cons x (- health 3)) units)))))

(defun available-moves (pos map enemies)
  "Produce the available moves from POS in MAP.

Squares with ENEMIES on them can't be moved to."
  (pcase pos
    (`(,x . ,y)
      (cl-remove-if (lambda (position) (or (gethash position enemies)
                                           (aref (aref map (cdr position)) (car position))))
                    (list (cons (1+ x) y)
                          (cons (1- x) y)
                          (cons x      (1+ y))
                          (cons x      (1- y)))))))

(defun shortest-distance (start to map units enemies)
  "Produce the next square from START to TO in shortest path on MAP.

Destinations with UNITS on them can't be targets.

Squares with ENEMIES on them can't be moved to."
  (cl-loop
     with destinations = (let* ((dests (thread-first (available-moves to
                                                                      map
                                                                      enemies)
                                         (sort-coordinates))))
                           (if (eq 1 (manhattan-distance start to))
                               (cl-set-difference dests
                                                  (hash-table-keys units)
                                                  :test #'equal)
                               dests))
     with explored  = (list start)
     with paths     = (list explored)
     do (setq paths
              (cl-loop for path in paths
                 for current-square = (car path)
                 for next-moves     = (thread-first (available-moves current-square
                                                                     map
                                                                     enemies)
                                        (cl-set-difference explored :test #'equal)
                                        (sort-coordinates))
                 do (setq explored (append explored next-moves))
                 append (mapcar (lambda (next-pos) (cons next-pos path)) next-moves)))
        ;; If I can't find a path then don't move... (might be wrong but
        ;; the thing doesn't specify)  Will have to test to see.
     when (eq nil paths)
       return nil
     for found = (cl-remove-if-not (lambda (pos) (cl-member pos destinations :test #'equal))
                    paths :key #'car)
     when found
       return (nth (- (length found) 2) found)))

(defun move (position to units map enemies)
  "Move the unit at POSITION towards TO in UNITS on MAP.

Positions with ENEMIES on them can't be moved to."
  (let* ((unit-to-move (gethash position units)))
    (let* ((pos-to-move-to (shortest-distance position to map units enemies)))
      (when (and (not  (null pos-to-move-to))
                 (null (gethash pos-to-move-to units)))
        (remhash position units)
        (puthash pos-to-move-to unit-to-move units)))))

(defun make-move (map elves goblins position)
  "Move..."
  (let* ((in-elves   (gethash position elves))
         (in-goblins (gethash position goblins)))
    (if in-elves
        (let* ((closest (closest-enemy position (hash-table-keys goblins) goblins)))
          (if (eq 1 (manhattan-distance closest position))
              (attack closest goblins)
              (move position closest elves map goblins)))
        (when in-goblins
          (let* ((closest (closest-enemy position (hash-table-keys elves) elves)))
            (if (eq 1 (manhattan-distance closest position))
                (attack closest elves)
                (move position closest goblins map elves)))))))

(defun tick (map elves goblins)
  "Play a round of the simulation on MAP with ELVES and GOBLINS."
  (let* ((all-positions (thread-first (append (hash-table-keys elves)
                                              (hash-table-keys goblins))
                          (sort-coordinates))))
    (cl-mapc (lambda (position) (make-move map elves goblins position))
             all-positions)))

(defun draw-map (map elves goblins)
  "Drap MAP with ELVES and GOBLINS on it."
  (cl-loop for line being the elements of map
     using (index y)
     do (cl-loop for char being the elements of line
           using (index x)
           for goblin = (gethash (cons x y) goblins)
           for elf    = (gethash (cons x y) elves)
           if goblin
             do (princ "G")
           else if elf
                  do (princ "E")
           else
             do (princ (if char "#" ".")))
     do (princ "\n")))

(defun day15-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (pcase (parse-map-and-units input-file)
    (`(,map ,elves ,goblins)
      (cl-loop repeat 1000
         with round = 0
         do (draw-map map elves goblins)
         for goblins-all-dead = (hash-table-empty-p goblins)
         for elves-all-dead   = (hash-table-empty-p elves)
         when goblins-all-dead
           do (progn
                (message "Finished on round: %s with health: %s & Elves: %s"
                         round
                         (health-of-units elves)
                         (hash-table-values elves))
                (cl-return (* round (health-of-units elves))))
         when elves-all-dead
           do (progn
                (message "Finished on round: %s with health: %s"
                         round
                         (health-of-units goblins))
                (cl-return (* round (health-of-units goblins))))
         do (tick map elves goblins)
         do (cl-incf round)
         finally return (format "No winner in %s rounds\n Elves: %s\n Goblins: %s"
                                round
                                (hash-table-values elves)
                                (hash-table-values goblins))))))

(let* ((test-input    "#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######")
       (test-computed (day15-part-1 test-input))
       (test-ans      36334))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; # PART 2:

(defun day15-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  )

(let* ((test-input    "")
       (test-computed (day15-part-2 test-input))
       (test-ans      0))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day15-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day15-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day15-part-1 input-1))
      (message "Part 2: %s\n" (day15-part-2 input-2)))))

(provide 'day15)
;;; day15 ends here
