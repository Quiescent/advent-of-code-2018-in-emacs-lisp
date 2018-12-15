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
       with elves   = (make-hash-table :test #'equal)
       with goblins = (make-hash-table :test #'equal)
       with map     = (make-vector (length lines) nil)
       do (aset map y (make-vector (length line) t))
       do (cl-loop for char being the elements of line
             using (index x)
             do (pcase char
                  (?# (aset (aref map y) x t))
                  (?. (aset (aref map y) x nil))
                  (?G (progn
                        (aset (aref map y) x nil)
                        (puthash (cons x y) 200 goblins)))
                  (?E (progn
                        (aset (aref map y) x nil)
                        (puthash (cons x y) 200 elves)))))
       finally return (list map elves goblins))))

(require 'subr-x)

(defun health-of-units (units)
  "Produce the remaining health of UNITS."
  (apply #'+ (hash-table-values units)))

(defun manhattan-distance (this that)
  "Produce the manhattan distance between THIS and THAT."
  (pcase (cons this that)
    (`((,this-x . ,this-y) . (,that-x . ,that-y))
      (+ (abs (- this-x that-x))
         (abs (- this-y that-y))))))

(defun sort-coordinates (coordinates)
  "Produce the given COORDINATES sorted by top to bottom left to right."
  (thread-first (cl-stable-sort (cl-subseq coordinates 0) #'< :key #'car)
    (cl-stable-sort #'< :key #'cdr)))

(defun closest-enemy (position enemy-positions enemy-units)
  "Produce the position of the closest enemy to POSITION in ENEMY-POSITIONS.

Ties are broken at distance of 1 by health of unit in ENEMY-UNITS."
  (let* ((sorted-positions (sort-coordinates enemy-positions)))
    (cl-loop for enemy-position in (cdr sorted-positions)
       with closest          = (car sorted-positions)
       with closest-distance = (manhattan-distance position closest)
       for  next-distance    = (manhattan-distance position enemy-position)
       when (or (< next-distance closest-distance)
                (and (eq next-distance 1)
                     (< (gethash enemy-position enemy-units)
                        (gethash closest        enemy-units))))
         do (setq closest          enemy-position
                  closest-distance next-distance)
       finally return closest)))

(defun attack (position units &optional attack-power)
  "Attack the unit at POSITION in UNITS.

Assume attack power of 3, unless the optional argument
ATTACK-POWER is supplied, in which case use it instead."
  (let ((health (gethash position units))
        (power  (or attack-power 3)))
    (if (<= health power)
        (progn
          (remhash position units)
          t)
        (progn
          (puthash position (- health power) units)
          nil))))

(defun available-moves (pos map units enemies)
  "Produce the available moves from POS in MAP.

Squares with UNITS and ENEMIES on them can't be moved to."
  (pcase pos
    (`(,x . ,y)
      (cl-remove-if (lambda (position) (or (gethash position enemies)
                                           (gethash position units)
                                           (aref (aref map (cdr position)) (car position))))
                    (list (cons (1+ x) y)
                          (cons (1- x) y)
                          (cons x      (1+ y))
                          (cons x      (1- y)))))))

(defun destinations (enemies map friendlies)
  "Produce all free squares around ENEMIES in MAP.

Squares on FRIENDLIES cant be destinations."
  (cl-loop for enemy-coord in (hash-table-keys enemies)
     append (available-moves enemy-coord map friendlies enemies)))

(defun expand (heads map units enemies searched)
  "Expand the search from HEADS in MAP avoiding UNITS and ENEMIES.

Disallow searching back to SEARCHED squares."
  (cl-remove-duplicates
   (cl-loop for head in heads
      for next-moves = (thread-first (available-moves head
                                                      map
                                                      units
                                                      enemies)
                         (cl-set-difference searched :test #'equal))
      append next-moves)
   :test #'equal))

(defun found-destination (heads destinations)
  "Produce t if one a head in HEADS is in DESTINATIONS."
  (cl-find-if (lambda (head) (cl-member head destinations :test #'equal)) heads))

(defun best-found-squares (heads destinations)
  "Produce the best found head in HEADS which is in DESTINATIONS."
  (cl-remove-if-not (lambda (pos) (cl-member pos destinations :test #'equal)) heads))

(defun shortest-distance (start map units enemies)
  "Produce the next square from START to an destination in shortest path on MAP.

Destinations with UNITS on them can't be targets.

Squares with ENEMIES on them can't be moved to."
  (cl-labels ((go-one-way (start map units enemies destinations)
                (cl-loop
                   with distance     = 0
                   with best-lengths = (make-hash-table :test 'equal)
                   with searched     = (list start)
                   with heads        = searched
                   initially do (let ((did-find (found-destination heads destinations)))
                                  (when did-find
                                    (let* ((in-range-squares (best-found-squares heads destinations)))
                                      (cl-return (car (sort-coordinates in-range-squares))))))
                   do (cl-mapc (lambda (coord) (puthash coord distance best-lengths)) heads)
                   do (setq heads    (expand heads map units enemies searched)
                            searched (append searched heads))
                   when (eq nil heads)
                     return nil
                   for did-find = (found-destination heads destinations)
                   when did-find
                     do (let* ((in-range-squares (best-found-squares heads destinations)))
                          (cl-return (car (sort-coordinates in-range-squares))))
                   do (cl-incf distance))))
    (let ((best-square-at-enemy (go-one-way start map units enemies
                                            (destinations enemies map units))))
      (go-one-way best-square-at-enemy map enemies units
                  (available-moves start map units enemies)))))

(defun move (position units map enemies)
  "Move the unit at POSITION in UNITS towards the closest reachable of ENEMIES on MAP.

Positions with ENEMIES on them can't be moved to."
  (let* ((unit-to-move   (gethash position units))
         (pos-to-move-to (progn
                           ;; (message "Computing shortest for: %s" position)
                           (shortest-distance position map units enemies))))
    (when (not (null pos-to-move-to))
      (remhash position units)
      (puthash pos-to-move-to unit-to-move units)
      pos-to-move-to)))

(defun tick (map elves goblins &optional elf-power)
  "Play a round of the simulation on MAP with ELVES and GOBLINS.

ELF-POWER, when supplied, sets the elf attack power in attacks
against goblins."
  (let* ((all-positions (thread-first (append (hash-table-keys elves)
                                              (hash-table-keys goblins))
                          (sort-coordinates))))
    (cl-loop
       while all-positions
       for position        = (pop all-positions)
       for in-elves        = (gethash position elves)
       for in-goblins      = (gethash position goblins)
       for units           = (if in-elves elves   goblins)
       for enemies         = (if in-elves goblins elves)
       for enemy-positions = (hash-table-keys enemies)
       when (or in-goblins in-elves)
         do (progn
              (when (null enemy-positions)
                (cl-return t))
              (let* ((closest (closest-enemy position enemy-positions enemies)))
                (when (not (eq 1 (manhattan-distance closest position)))
                  (setq position (or (move position units map enemies) position))
                  (setq closest (closest-enemy position enemy-positions enemies)))
                (when (and position (eq 1 (manhattan-distance closest position)))
                  (when (attack closest enemies (or (and in-elves elf-power) 3))
                    (setq all-positions (remove closest all-positions)))))))))

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
         for early-exit = (tick map elves goblins)
         when early-exit
           do (progn
                (message "Finished early on round: %s with Elf health: %s, Goblin health: %s"
                         round
                         (health-of-units elves)
                         (health-of-units goblins))
                (message "Elves: %s\nGoblins: %s"
                         (hash-table-values elves)
                         (hash-table-values goblins))
                (let* ((elf-health    (health-of-units elves))
                       (goblin-health (health-of-units goblins)))
                  (if (> elf-health 0)
                      (cl-return (* round elf-health))
                      (cl-return (* round goblin-health)))))
         do (cl-incf round)
         finally return (format "No winner in %s rounds\n Elves: %s\n Goblins: %s"
                                round
                                (hash-table-values elves)
                                (hash-table-values goblins))))))

;; Too high: 241707
;; Too high: 239108 (tried off by one)
;; Too low: 228536 (off by one means times by one more)
;; Print out reads:
;;   Finished on round: 89 with health: 2597 & Goblins: ((3 . 47) (3 . 65) (3 . 77) (3 . 200) (3 . 200) (3 . 200) (3 . 134) (3 . 74) (3 . 200) (3 . 200) (3 . 200) (3 . 200) (3 . 200) (3 . 200) (3 . 200) (3 . 200))
;;   Part 1: 228536
;; 89 * 2597 = 231133
;; Wrong answer: 231133

;; With latest adjustments: 228272

;; Latest: 230599
;; with off by one (please...)
;; 2591 * (89 + 1) = 233190 (wrong) :'(

;; Wrong: 227744 (curiously this is the same answer as is given by two
;;   top 100 solutions)

;; Solution is 229798.  Problem was not removing dead coordinates from
;; the turn schedule.

(let* ((test-input    "#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########")
       (test-computed (day15-part-1 test-input))
       (test-ans      nil))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; (let* ((test-input    "######
;; #.G..#
;; #...E#
;; #E...#
;; ######")
;;        (test-computed (day15-part-1 test-input))
;;        (test-ans      nil))
;;   (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; (let* ((test-input    "#######
;; #E..G.#
;; #...#.#
;; #.G.#G#
;; #######")
;;        (test-computed (day15-part-1 test-input))
;;        (test-ans      nil))
;;   (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; (let* ((test-input    "#######
;; #.G...#
;; #...EG#
;; #.#.#G#
;; #..G#E#
;; #.....#
;; #######")
;;        (test-computed (day15-part-1 test-input))
;;        (test-ans      27730))
;;   (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; (let* ((test-input    "#######
;; #E..EG#
;; #.#G.E#
;; #E.##E#
;; #G..#.#
;; #..E#.#
;; #######")
;;        (test-computed (day15-part-1 test-input))
;;        (test-ans      39514))
;;   (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; (let* ((test-input    "#######
;; #E.G#.#
;; #.#G..#
;; #G.#.G#
;; #G..#.#
;; #...E.#
;; #######")
;;        (test-computed (day15-part-1 test-input))
;;        (test-ans      27755))
;;   (message "Expected: %s\n    Got:      %s" test-ans test-computed))

(let* ((test-input    "#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######")
       (test-computed (day15-part-1 test-input))
       (test-ans      28944))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; (let* ((test-input    "#########
;; #G......#
;; #.E.#...#
;; #..##..G#
;; #...##..#
;; #...#...#
;; #.G...G.#
;; #.....G.#
;; #########")
;;        (test-computed (day15-part-1 test-input))
;;        (test-ans      18740))
;;   (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; # PART 2:

(defun copy-hash-table (table)
  "Produce a copy of the given hash TABLE.

Assumes a test of `equal'."
  (cl-loop for key in (hash-table-keys table)
     with result = (make-hash-table :test #'equal)
     do (puthash key (gethash key table) result)
     finally return result))

(defun day15-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (pcase (parse-map-and-units input-file)
    (`(,map ,golden-elves ,golden-goblins)
      (cl-loop for elf-power from 4 to 200
         named main
         with  elf-count = (length (hash-table-keys golden-elves))
         for   elves     = (copy-hash-table golden-elves)
         for   goblins   = (copy-hash-table golden-goblins)
         do (cl-loop repeat 1000
               named inner-simulation
               with  round      = 0
               for   early-exit = (tick map elves goblins elf-power)
               when early-exit
                 do (progn
                      (message "Finished early on round: %s with Elf health: %s, Goblin health: %s"
                               round
                               (health-of-units elves)
                               (health-of-units goblins))
                      (message "Elves: %s\nGoblins: %s"
                               (hash-table-values elves)
                               (hash-table-values goblins))
                      (let* ((elf-health    (health-of-units elves))
                             (goblin-health (health-of-units goblins)))
                        (if (eq elf-count (length (hash-table-keys elves)))
                            (cl-return-from main (* round elf-health))
                            (cl-return-from inner-simulation))))
               do (cl-incf round)
               finally return (format "No winner in %s rounds\n Elves: %s\n Goblins: %s"
                                      round
                                      (hash-table-values elves)
                                      (hash-table-values goblins)))))))

(let* ((test-input    "#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######")
       (test-computed (day15-part-2 test-input))
       (test-ans      6474))
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
