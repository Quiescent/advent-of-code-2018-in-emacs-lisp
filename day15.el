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

(defun sort-paths-by-first-move (paths)
  "Sort PATHS by their first move in reading order."
  (thread-first (cl-stable-sort paths #'< :key (lambda (path) (car (nth (- (length path) 2) path))))
    (cl-stable-sort #'< :key (lambda (path) (cdr (nth (- (length path) 2) path))))))

(defun sort-paths-by-last-move (paths)
  "Sort PATHS by their first move in reading order."
  (thread-first (cl-stable-sort paths #'< :key (lambda (path) (caar path)))
    (cl-stable-sort #'< :key (lambda (path) (cdar path)))))

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
       when (or (< next-distance closest-distance)
                (and (eq next-distance 1)
                     (< (cdr (gethash enemy-position enemy-units))
                        (cdr (gethash closest        enemy-units)))))
         do (setq closest          enemy-position
                  closest-distance next-distance)
       finally return closest)))

(defun attack (position units)
  "Attack the unit at POSITION in UNITS.

Assume attack power of 3."
  (pcase (gethash position units)
    (`(,x . ,health)
      (progn
        ;;(message "%s -> %s" position (cons x (- health 3)))
        (if (<= health 3)
            (remhash position units)
            (puthash position (cons x (- health 3)) units))))))

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

(defun shortest-distance (start map units enemies)
  "Produce the next square from START to an enemy in shortest path on MAP.

Destinations with UNITS on them can't be targets.

Squares with ENEMIES on them can't be moved to."
  (cl-loop
     with destinations = (destinations enemies map units)
     with explored  = (list start)
     with paths     = (list explored)
     do (sort-paths-by-first-move paths)
     do (setq paths
              (cl-loop for path in paths
                 for current-square = (car path)
                 for next-moves     = (thread-first (available-moves current-square
                                                                     map
                                                                     units
                                                                     enemies)
                                        (cl-set-difference explored :test #'equal)
                                        (sort-coordinates))
                 do (setq explored (append explored next-moves))
                 append (mapcar (lambda (next-pos) (cons next-pos path)) next-moves)))
     when (eq nil paths)
       return nil
     for found = (thread-first
                     (cl-remove-if-not (lambda (pos) (cl-member pos destinations :test #'equal))
                                       paths :key #'car)
                   (sort-paths-by-last-move))
     when found
       return (nth (- (length (car found)) 2) (car found))))

(defun move (position units map enemies)
  "Move the unit at POSITION in UNITS towards the closest reachable of ENEMIES on MAP.

Positions with ENEMIES on them can't be moved to."
  (let* ((unit-to-move   (gethash position units))
         (pos-to-move-to (shortest-distance position map units enemies)))
    (when (not (null pos-to-move-to))
      (remhash position units)
      (puthash pos-to-move-to unit-to-move units)
      pos-to-move-to)))

(defun tick (map elves goblins)
  "Play a round of the simulation on MAP with ELVES and GOBLINS."
  (let* ((all-positions (thread-first (append (hash-table-keys elves)
                                              (hash-table-keys goblins))
                          (sort-coordinates))))
    (cl-block detect-early-exit
      (mapc (lambda (position)
              (let* ((in-elves        (gethash position elves))
                     (in-goblins      (gethash position goblins))
                     (units           (if in-elves elves   goblins))
                     (enemies         (if in-elves goblins elves))
                     (enemy-positions (hash-table-keys enemies)))
                (when (or in-goblins in-elves)
                  (let* ((closest (closest-enemy position enemy-positions enemies)))
                    (when (null closest)
                      (cl-return-from detect-early-exit t))
                    (if (eq 1 (manhattan-distance closest position))
                        (progn
                          ;;(message "%s attacks %s" position closest)
                          (attack closest enemies)
                          nil)
                        (let* ((new-pos     (move position units map enemies))
                               (new-closest (and new-pos
                                                 (closest-enemy new-pos
                                                                enemy-positions
                                                                enemies))))
                          (when (and new-pos
                                     (eq 1 (manhattan-distance new-closest new-pos)))
                            ;;(message "%s attacks %s" new-pos new-closest)
                            (attack new-closest enemies)
                            nil)))))))
            all-positions)
      nil)))

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
         do (message "After: %s rounds..." round)
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

(let* ((test-input    "#######
#E..G.#
#...#.#
#.G.#G#
#######")
       (test-computed (day15-part-1 test-input))
       (test-ans      nil))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

(let* ((test-input    "#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######")
       (test-computed (day15-part-1 test-input))
       (test-ans      27730))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

(let* ((test-input    "#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######")
       (test-computed (day15-part-1 test-input))
       (test-ans      39514))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

(let* ((test-input    "#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######")
       (test-computed (day15-part-1 test-input))
       (test-ans      27755))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

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

(let* ((test-input    "#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########")
       (test-computed (day15-part-1 test-input))
       (test-ans      18740))
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
