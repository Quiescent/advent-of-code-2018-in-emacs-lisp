;;; day24 --- My solution to day24 -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day24

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defvar run-from-batch nil
  "Whether this was run froom batch.

We only want long running executions to be done from the terminal
so that Emacs doesn't hang.")

;; ==========TIE BREAKS==========
;; 
;; 1. In decreasing order of effective power, groups choose their
;; targets; in a tie, the group with the higher initiative chooses
;; first.
;;
;; 2. If an attacking group is considering two defending groups to
;; which it would deal equal damage, it chooses to target the
;; defending group with the largest effective power; if there is still
;; a tie, it chooses the defending group with the highest initiative.
;;
;;

;; ==========Interesting Rules==========
;;
;; 1. If it cannot deal any defending groups damage, it does not
;; choose a target.
;;
;; 2. Defending groups can only be chosen as a target by one attacking
;; group.
;;
;; 3. Groups attack in decreasing order of initiative.
;;
;; 4. If the defending group is immune to the attacking group's attack
;; type, the defending group instead takes no damage.
;;
;; 5. If the defending group is weak to the attacking group's attack
;; type, the defending group instead takes double damage
;;
;; 6. The defending group only loses whole units from damage.
;;
;; 7. Any remaining damage to a unit that does not immediately kill it
;; is ignored.

(defun parse-armies (input-file)
  "Parse the INPUT-FILE into a cons of the immune system and infection army.

An army is:
((INITIATIVE
  DAMAGE
  DAMAGE-TYPE
  UNIT-COUNT
  HIT-POINTS
  (IMMUNITY...)
  (WEAKNESS...))...))"
  (let* ((without-immune-heading (substring input-file (1+ (cl-position ?\n input-file))))
         (army-split             (split-string without-immune-heading "\nInfection:\n" t " ")))
    (cons (parse-army (car  army-split))
          (parse-army (cadr army-split)))))

(require 'subr-x)

(defun parse-damage-type (str)
  "Parse the damage type from STR."
  (progn
    (string-match "does [0-9]+ \\(.*\\) damage" str)
    (intern (upcase (match-string 1 str)))))

(defun parse-immunities-and-weaknesses (str)
  "Parse a cons of immunities and weaknesses from STR."
  (let* ((modifier-section (substring str
                                      (string-match "(.*)" str)
                                      (cl-position ?\) str)))
         (sub-groups       (split-string modifier-section ";" t " "))
         (immune-first     (cl-search "immune" (car sub-groups)))
         (has-second-group (eq (length sub-groups) 2))
         (first-group      (thread-last (split-string (substring (car sub-groups)
                                                                 (+ 3 (cl-search "to "
                                                                                 (car sub-groups))))
                                                      "," t " ")
                             (cl-mapcar #'upcase)
                             (cl-mapcar #'intern)))
         (second-group     (when has-second-group
                             (thread-last (split-string (substring (cadr sub-groups)
                                                                   (+ 3 (cl-search "to "
                                                                                   (cadr sub-groups))))
                                                        "," t " ")
                               (cl-mapcar #'upcase)
                               (cl-mapcar #'intern))))
         (result           nil))
    (if immune-first
        (progn
          (push second-group result)
          (push first-group result))
        (progn
          (push first-group result)
          (push second-group result)))))

(defun parse-army (army-str)
  "Parse ARMY-STR into the army structure.

See `parse-armies' for structure."
  (cl-mapcar (lambda (line) (let* ((stats       (thread-last (split-string line "[^0-9]" t " ")
                                                  (cl-mapcar #'string-to-number)))
                                   (initiative  (nth 3 stats))
                                   (damage      (nth 2 stats))
                                   (damage-type (parse-damage-type line))
                                   (unit-count  (nth 1 stats))
                                   (hit-points  (nth 2 stats))
                                   (modifiers   (parse-immunities-and-weaknesses line))
                                   (immunities  (car  modifiers))
                                   (weaknesses  (cadr modifiers)))
                              (list initiative
                                    damage
                                    damage-type
                                    unit-count
                                    hit-points
                                    immunities
                                    weaknesses)))
             (cl-loop for lines on (split-string army-str "\n" t " ") by #'cddr
                collect (concat (car lines) (cadr lines)))))

(defun tick (immune-system infection)
  "Advance IMMUNE-SYSTEM and INFECTION by one unit of targetting and attack."
  (cons immune-system infection))

(defun day24-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (let* ((armies        (parse-armies input-file))
         (immune-system (car armies))
         (infection     (cadr armies)))
    (cl-loop while (and (> (length immune-system) 0)
                        (> (length infection)     0))
       for (next-immun-system . next-infection) = (tick immune-system infection)
       do (setq immune-system next-immun-system
                infection     next-infection)
       repeat 5)))

(let* ((test-input    "Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with
 an attack that does 4507 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning,
 slashing) with an attack that does 25 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack
 that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire,
 cold) with an attack that does 12 slashing damage at initiative 4")
       (test-computed (day24-part-1 test-input))
       (test-ans      5216))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; # PART 2:

(defun day24-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  )

(let* ((test-input    "")
       (test-computed (day24-part-2 test-input))
       (test-ans      0))
  (message "Expected: %s\n    Got:      %s" test-ans test-computed))

;; Run the solution:

(when run-from-batch
  (progn
    (message "\n********** OUTPUT **********")
    (let ((input-1 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day24-part-1")
                       (buffer-substring (point-min)
                                         (point-max)))))
          (input-2 (save-window-excursion
                     (with-temp-buffer
                       (find-file-literally "day24-part-1")
                       (buffer-substring (point-min)
                                         (point-max))))))
      (message "Part 1: %s" (day24-part-1 input-1))
      (message "Part 2: %s\n" (day24-part-2 input-2)))))

(provide 'day24)
;;; day24 ends here
