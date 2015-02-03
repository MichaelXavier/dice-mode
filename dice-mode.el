;;; dice-mode.el --- a package for rolling dice in DnD style games

(require 'cl-lib)
(require 'dash)
(require 's)

(defun dice-mode-roll (sides)
  "Roll a dice with the given number of SIDES."
  (interactive)
  (+ 1 (random sides))
  )

(defun dice-mode-roll-many (count sides)
  "Roll the COUNT dice with the given number of SIDES."
  (interactive)
  (let (results)
    (dotimes (_ count results)
      (setq results (cons (dice-mode-roll sides) results)))
    ))


(defun dice-mode-keep-highest (count results)
  "Keep the highest COUNT results in the list. Common with advantage rolls."
  (interactive)
  (-take count (-sort '> results))
  )

(defun dice-mode-keep-lowest (count results)
  "Keep the lowest COUNT results in the list. Common with disadvantage rolls."
  (interactive)
  (-take count (-sort '< results))
  )

(defstruct dice-mode-spec count sides)


(defun dice-mode-parse-roll-string (string)
  "Parses STRING into a dice-mode-roll"
  (interactive)
  (let* ((matches (s-match (rx bol
                               (group (zero-or-more digit))
                               "d"
                               (group (one-or-more digit))
                               eol) string))
         (caps (cdr matches))
         (count (string-to-number (car caps)))
         (sides (string-to-number (car (cdr caps)))))
    (make-dice-mode-spec :count count :sides sides)))

