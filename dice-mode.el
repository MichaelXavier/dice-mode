;;; dice-mode.el --- a package for rolling dice in DnD style games

(require 'cl-lib)
(require 'dash)
(require 's)


(defstruct dice-mode-result
  rolls-before
  rolls-after
  mod
  total)

(defun dice-mode-roll-spec (spec)
  "Roll a dice spec"
  (let* ((rolls-before (dice-mode-roll-many (dice-mode-spec-count spec) (dice-mode-spec-sides spec)))
         (km (dice-mode-spec-keepmode spec))
         (kc (dice-mode-spec-keepcount spec))
         (rolls-after (cond ((string= km "h") (dice-mode-keep-highest kc rolls-before))
                            ((string= km "l") (dice-mode-keep-lowest kc rolls-before))
                            (t rolls-before)))
         (mod (dice-mode-spec-mod spec))
         (total (+ mod (-sum rolls-after))))
    (make-dice-mode-result :rolls-before rolls-before
                           :rolls-after rolls-after
                           :mod mod
                           :total total)))

(defun dice-mode-render (result)
  "Render a roll result into a string"
  (interactive)
  (format "%s => %s + %s => %s"
          (dice-mode-result-rolls-before result)
          (dice-mode-result-rolls-after result)
          (dice-mode-result-mod result)
          (dice-mode-result-total result)))

(defun dice-mode-roll (sides)
  "Roll a dice with the given number of SIDES."
  (interactive)
  (+ 1 (random sides)))

(defun dice-mode-roll-many (count sides)
  "Roll the COUNT dice with the given number of SIDES."
  (interactive)
  (let (results)
    (dotimes (_ count results)
      (setq results (cons (dice-mode-roll sides) results)))))


(defun dice-mode-keep-highest (count results)
  "Keep the highest COUNT results in the list. Common with advantage rolls."
  (interactive)
  (-take count (-sort '> results)))

(defun dice-mode-keep-lowest (count results)
  "Keep the lowest COUNT results in the list. Common with disadvantage rolls."
  (interactive)
  (-take count (-sort '< results)))

(defstruct dice-mode-spec
  name
  (count 1)
  (sides 20)
  (mod 0)
  keepmode
  keepcount)


(defun dice-mode-parse-roll-string (string &optional name)
  "Parses STRING into a dice-mode-roll-spec"
  (interactive)
  (let* ((matches (s-match (rx bol
                               (group (zero-or-more digit))
                               "d"
                               (group (one-or-more digit))
                               (optional (char ?+ ?-) (group (one-or-more digit)))
                               (optional (group (char ?h ?l)) (group (one-or-more digit)))
                               eol) string))
         (caps (cdr matches))
         (count (string-to-number (elt caps 0)))
         (sides (string-to-number (elt caps 1)))
         (mod (string-to-number (or (elt caps 2) "0")))
         (keepmode (elt caps 3))
         (keepcount (if (elt caps 4) (string-to-number (elt caps 4)))))
    (make-dice-mode-spec :name (or name string)
                         :count count
                         :sides sides
                         :mod mod
                         :keepmode keepmode
                         :keepcount keepcount)))


(define-derived-mode dice-mode tabulated-list-mode "dice-mode" "A mode for making dice rolls"
  (setq tabulated-list-format [("Name" 18 nil)
                               ("Dice" 12 nil)
                               ("Result"  10 nil)
                               ])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (setq tabulated-list-entries (dice-mode-load)) ;; need to hook in before tabulated-list-mode dicks with the buffer
  (setq write-contents-hooks '(dice-mode-save))
  (tabulated-list-init-header)
  (tabulated-list-print t)
  )


(defun dice-mode-save ()
  (let ((entries tabulated-list-entries))
    (with-temp-file (buffer-file-name)
      (print entries (current-buffer)))
    t
    ))

(defun dice-mode-load ()
  (ignore-errors (read (buffer-string)))
  )

(defun dice-mode-roll-selected ()
  (interactive)
  (let* ((row (tabulated-list-get-entry))
         (name (elt row 0))
         (dice (elt row 1))
         (spec (dice-mode-parse-roll-string dice name)))
    (tabulated-list-set-col "Result" (dice-mode-render (dice-mode-roll-spec spec)))))


(defun dice-mode-delete-dice ()
  (interactive)
  (let* ((id (tabulated-list-get-id)))
    (setq tabulated-list-entries (-remove (lambda (l) (string= id (car l))) tabulated-list-entries))
    (tabulated-list-print t)
    ))

(defvar-local current-id 0)

(defun dice-mode-add-dice (name specstr)
  (interactive "sName: \nsDice (e.g. 1d20+2l1): ")
  (setq current-id (+ current-id 1))
  (setq tabulated-list-entries
        (-snoc tabulated-list-entries (list (number-to-string current-id)  (vector name specstr "N/A"))))
  (tabulated-list-print t)
  )


(defvar dice-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map (kbd "C-c C-n") 'dice-mode-add-dice)
    (define-key map (kbd "C-c C-k") 'dice-mode-delete-dice)
    (define-key map (kbd "C-c C-r") 'dice-mode-roll-selected)
    map)
  "Local keymap for `dice-mode' buffers.")
