;;; company-fuzzy.el --- Fuzzy matching for `company-mode'.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-08-01 16:54:34

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Fuzzy matching for `company-mode'.
;; Keyword: auto auto-complete complete fuzzy matching
;; Version: 0.3.3
;; Package-Requires: ((emacs "24.3") (company "0.8.12"))
;; URL: https://github.com/jcs090218/company-fuzzy

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Fuzzy matching for `company-mode'.
;;

;;; Code:

(require 'company)


(defgroup company-fuzzy nil
  "Fuzzy matching for `company-mode'."
  :prefix "company-fuzzy-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/company-fuzzy"))


(defcustom company-fuzzy-sorting-backend 'alphabetic
  "Type for sorting/scoring backend."
  :type '(choice (const :tag "none" none)
                 (const :tag "alphabetic" alphabetic)
                 (const :tag "flx" flx))
  :group 'company-fuzzy)

(defcustom company-fuzzy-prefix-ontop t
  "Have the matching prefix ontop."
  :type 'boolean
  :group 'company-fuzzy)

(defcustom company-fuzzy-sorting-function nil
  "Function that gives the candidates and you do your own sorting."
  :type 'function
  :group 'company-fuzzy)


(defvar-local company-fuzzy--backends nil
  "Record down the company local backend in current buffer.")

(defvar-local company-fuzzy--matching-reg ""
  "Record down the company current search reg/characters.")


(defun company-fuzzy--match-char (backend c)
  "Fuzzy match the candidates with character C and current BACKEND."
  (let ((valid-candidates (ignore-errors (funcall backend 'candidates c))))
    (if (and (listp valid-candidates)
             (stringp (nth 0 valid-candidates)))
        valid-candidates
      nil)))

(defun company-fuzzy--match-char-exists-candidates (match-results c)
  "Fuzzy match the existing MATCH-RESULTS with character C."
  (let ((also-match-candidates '())
        (also-match-positions '())
        (candidates (car match-results))
        (match-positions (cdr match-results))
        (index 0))
    (while (< index (length candidates))
      (let* ((cand (nth index candidates))
             (cur-pos (nth index match-positions))
             (pos (if cur-pos (1+ cur-pos) 1))
             (new-pos (string-match-p c cand pos)))
        (when (and (numberp new-pos)
                   (<= pos new-pos))
          (push cand also-match-candidates)
          (push new-pos also-match-positions)))
      (setq index (1+ index)))
    (cons also-match-candidates also-match-positions)))

(defun company-fuzzy--match-string (backend str)
  "Fuzzy match the candidates with string STR and current BACKEND."
  (unless (string= str "")
    (let* ((splitted-c (remove "" (split-string str "")))
           (first-char (nth 0 splitted-c))
           (result-candidates (company-fuzzy--match-char backend first-char))
           (break-it (not result-candidates))
           ;; Record all match position for all candidates, for ordering issue.
           (match-positions '())
           (index 1))
      (while (and (not break-it)
                  (< index (length splitted-c)))
        (let* ((c (nth index splitted-c))
               (match-results
                (company-fuzzy--match-char-exists-candidates (cons result-candidates
                                                                   match-positions)
                                                             c)))
          (setq result-candidates (car match-results))
          (setq match-positions (cdr match-results)))
        (if (= (length result-candidates) 0)
            (setq break-it t)
          (setq index (1+ index))))
      result-candidates)))

(defun company-fuzzy--sort-prefix-ontop (candidates)
  "Sort CANDIDATES that match prefix ontop of all other selection."
  (let ((prefix-matches '()))
    (dolist (cand candidates)
      (when (string-match-p company-fuzzy--matching-reg cand)
        (push cand prefix-matches)
        (setq candidates (remove cand candidates))))
    (setq prefix-matches (reverse prefix-matches))
    (setq prefix-matches (sort prefix-matches (lambda (str1 str2) (< (length str1) (length str2)))))
    (setq candidates (append prefix-matches candidates)))
  candidates)

(defun company-fuzzy--sort-candidates (candidates)
  "Sort all CANDIDATES base on type of sorting backend."
  (cl-case company-fuzzy-sorting-backend
    ('none candidates)
    ('alphabetic (setq candidates (sort candidates #'string-lessp)))
    ('flx
     (require 'flx)
     (let ((scoring-table (make-hash-table))
           (scoring-keys '()))
       (dolist (cand candidates)
         (let* ((scoring (flx-score cand company-fuzzy--matching-reg))
                ;; Ensure score is not `nil'.
                (score (if scoring (nth 0 scoring) 0)))
           ;; For first time access score with hash-table.
           (unless (gethash score scoring-table) (setf (gethash score scoring-table) '()))
           ;; Push the candidate with the target score to hash-table.
           (push cand (gethash score scoring-table))))
       ;; Get all the keys into a list.
       (maphash (lambda (score-key _cand-lst) (push score-key scoring-keys)) scoring-table)
       (setq scoring-keys (sort scoring-keys #'>))  ; Sort keys in order.
       (setq candidates '())  ; Clean up, and ready for final output.
       (dolist (key scoring-keys)
         (let ((cands (sort (gethash key scoring-table) #'string-lessp)))
           (setq candidates (append candidates cands)))))))
  (when company-fuzzy-prefix-ontop
    (setq candidates (company-fuzzy--sort-prefix-ontop candidates)))
  (when (functionp company-fuzzy-sorting-function)
    (setq candidates (funcall company-fuzzy-sorting-function candidates)))
  candidates)

(defun company-fuzzy-all-candidates ()
  "Return the list of all candidates."
  (let ((all-candidates '()))
    (dolist (backend company-fuzzy--backends)
      (let ((temp-candidates nil))
        (setq temp-candidates (company-fuzzy--match-string backend company-fuzzy--matching-reg))
        (when temp-candidates
          (setq all-candidates (append all-candidates temp-candidates))
          (delete-dups all-candidates))))
    all-candidates))


(defun company-fuzzy-all-other-backends (command &optional arg &rest ignored)
  "Backend source for all other backend except this backend, COMMAND, ARG, IGNORED."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-fuzzy-all-other-backends))
    (prefix (and (not (company-in-string-or-comment))
                 (company-grab-symbol)))
    (candidates
     (setq company-fuzzy--matching-reg arg)
     (company-fuzzy-all-candidates))))


(defun company-fuzzy--enable ()
  "Record down all other backend to `company-fuzzy--backends'."
  (unless company-fuzzy--backends
    (setq company-fuzzy--backends company-backends)
    (setq-local company-backends '(company-fuzzy-all-other-backends))
    (setq-local company-transformers (append company-transformers '(company-fuzzy--sort-candidates)))))

(defun company-fuzzy--disable ()
  "Revert all other backend back to `company-backends'."
  (when company-fuzzy--backends
    (setq-local company-backends company-fuzzy--backends)
    (setq company-fuzzy--backends nil)
    (setq-local company-transformers (delq 'company-fuzzy--sort-candidates company-transformers))))


;;;###autoload
(define-minor-mode company-fuzzy-mode
  "Minor mode 'company-fuzzy-mode'."
  :lighter " ComFuz"
  :group company-fuzzy
  (if company-fuzzy-mode
      (company-fuzzy--enable)
    (company-fuzzy--disable)))

(defun company-fuzzy-turn-on-company-fuzzy-mode ()
  "Turn on the 'company-fuzzy-mode'."
  (company-fuzzy-mode 1))

;;;###autoload
(define-globalized-minor-mode global-company-fuzzy-mode
  company-fuzzy-mode company-fuzzy-turn-on-company-fuzzy-mode
  :require 'company-fuzzy)


(declare-function flx-score "ext:flx.el")


(provide 'company-fuzzy)
;;; company-fuzzy.el ends here
