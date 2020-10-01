;;; company-fuzzy.el --- Fuzzy matching for `company-mode'  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-08-01 16:54:34

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Fuzzy matching for `company-mode'.
;; Keyword: auto auto-complete complete fuzzy matching
;; Version: 0.7.4
;; Package-Requires: ((emacs "24.4") (company "0.8.12") (s "1.12.0"))
;; URL: https://github.com/jcs-elpa/company-fuzzy

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
(require 'cl-lib)
(require 'ffap)
(require 's)
(require 'subr-x)

(defgroup company-fuzzy nil
  "Fuzzy matching for `company-mode'."
  :prefix "company-fuzzy-"
  :group 'company
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/company-fuzzy"))

(defcustom company-fuzzy-sorting-backend 'alphabetic
  "Type for sorting/scoring backend."
  :type '(choice (const :tag "none" none)
                 (const :tag "alphabetic" alphabetic)
                 (const :tag "flx" flx))
  :group 'company-fuzzy)

(defcustom company-fuzzy-prefix-on-top t
  "Have the matching prefix on top."
  :type 'boolean
  :group 'company-fuzzy)

(defcustom company-fuzzy-sorting-function nil
  "Function that gives all candidates and let you do your own sorting."
  :type '(choice (const :tag "None" nil)
                 function)
  :group 'company-fuzzy)

(defcustom company-fuzzy-sorting-score-function nil
  "Function that gives candidates with same score and let you do your own sorting."
  :type '(choice (const :tag "None" nil)
                 function)
  :group 'company-fuzzy)

(defcustom company-fuzzy-show-annotation t
  "Show annotation from source."
  :type 'boolean
  :group 'company-fuzzy)

(defcustom company-fuzzy-annotation-format " <%s>"
  "Annotation string format."
  :type 'string
  :group 'company-fuzzy)

(defcustom company-fuzzy-full-input-backends '(company-files)
  "List of backends that takes in full input to do auto completion."
  :type 'list
  :group 'company-fuzzy)

(defcustom company-fuzzy-no-prefix-backends '(company-yasnippet)
  "List of backends that doesn't accept prefix argument."
  :type 'list
  :group 'company-fuzzy)

(defvar-local company-fuzzy--recorded-backends nil
  "Record down company local backends in current buffer.")

(defvar-local company-fuzzy--backends nil
  "Company fuzzy backends we are going to use.")

(defvar-local company-fuzzy--used-full-input-backends '()
  "List of `full-input-backends' backends that are used.")

(defvar-local company-fuzzy--used-no-prefix-backends '()
  "List of `no-prefix-backends' backends that are used.")

(defvar-local company-fuzzy--valid-backends nil
  "Pair data with `company-fuzzy--valid-candidates', for cache searching.")

(defvar-local company-fuzzy--valid-candidates nil
  "Pair data with `company-fuzzy--valid-backends', for cache searching.")

(defvar-local company-fuzzy--prefix ""
  "Record down the company current search reg/characters.")

;;; Mode

(defun company-fuzzy--enable ()
  "Record down all other backend to `company-fuzzy--backends'."
  (unless company-fuzzy--recorded-backends
    (setq company-fuzzy--recorded-backends company-backends)
    (setq company-fuzzy--backends (company-fuzzy--normalize-backend-list company-fuzzy--recorded-backends))
    (let ((backend-lst company-fuzzy--backends))
      (dolist (backend backend-lst)
        (when (company-fuzzy--is-contain-list-symbol company-fuzzy-full-input-backends
                                                     backend)
          (push backend company-fuzzy--used-full-input-backends)
          (setq company-fuzzy--backends (remove backend company-fuzzy--backends)))
        (when (company-fuzzy--is-contain-list-symbol company-fuzzy-no-prefix-backends
                                                     backend)
          (push backend company-fuzzy--used-no-prefix-backends)
          (setq company-fuzzy--backends (remove backend company-fuzzy--backends)))))
    (setq-local company-backends '(company-fuzzy-all-other-backends))
    (setq-local company-transformers (append company-transformers '(company-fuzzy--sort-candidates)))
    (advice-add 'company-fill-propertize :around #'company-fuzzy--company-fill-propertize)))

(defun company-fuzzy--disable ()
  "Revert all other backend back to `company-backends'."
  (when company-fuzzy--recorded-backends
    (setq-local company-backends company-fuzzy--recorded-backends)
    (setq company-fuzzy--recorded-backends nil)
    (setq company-fuzzy--backends nil)
    (setq company-fuzzy--used-full-input-backends '())
    (setq company-fuzzy--used-no-prefix-backends '())
    (setq-local company-transformers (delq 'company-fuzzy--sort-candidates company-transformers))
    (advice-remove 'company-fill-propertize #'company-fuzzy--company-fill-propertize)))

;;;###autoload
(define-minor-mode company-fuzzy-mode
  "Minor mode 'company-fuzzy-mode'."
  :lighter " ComFuz"
  :group company-fuzzy
  (if company-fuzzy-mode (company-fuzzy--enable) (company-fuzzy--disable)))

(defun company-fuzzy-turn-on-company-fuzzy-mode ()
  "Turn on the 'company-fuzzy-mode'."
  (company-fuzzy-mode 1))

;;;###autoload
(define-globalized-minor-mode global-company-fuzzy-mode
  company-fuzzy-mode company-fuzzy-turn-on-company-fuzzy-mode
  :group 'company-fuzzy
  :require 'company-fuzzy)

;;; Utilies

(defun company-fuzzy--valid-candidates-p (candidates)
  "Return non-nil if CANDIDATES is list of valid candidates."
  (ignore-errors (stringp (nth 0 candidates))))

(defun company-fuzzy--symbol-start ()
  "Return symbol start point from current cursor position."
  (save-excursion (forward-symbol -1) (point)))

(defun company-fuzzy--symbol-prefix ()
  "Return symbol prefix."
  (let ((str (thing-at-point 'symbol))
        (start (company-fuzzy--symbol-start)))
    (if str
        (substring str 0 (- (point) start))
      nil)))

(defun company-fuzzy--string-match-p (regexp string &optional start)
  "Safe way to execute function `string-match-p'.
See function `string-match-p' for arguments REGEXP, STRING and START."
  (or (ignore-errors (string-match-p regexp string start))
      (ignore-errors (string-match-p (regexp-quote regexp) string start))))

(defun company-fuzzy--is-contain-list-string (in-list in-str)
  "Check if a string IN-STR contain in any string in the string list IN-LIST."
  (cl-some #'(lambda (lb-sub-str) (string= lb-sub-str in-str)) in-list))

(defun company-fuzzy--is-contain-list-symbol (in-list in-symbol)
  "Check if a symbol IN-SYMBOL contain in any symbol in the symbol list IN-LIST."
  (cl-some #'(lambda (lb-sub-symbol) (equal lb-sub-symbol in-symbol)) in-list))

(defun company-fuzzy--normalize-backend-list (backends)
  "Normalize all BACKENDS as list."
  (let ((result-lst '()))
    (dolist (backend backends)
      (if (listp backend)
          (let ((index 0))
            (dolist (back backend)
              (when (company-fuzzy--string-match-p "company-" (symbol-name back))
                (push (nth index backend) result-lst))
              (setq index (1+ index))))
        (push backend result-lst)))
    (cl-remove-duplicates result-lst)))

(defun company-fuzzy--call-backend (backend command key)
  "Safely call BACKEND by COMMAND and KEY."
  (ignore-errors (funcall backend command key)))

(defun company-fuzzy--get-backend-by-candidate (candidate)
  "Return the backend symbol by using CANDIDATE as search index."
  (let ((result-backend nil) (break-it nil) (index 0))
    (while (and (not break-it) (< index (length company-fuzzy--valid-backends)))
      (let ((candidates (nth index company-fuzzy--valid-candidates))
            (backend (nth index company-fuzzy--valid-backends)))
        (when (company-fuzzy--is-contain-list-string candidates candidate)
          (setq result-backend backend)
          (setq break-it t)))
      (setq index (1+ index)))
    result-backend))

(defun company-fuzzy-backends ()
  "List of currently used backends."
  (let ((lst company-fuzzy--backends))
    (setq lst (append company-fuzzy--used-full-input-backends lst))
    (setq lst (append company-fuzzy--used-no-prefix-backends lst))
    (sort lst #'string-lessp)))

;;; Documentation

(defun company-fuzzy--doc-as-buffer (candidate)
  "Provide doc by CANDIDATE."
  (let ((backend (company-fuzzy--get-backend-by-candidate candidate)))
    (if (or (string-empty-p candidate) (not backend))
        nil
      (company-fuzzy--call-backend backend 'doc-buffer candidate))))

;;; Annotation

(defun company-fuzzy--get-backend-string (backend)
  "Get BACKEND's as a string."
  (if backend
      (let ((backend-str (symbol-name backend)))
        (s-replace "company-" "" backend-str))
    ""))

(defun company-fuzzy--backend-string (candidate backend)
  "Form the BACKEND string by CANDIDATE."
  (if (and company-fuzzy-show-annotation candidate)
      (let ((backend-str (company-fuzzy--get-backend-string backend)))
        (when (string-empty-p backend-str) (setq backend-str "unknown"))
        (format company-fuzzy-annotation-format backend-str))
    ""))

(defun company-fuzzy--source-anno-string (candidate backend)
  "Return the source annotation string by CANDIDATE and BACKEND."
  (if (and candidate backend)
      (company-fuzzy--call-backend backend 'annotation candidate)
    ""))

(defun company-fuzzy--extract-annotation (candidate)
  "Extract annotation from CANDIDATE."
  (let* ((backend (company-fuzzy--get-backend-by-candidate candidate))
         (backend-str (company-fuzzy--backend-string candidate backend))
         (orig-anno (company-fuzzy--source-anno-string candidate backend)))
    (concat orig-anno backend-str)))

;;; Highlighting

(defun company-fuzzy--company-fill-propertize (fnc &rest args)
  "Highlight the matching characters with original function FNC, and rest ARGS."
  (if company-fuzzy-mode
      (let* ((line (apply fnc args))
             (cur-selection (nth company-selection company-candidates))
             (splitted-section (remove "" (split-string line " ")))
             (process-selection (nth 0 splitted-section))
             (selected (string= cur-selection process-selection))
             (selected-face (if selected
                                'company-tooltip-common-selection
                              'company-tooltip-common))
             (selected-common-face (if selected
                                       'company-tooltip-selection
                                     'company-tooltip))
             (splitted-c (remove "" (split-string company-fuzzy--prefix "")))
             (right-pt (+ (length process-selection) company-tooltip-margin)))
        (font-lock-prepend-text-property 0 right-pt 'face selected-common-face line)
        (dolist (c splitted-c)
          (let ((pos (company-fuzzy--string-match-p c line)))
            (while (and (numberp pos) (< pos right-pt))
              (font-lock-prepend-text-property pos (1+ pos) 'face selected-face line)
              (setq pos (company-fuzzy--string-match-p c line (1+ pos))))))
        line)
    (apply fnc args)))

;;; Fuzzy Matching

(defun company-fuzzy--match-char (backend c)
  "Fuzzy match the candidates with character C and current BACKEND."
  (let ((valid-candidates (company-fuzzy--call-backend backend 'candidates c)))
    (cond ((and (listp valid-candidates) (stringp (nth 0 valid-candidates)))
           valid-candidates)
          (t nil))))

(defun company-fuzzy--match-char-exists-candidates (match-results c)
  "Fuzzy match the existing MATCH-RESULTS with character C."
  (let ((also-match-candidates '()) (also-match-positions '())
        (candidates (car match-results))
        (match-positions (cdr match-results))
        (index 0))
    (while (< index (length candidates))
      (let* ((cand (nth index candidates)) (cur-pos (nth index match-positions))
             (pos (if cur-pos (1+ cur-pos) 1)) (new-pos (company-fuzzy--string-match-p c cand pos)))
        (when (and (numberp new-pos) (<= pos new-pos))
          (push cand also-match-candidates)
          (push new-pos also-match-positions)))
      (setq index (1+ index)))
    (cons also-match-candidates also-match-positions)))

(defun company-fuzzy--match-string (backend str)
  "Fuzzy match the candidates with string STR and current BACKEND."
  (unless (string-empty-p str)
    (let* ((splitted-c (remove "" (split-string str "")))
           (first-char (nth 0 splitted-c))
           (result-candidates (company-fuzzy--match-char backend first-char))
           (break-it (not result-candidates))
           ;; Record all match position for all candidates, for ordering issue.
           (match-positions '())
           (index 1))
      (while (and (not break-it) (< index (length splitted-c)))
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

;;; Sorting / Scoring

(defun company-fuzzy--sort-by-length (candidates)
  "Sort CANDIDATES by length."
  (sort candidates (lambda (str1 str2) (< (length str1) (length str2)))))

(defun company-fuzzy--sort-prefix-on-top (candidates)
  "Sort CANDIDATES that match prefix on top of all other selection."
  (let ((prefix-matches '())
        (check-match-str company-fuzzy--prefix))
    (while (and (= (length prefix-matches) 0) (not (= (length check-match-str) 1)))
      (dolist (cand candidates)
        (when (company-fuzzy--string-match-p check-match-str cand)
          (push cand prefix-matches)
          (setq candidates (remove cand candidates))))
      (setq check-match-str (substring check-match-str 0 (1- (length check-match-str)))))
    (setq prefix-matches (sort prefix-matches #'string-lessp))
    (setq candidates (append prefix-matches candidates)))
  candidates)

(defun company-fuzzy--sort-candidates (candidates)
  "Sort all CANDIDATES base on type of sorting backend."
  (cl-case company-fuzzy-sorting-backend
    ('none candidates)
    ('alphabetic (setq candidates (sort candidates #'string-lessp)))
    ('flx
     (require 'flx)
     (let ((scoring-table (make-hash-table)) (scoring-keys '()))
       (dolist (cand candidates)
         (let* ((scoring (flx-score cand company-fuzzy--prefix))
                ;; Ensure score is not `nil'.
                (score (if scoring (nth 0 scoring) 0)))
           (when scoring
             ;; For first time access score with hash-table.
             (unless (gethash score scoring-table) (setf (gethash score scoring-table) '()))
             ;; Push the candidate with the target score to hash-table.
             (push cand (gethash score scoring-table)))))
       ;; Get all keys, and turn into a list.
       (maphash (lambda (score-key _cand-lst) (push score-key scoring-keys)) scoring-table)
       (setq scoring-keys (sort scoring-keys #'>))  ; Sort keys in order.
       (setq candidates '())  ; Clean up, and ready for final output.
       (dolist (key scoring-keys)
         (let ((cands (gethash key scoring-table)))
           (setq cands (company-fuzzy--sort-by-length cands))  ; sort by length once.
           (when (functionp company-fuzzy-sorting-score-function)
             (setq cands (funcall company-fuzzy-sorting-score-function cands)))
           (setq candidates (append candidates cands)))))))
  (when company-fuzzy-prefix-on-top
    (setq candidates (company-fuzzy--sort-prefix-on-top candidates)))
  (when (functionp company-fuzzy-sorting-function)
    (setq candidates (funcall company-fuzzy-sorting-function candidates)))
  candidates)

;;; Core

(defun company-fuzzy--found-candidates (all-candidates temp-candidates backend)
  "Do stuff after we found TEMP-CANDIDATES.
Apply and return ALL-CANDIDATES.  BACKEND is used for identify valid candidates."
  (when (company-fuzzy--valid-candidates-p temp-candidates)
    (setq all-candidates (append all-candidates temp-candidates))
    (progn  ; Record all candidates by backend as an id.
      (push backend company-fuzzy--valid-backends)
      (push (copy-sequence temp-candidates) company-fuzzy--valid-candidates)))
  all-candidates)

(defun company-fuzzy-all-candidates ()
  "Return the list of all candidates."
  (progn  ; Clean up.
    (setq company-fuzzy--valid-backends '())
    (setq company-fuzzy--valid-candidates '()))
  (let ((all-candidates '()) temp-candidates)
    ;; Full Input
    (dolist (backend company-fuzzy--used-full-input-backends)
      (setq temp-candidates (company-fuzzy--call-backend backend 'candidates company-fuzzy--prefix))
      (setq all-candidates (company-fuzzy--found-candidates
                            all-candidates temp-candidates backend)))
    ;; No Prefix
    (dolist (backend company-fuzzy--used-no-prefix-backends)
      (setq temp-candidates (company-fuzzy--call-backend backend 'candidates ""))
      (setq all-candidates (company-fuzzy--found-candidates
                            all-candidates temp-candidates backend)))
    ;; Normal
    (dolist (backend company-fuzzy--backends)
      ;; NOTE: For normal backends, you can call the same as No Prefix backends.
      ;; But everything is quite slow; hence we use the `company-fuzzy--match-string'
      ;; function to limit the overall matching candidates.
      (setq temp-candidates (company-fuzzy--match-string backend company-fuzzy--prefix))
      (setq all-candidates (company-fuzzy--found-candidates
                            all-candidates temp-candidates backend)))
    (delete-dups all-candidates)))

(defun company-fuzzy--get-prefix ()
  "Set the prefix just right before completion."
  (setq company-fuzzy--prefix (or (ignore-errors (company-fuzzy--symbol-prefix))
                                  (ffap-file-at-point))))

(defun company-fuzzy-all-other-backends (command &optional arg &rest ignored)
  "Backend source for all other backend except this backend, COMMAND, ARG, IGNORED."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-fuzzy-all-other-backends))
    (prefix (company-fuzzy--get-prefix))
    (annotation (company-fuzzy--extract-annotation arg))
    (candidates (company-fuzzy-all-candidates))
    (doc-buffer (company-fuzzy--doc-as-buffer arg))))

(declare-function flx-score "ext:flx.el")

(provide 'company-fuzzy)
;;; company-fuzzy.el ends here
