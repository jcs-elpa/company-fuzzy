;;; company-fuzzy.el --- Fuzzy matching for `company-mode'.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-08-01 16:54:34

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Fuzzy matching for `company-mode'.
;; Keyword: auto auto-complete complete fuzzy matching
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Fuzzy matching for `company-mode'.
;;

;;; Code:


(defvar-local jcs-company-backends nil
  "")

(defvar-local jcs-company-matching-reg "")

(defun jcs-company-fuzzy-match-char (backend c)
  "Fuzzy match the candidates with character C."
  (let ((valid-candidates (ignore-errors (funcall backend 'candidates c))))
    (if (and (listp valid-candidates)
             (stringp (nth 0 valid-candidates)))
        valid-candidates
      nil)))

(defun jcs-company-fuzzy-match-char-exists-candidates (candidates c)
  "Fuzzy match the existing CANDIDATES with character C."
  (let ((also-match-candidates '()))
    (dolist (cand candidates)
      (when (string-match-p c cand)
        (push candidates also-match-candidates)))
    also-match-candidates))

(defun jcs-company-fuzzy-match-string (backend str)
  "Fuzzy match the candidates with string STR."
  (let ((result-candidates nil)
        (splitted-c (split-string str "")))
    (dolist (c splitted-c)
      (unless (string= c "")
        (if result-candidates
            (let ((current-candidates (jcs-company-fuzzy-match-char-exists-candidates result-candidates c)))
              (setq result-candidates (append result-candidates current-candidates)))
          (setq result-candidates (jcs-company-fuzzy-match-char backend c)))))
    result-candidates))

(defun jcs-company-all-candidates ()
  "Return the list of all candidates."
  (let ((all-candidates '()))
    (dolist (backend jcs-company-backends)
      (let ((temp-candidates nil))
        (message "--------------------------------")
        (message "backend: %s" backend)
        (setq temp-candidates (jcs-company-fuzzy-match-string backend jcs-company-matching-reg))
        (when temp-candidates
          (setq all-candidates (append all-candidates temp-candidates))
          (delete-dups all-candidates)))
      (message "all-candidates: %s" all-candidates))
    all-candidates))


(defun jcs-company-all-other-backends (command &optional arg &rest ignored)
  "Backend source for all other backend except this backend."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'jcs-company-all-other-backends))
    (prefix (and (not (company-in-string-or-comment))
                 (company-grab-symbol)))
    (candidates
     (setq jcs-company-matching-reg arg)
     (cl-remove-if-not
      (lambda (c)
        (let ((match-reg (if (string= arg "") ".*" (concat "[" arg "]"))))
          ;; NOTE: Fuzzy matching algorithm here.
          (string-match-p match-reg c)))
      (jcs-company-all-candidates)))))

(defun jcs-company-setup ()
  "Record down all other backend."
  (interactive)
  (unless jcs-company-backends
    (setq jcs-company-backends company-backends)
    (setq-local company-backends '(jcs-company-all-other-backends))))


(provide 'company-fuzzy)
;;; company-fuzzy.el ends here
