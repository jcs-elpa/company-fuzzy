;;; company-fuzzy.el --- Fuzzy matching for `company-mode'  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2025  Shen, Jen-Chieh
;; Created date 2019-08-01 16:54:34

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/company-fuzzy
;; Version: 1.4.0
;; Package-Requires: ((emacs "26.1") (company "0.8.12") (s "1.12.0") (ht "2.0"))
;; Keywords: matching auto-complete complete fuzzy

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

(require 'cl-lib)
(require 'ffap)
(require 'subr-x)

(require 'company)
(require 'ht)
(require 's)

(defgroup company-fuzzy nil
  "Fuzzy matching for `company-mode'."
  :prefix "company-fuzzy-"
  :group 'company
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/company-fuzzy"))

(defcustom company-fuzzy-sorting-backend 'alphabetic
  "Type for sorting/scoring backend."
  :type '(choice (const :tag "none" none)
                 (const :tag "alphabetic" alphabetic)
                 (const :tag "flex" flex)
                 (const :tag "flx" flx)
                 (const :tag "flx-rs" flx-rs)
                 (const :tag "flxy" flxy)
                 (const :tag "fuz-skim" fuz-skim)
                 (const :tag "fuz-clangd" fuz-clangd)
                 (const :tag "fuz-bin-skim" fuz-bin-skim)
                 (const :tag "fuz-bin-clangd" fuz-bin-clangd)
                 (const :tag "liquidmetal" liquidmetal)
                 (const :tag "sublime-fuzzy" sublime-fuzzy))
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

(defcustom company-fuzzy-passthrough-backends nil
  "List of backends that already are fuzzy, so no filtering of candidates is done."
  :type 'list
  :group 'company-fuzzy)

(defcustom company-fuzzy-trigger-symbols '("." "->")
  "List of symbols that allow trigger company when there is no prefix."
  :type 'list
  :group 'company-fuzzy)

(defcustom company-fuzzy-completion-separator "[ \t\r\n]\\|\\_<\\|\\_>"
  "Use to identify the completion unit."
  :type 'string
  :group 'company-fuzzy)

(defcustom company-fuzzy-reset-selection nil
  "If non-nil, reset the selection to default."
  :type 'boolean
  :group 'company-fuzzy)

(defface company-fuzzy-annotation-face
  '((t (:inherit company-tooltip-annotation)))
  "Face for annotation."
  :group 'company-fuzzy)

(defvar-local company-fuzzy--prefix ""
  "Generic prefix.")

(defvar-local company-fuzzy--prefix-first ""
  "Store generic prefix's first character for caching.")

(defvar-local company-fuzzy--backends nil
  "Company fuzzy backends we are going to use.")

(defvar-local company-fuzzy--recorded-backends nil
  "Record down company local backends in current buffer.")

(defvar-local company-fuzzy--is-trigger-prefix-p nil
  "Flag to see if currently completion having a valid prefix.")

(defvar-local company-fuzzy--prefixes (make-hash-table :test 'equal)
  "Map for each backend's prefix.")

(defvar-local company-fuzzy--candidates (make-hash-table :test 'equal)
  "Map for each bakend's candidates.")

;;
;; (@* "External" )
;;

(declare-function flex-score "ext:flex.el")
(declare-function flx-score "ext:flx.el")
(declare-function flx-rs-score "ext:flx-rs.el")
(declare-function flx-rs-load-dyn "ext:flx-rs.el")

(declare-function flxy-score "ext:flxy.el")
(declare-function flxy-load-dyn "ext:flxy.el")

(declare-function fuz-calc-score-skim "ext:fuz.el")
(declare-function fuz-calc-score-clangd "ext:fuz.el")
(declare-function fuz-build-and-load-dymod "ext:fuz.el")

(declare-function fuz-bin-score-skim "ext:fuz-bin.el")
(declare-function fuz-bin-score-clangd "ext:fuz-bin.el")
(declare-function fuz-bin-load-dyn "ext:fuz-bin.el")

(declare-function liquidmetal-score "ext:liquidmetal.el")

(declare-function sublime-fuzzy-score "ext:sublime-fuzzy.el")
(declare-function sublime-fuzzy-load-dyn "ext:sublime-fuzzy.el")

(declare-function company-emmet--prefix "ext:company-emmet.el")

;;
;; (@* "Mode" )
;;

(defun company-fuzzy--init ()
  "Initialize all sorting backends."
  (cl-case company-fuzzy-sorting-backend
    (`flex (require 'flex))
    (`flx (require 'flx))
    (`flx-rs (require 'flx-rs) (flx-rs-load-dyn))
    (`flxy (require 'flxy) (flxy-load-dyn))
    ((or fuz-skim fuz-clangd)
     (require 'fuz)
     (unless (require 'fuz-core nil t) (fuz-build-and-load-dymod)))
    ((or fuz-bin-skim fuz-bin-clangd)
     (require 'fuz-bin) (fuz-bin-load-dyn))
    (`liquidmetal (require 'liquidmetal))
    (`sublime-fuzzy (require 'sublime-fuzzy) (sublime-fuzzy-load-dyn))))

(defun company-fuzzy--enable ()
  "Record down all other backend to `company-fuzzy--backends'."
  (company-fuzzy--init)
  ;; XXX Don't know why, but you need to clear it first to make local
  ;; variables work!
  (ht-clear company-fuzzy--prefixes)
  (ht-clear company-fuzzy--candidates)
  (unless company-fuzzy--recorded-backends
    (setq company-fuzzy--recorded-backends company-backends
          company-fuzzy--backends (company-fuzzy--normalize-backend-list company-fuzzy--recorded-backends))
    (setq-local company-backends '(company-fuzzy-all-other-backends))
    (setq-local company-transformers (append company-transformers '(company-fuzzy--sort-candidates)))
    (advice-add 'company--insert-candidate :before #'company-fuzzy--insert-candidate)
    (advice-add 'company-yasnippet--completions-for-prefix :around #'company-fuzzy-yasnippet--completions-for-prefix))
  (add-hook 'lsp-completion-mode-hook #'company-fuzzy--lsp-after-enabled nil t)
  (add-hook 'eglot-managed-mode-hook #'company-fuzzy--lsp-after-enabled nil t))

(defun company-fuzzy--disable ()
  "Revert all other backend back to `company-backends'."
  (when company-fuzzy--recorded-backends
    (setq-local company-backends company-fuzzy--recorded-backends)
    (setq-local company-transformers (delq 'company-fuzzy--sort-candidates company-transformers))
    (setq company-fuzzy--recorded-backends nil
          company-fuzzy--backends nil)
    (advice-remove 'company--insert-candidate #'company-fuzzy--insert-candidate)
    (advice-remove 'company-yasnippet--completions-for-prefix #'company-fuzzy-yasnippet--completions-for-prefix))
  (remove-hook 'lsp-completion-mode-hook #'company-fuzzy--lsp-after-enabled t)
  (remove-hook 'eglot-managed-mode-hook #'company-fuzzy--lsp-after-enabled t))

;;;###autoload
(define-minor-mode company-fuzzy-mode
  "Minor mode `company-fuzzy-mode'."
  :lighter " ComFuz"
  :group company-fuzzy
  (if company-fuzzy-mode (company-fuzzy--enable) (company-fuzzy--disable)))

(defun company-fuzzy-turn-on-company-fuzzy-mode ()
  "Turn on the `company-fuzzy-mode'."
  (company-fuzzy-mode 1))

;;;###autoload
(define-globalized-minor-mode global-company-fuzzy-mode
  company-fuzzy-mode company-fuzzy-turn-on-company-fuzzy-mode
  :group 'company-fuzzy
  :require 'company-fuzzy)

;;
;; (@* "Utilies" )
;;

(defun company-fuzzy--valid-candidates-p (candidates)
  "Return non-nil if CANDIDATES is list of valid candidates."
  (ignore-errors (stringp (nth 0 candidates))))

(defun company-fuzzy--async-candidates-p (candidates)
  "Return non-nil if CANDIDATES is in async format."
  (when (consp candidates)
    (and (eq (car candidates) :async) (functionp (cdr candidates)))))

(defun company-fuzzy--symbol-start ()
  "Return symbol start point from current cursor position."
  (ignore-errors
    (save-excursion
      (forward-char -1)
      (re-search-backward company-fuzzy-completion-separator)
      (point))))

(defun company-fuzzy--furthest-prefix ()
  "Return the possible furthest (greatest length) prefix."
  (ht-clear company-fuzzy--prefixes)
  (let ((final-len 0) final-prefix)
    (dolist (backend company-fuzzy--backends)
      (when-let* ((prefix (ignore-errors (funcall backend 'prefix))))
        (ht-set company-fuzzy--prefixes backend prefix)
        (when-let* ((len (ignore-errors (length prefix)))
                    ((< final-len len)))
          (setq final-prefix prefix
                final-len len))))
    final-prefix))

(defun company-fuzzy--generic-prefix ()
  "Return the most generic prefix."
  (let ((start (company-fuzzy--symbol-start)))
    (ignore-errors
      (string-trim (buffer-substring-no-properties (or start (point-min)) (point))))))

(defun company-fuzzy--trigger-prefix-p ()
  "Check if current prefix a trigger prefix."
  (member company-fuzzy--prefix company-fuzzy-trigger-symbols))

(defun company-fuzzy--string-match (regexp string &optional start)
  "Safe way to execute function `string-match'.
See function `string-match' for arguments REGEXP, STRING and START."
  (or (ignore-errors (string-match regexp string start))
      (ignore-errors (string-match (regexp-quote regexp) string start))))

(defun company-fuzzy--string-match-p (regexp string &optional start)
  "Safe way to execute function `string-match-p'.
See function `string-match-p' for arguments REGEXP, STRING and START."
  (or (ignore-errors (string-match-p regexp string start))
      (ignore-errors (string-match-p (regexp-quote regexp) string start))))

(defun company-fuzzy--string-prefix-p (prefix string &optional ignore-case)
  "Safe way to execute function `string-prefix-p'.
See function `string-prefix-p' for arguments PREFIX, STRING and IGNORE-CASE."
  (ignore-errors (string-prefix-p prefix string ignore-case)))

(defun company-fuzzy--normalize-backend-list (backends)
  "Normalize all BACKENDS as list."
  (let (result-lst)
    (dolist (backend backends)
      (if (listp backend)
          (let ((index 0))
            (dolist (back backend)
              (when (company-fuzzy--string-prefix-p "company-" (symbol-name back))
                (push (nth index backend) result-lst))
              (setq index (1+ index))))
        (push backend result-lst)))
    (setq result-lst (reverse result-lst))
    (cl-remove-duplicates result-lst)))

(defun company-fuzzy--get-backend-by-candidate (candidate)
  "Return the backend symbol by using CANDIDATE as search index."
  (let ((match (ht-find (lambda (_backend cands)
                          (member candidate cands))
                        company-fuzzy--candidates)))
    (car match)))

(defun company-fuzzy--call-backend (backend command key)
  "Safely call BACKEND by COMMAND and KEY."
  (ignore-errors (funcall backend command key)))

(defun company-fuzzy--backend-command (candidate command)
  "Find the backend from the CANDIDATE then call the COMMAND."
  (unless (string-empty-p candidate)
    (when-let* ((backend (company-fuzzy--get-backend-by-candidate candidate)))
      (company-fuzzy--call-backend backend command candidate))))

;;
;; (@* "Annotation" )
;;

(defun company-fuzzy--get-backend-string (backend)
  "Get BACKEND's as a string."
  (if backend
      (let ((name (symbol-name backend)))
        (setq name (s-replace "company-" "" name)
              name (s-replace "-company" "" name))
        name)
    ""))

(defun company-fuzzy--backend-string (candidate backend)
  "Form the BACKEND string by CANDIDATE."
  (if (and company-fuzzy-show-annotation candidate)
      (let ((backend-str (company-fuzzy--get-backend-string backend)))
        (when (string-empty-p backend-str) (setq backend-str "unknown"))
        (propertize
         (format company-fuzzy-annotation-format backend-str)
         'face 'company-fuzzy-annotation-face))
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

;;
;; (@* "Highlighting" )
;;

(defun company-fuzzy--pre-render (str &optional annotation-p)
  "Prerender color with STR and flag ANNOTATION-P."
  (unless annotation-p
    (let* ((str-len (length str))
           (prefix (company-fuzzy--backend-prefix-candidate str 'match))
           (prefix (company-fuzzy--validate-prefix prefix))
           (selection (or company-selection 0))
           (cur-selection (nth selection company-candidates))
           (splitted-section (remove "" (split-string str " ")))
           (process-selection (nth 0 splitted-section))
           (selected (string= cur-selection process-selection))
           (selected-face (if selected
                              'company-tooltip-common-selection
                            'company-tooltip-common))
           (selected-common-face (if selected
                                     'company-tooltip-selection
                                   'company-tooltip))
           (splitted-c (remove "" (split-string prefix ""))))
      (set-text-properties 0 str-len nil str)
      (font-lock-prepend-text-property 0 str-len 'face selected-common-face str)
      (dolist (c splitted-c)
        (let ((pos (company-fuzzy--string-match-p (regexp-quote c) str)))
          (while (and (numberp pos) (< pos str-len))
            (font-lock-prepend-text-property pos (1+ pos) 'face selected-face str)
            (setq pos (company-fuzzy--string-match-p (regexp-quote c) str (1+ pos))))))))
  str)

;;
;; (@* "Sorting / Scoring" )
;;

(defun company-fuzzy--sort-prefix-on-top (candidates)
  "Sort CANDIDATES that match prefix on top of all other selection."
  (let (prefix-matches prefix)
    (dolist (cand candidates)
      (setq prefix (company-fuzzy--backend-prefix-candidate cand 'match)
            prefix (company-fuzzy--validate-prefix prefix))
      (when (company-fuzzy--string-prefix-p prefix cand)
        (push cand prefix-matches)
        (setq candidates (remove cand candidates))))
    (setq prefix-matches (sort prefix-matches #'string-lessp)
          candidates (append prefix-matches candidates)))
  candidates)

(defun company-fuzzy--sort-candidates-by-function (candidates fnc &optional flip)
  "Sort CANDIDATES with function call FNC.

If optional argument FLIP is non-nil, reverse query and pattern order."
  (let ((scoring-table (make-hash-table :test 'equal)) scoring-keys)
    (dolist (cand candidates)
      (when-let* ((prefix (company-fuzzy--backend-prefix-candidate cand 'match))
                  (scoring (or (equal prefix 'anything)
                               (ignore-errors
                                 (if flip (funcall fnc prefix cand)
                                   (funcall fnc cand prefix)))))
                  (score (cond ((listp scoring) (nth 0 scoring))
                               ((vectorp scoring) (aref scoring 0))
                               ((numberp scoring) scoring)
                               (t 0))))
        (ht-set scoring-table score (push cand (ht-get scoring-table score)))))
    ;; Get all keys, and turn into a list.
    (ht-map (lambda (score-key _cands) (push score-key scoring-keys)) scoring-table)
    (setq scoring-keys (sort scoring-keys #'>)  ; Sort keys in order.
          candidates nil)  ; Clean up, and ready for final output.
    (dolist (key scoring-keys)
      (let ((cands (ht-get scoring-table key)))
        (setq cands (reverse cands))  ; Respect to backend order.
        (when (functionp company-fuzzy-sorting-score-function)
          (setq cands (funcall company-fuzzy-sorting-score-function cands)))
        (setq candidates (append candidates cands)))))
  candidates)

(defun company-fuzzy--sort-candidates (candidates)
  "Sort all CANDIDATES base on type of sorting backend."
  ;; IMPORTANT: Since the command `candidates' will change by `company-mode',
  ;; we manually set the candidates here so we get can consistent result.
  (setq candidates (company-fuzzy--ht-all-candidates))
  (when company-fuzzy-reset-selection
    (setq company-selection company-selection-default))
  ;; Don't score when it start fresh, e.g. completing a function name in Java
  ;; with the . (dot) symbol
  (unless company-fuzzy--is-trigger-prefix-p
    (setq candidates
          (cl-case company-fuzzy-sorting-backend
            (`none candidates)
            (`alphabetic (sort candidates #'string-lessp))
            (`flex
             (company-fuzzy--sort-candidates-by-function candidates #'flex-score))
            (`flx
             (company-fuzzy--sort-candidates-by-function candidates #'flx-score))
            (`flx-rs
             (company-fuzzy--sort-candidates-by-function candidates #'flx-rs-score))
            (`flxy
             (company-fuzzy--sort-candidates-by-function candidates #'flxy-score))
            ((or fuz-skim fuz-clangd)
             (company-fuzzy--sort-candidates-by-function
              candidates (if (eq company-fuzzy-sorting-backend 'fuz-skim)
                             #'fuz-calc-score-skim
                           #'fuz-calc-score-clangd)
              t))
            ((or fuz-bin-skim fuz-bin-clangd)
             (company-fuzzy--sort-candidates-by-function
              candidates (if (eq company-fuzzy-sorting-backend 'fuz-bin-skim)
                             'fuz-bin-score-skim
                           'fuz-bin-score-clangd)
              t))
            (`liquidmetal
             (company-fuzzy--sort-candidates-by-function candidates #'liquidmetal-score))
            (`sublime-fuzzy
             (company-fuzzy--sort-candidates-by-function candidates #'sublime-fuzzy-score t))))
    (when company-fuzzy-prefix-on-top
      (setq candidates (company-fuzzy--sort-prefix-on-top candidates)))
    (when (functionp company-fuzzy-sorting-function)
      (setq candidates (funcall company-fuzzy-sorting-function candidates))))
  candidates)

;;
;; (@* "Completion" )
;;

(defun company-fuzzy--insert-candidate (candidate)
  "Insertion for CANDIDATE."
  (when company-fuzzy-mode
    ;; NOTE: Here we force to change `company-prefix' so the completion
    ;; will do what we expected.
    (let ((backend (company-fuzzy--get-backend-by-candidate candidate)))
      (setq company-prefix (company-fuzzy--backend-prefix backend 'complete)))))

;;
;; (@* "Prefix" )
;;

(defun company-fuzzy--valid-prefix (backend)
  "Guess the current BACKEND prefix."
  (let ((prefix (ht-get company-fuzzy--prefixes backend)))
    (if (stringp prefix) prefix
      (thing-at-point 'symbol))))  ; Fallback

(defun company-fuzzy--validate-prefix (prefix)
  "Validate the PREFIX to proper string."
  (if (stringp prefix)  ; this will handle 'anything symbol type
      prefix ""))

(defun company-fuzzy--backend-prefix-complete (backend)
  "Return prefix for each BACKEND while doing completion.

This function is use when function `company-fuzzy--insert-candidate' is
called.  It returns the current selection prefix to prevent completion
completes in an odd way."
  (cl-case backend
    (`company-paths (company-fuzzy--valid-prefix backend))
    (t (company-fuzzy--backend-prefix backend 'filter))))

(defun company-fuzzy--backend-prefix-filter (backend)
  "Return prefix for each BACKEND while doing the first basic filerting.

This is some what the opposite to function `company-fuzzy--backend-prefix-get'
since it's try get as much candidates as possible, but this function returns
a prefix that can filter out some obvious impossible candidates."
  (cl-case backend
    (`company-capf (let* ((prefix (company-fuzzy--backend-prefix backend 'match))
                          (prefix (company-fuzzy--validate-prefix prefix)))
                     prefix))
    (`company-files (company-fuzzy--valid-prefix backend))
    (`company-paths (company-fuzzy--backend-prefix 'company-files 'match))
    (t (company-fuzzy--backend-prefix backend 'match))))

(defun company-fuzzy--backend-prefix-match (backend)
  "Return prefix for each BACKEND while matching candidates.

This function is use for scoring and matching algorithm.  It returns a prefix
that best describe the current possible candidate.

For instance, if there is a candidate function `buffer-file-name' and with
current prefix `bfn'.  It will just return `bfn' because the current prefix
does best describe the for this candidate."
  (cl-case backend
    ((company-capf) (or (company-fuzzy--valid-prefix backend)
                        'anything))
    (`company-c-headers
     (when-let* ((prefix (ht-get company-fuzzy--prefixes backend)))
       ;; Skip the first < or " symbol
       (substring prefix 1 (length prefix))))
    (`company-files
     ;; NOTE: For `company-files', we will return the last section of the path
     ;; for the best match.
     ;;
     ;; Example, if I have path `/path/to/dir'; then it shall return `dir'.
     (when-let* ((prefix (ht-get company-fuzzy--prefixes backend))
                 (splitted (split-string prefix "/" t))
                 (len-splitted (length splitted))
                 (last (nth (1- len-splitted) splitted)))
       last))
    (`company-paths
     (when-let* ((prefix (ht-get company-fuzzy--prefixes backend)))
       (if (string-suffix-p "/" prefix) 'anything
         (nth 0 (last (split-string prefix "/" t))))))
    (t company-fuzzy--prefix)))

(defun company-fuzzy--backend-prefix-get (backend)
  "Return prefix for each BACKEND while getting candidates.

This function is use for simplify prefix, in order to get as much candidates
as possible for fuzzy work.

For instance, if I have prefix `bfn'; then most BACKEND will not return
function `buffer-file-name' as candidate.  But with this function will use a
letter `b' instead of full prefix `bfn'.  So the BACKEND will return something
that may be relavent to the first character `b'.

P.S.  Not all backend work this way."
  (cl-case backend
    (`company-c-headers
     ;; Skip the < or " symbol for the first character
     (ignore-errors (substring (ht-get company-fuzzy--prefixes backend) 1 2)))
    (`company-files
     (when-let* ((prefix (ht-get company-fuzzy--prefixes backend)))
       (let* ((splitted (split-string prefix "/" t))
              (len-splitted (length splitted))
              (last (nth (1- len-splitted) splitted))
              (new-prefix prefix))
         (when (< 1 len-splitted)
           (setq new-prefix
                 (substring prefix 0 (- (length prefix) (length last)))))
         new-prefix)))
    (`company-paths
     (when-let* ((prefix (ht-get company-fuzzy--prefixes backend)))
       (if (string-suffix-p "/" prefix) prefix
         (file-name-directory prefix))))
    (`company-emmet (company-emmet--prefix))
    (t
     ;; Return an empty string or first character is likely going to return a
     ;; full list of candaidates. And this is what we want.
     (when (ht-get company-fuzzy--prefixes backend)
       company-fuzzy--prefix-first))))

(defun company-fuzzy--backend-prefix-candidate (cand type)
  "Get the backend prefix by CAND and TYPE."
  (let ((backend (company-fuzzy--get-backend-by-candidate cand)))
    (company-fuzzy--backend-prefix backend type)))

(defun company-fuzzy--backend-prefix (backend type)
  "Get the BACKEND prefix by TYPE."
  (cl-case type
    (`complete (company-fuzzy--backend-prefix-complete backend))
    (`filter   (company-fuzzy--backend-prefix-filter backend))
    (`match    (company-fuzzy--backend-prefix-match backend))
    (`get      (company-fuzzy--backend-prefix-get backend))))

;;
;; (@* "Fuzzy Matching" )
;;

(defun company-fuzzy--trim-trailing-re (regex)
  "Trim incomplete REGEX.
If REGEX ends with \\|, trim it, since then it matches an empty string."
  (if (company-fuzzy--string-match "\\`\\(.*\\)[\\]|\\'" regex) (match-string 1 regex) regex))

(defun company-fuzzy--regex-fuzzy (str)
  "Build a regex sequence from STR.
Insert .* between each char."
  (setq str (company-fuzzy--trim-trailing-re str))
  (if (company-fuzzy--string-match "\\`\\(\\^?\\)\\(.*?\\)\\(\\$?\\)\\'" str)
      (concat (match-string 1 str)
              (let ((lst (string-to-list (match-string 2 str))))
                (apply #'concat
                       (cl-mapcar
                        #'concat
                        (cons "" (cdr (mapcar (lambda (c) (format "[^%c\n]*" c))
                                              lst)))
                        (mapcar (lambda (x) (format "\\(%s\\)" (regexp-quote (char-to-string x))))
                                lst))))
              (match-string 3 str))
    str))

(defun company-fuzzy--match-string (prefix candidates)
  "Return new CANDIDATES that match PREFIX."
  (when (stringp prefix)
    (let ((fuz-str (company-fuzzy--regex-fuzzy prefix)) new-cands)
      (dolist (cand candidates)
        (when (company-fuzzy--string-match-p fuz-str cand)
          (push cand new-cands)))
      new-cands)))

;;
;; (@* "Core" )
;;

(defun company-fuzzy--ht-all-candidates ()
  "Return all candidates from the data."
  (let (all-candidates)
    (ht-map (lambda (_backend cands)
              (setq all-candidates (append all-candidates cands)))
            company-fuzzy--candidates)
    (delete-dups all-candidates)))

(defun company-fuzzy-all-candidates ()
  "Return the list of all candidates."
  (ht-clear company-fuzzy--candidates)  ; Clean up
  (setq company-fuzzy--is-trigger-prefix-p (company-fuzzy--trigger-prefix-p))
  (dolist (backend company-fuzzy--backends)
    (if (or (company-fuzzy--lsp-passthrough backend)
            (memq backend company-fuzzy-passthrough-backends))
        (company-fuzzy--candidates-from-passthrough-backend backend)
      (company-fuzzy--candidates-from-backend backend)))
  ;; Since we insert the candidates before sorting event, see function
  ;; `company-fuzzy--sort-candidates', we return to simply avoid the process
  ;; from `company-mode'.
  ;;
  ;; This should help us save some performance!
  (when (eq this-command 'company-diag)
    ;; We did return candidates here, yet this does not mean `company-diag'
    ;; will respect this result.
    (company-fuzzy--ht-all-candidates)))

(defun company-fuzzy--candidates-from-passthrough-backend (backend)
  "Use candidates of already fuzzy BACKEND as is."
  (let ((prefix-get (company-fuzzy--backend-prefix backend 'get))
        temp-candidates)
    (when prefix-get
      (setq temp-candidates (company-fuzzy--call-backend backend 'candidates prefix-get)))
    (company-fuzzy--collect-candidates backend temp-candidates)))

(defun company-fuzzy--candidates-from-backend (backend)
  "Do fuzzy matching for current BACKEND."
  (let ((prefix-get (company-fuzzy--backend-prefix backend 'get))
        (prefix-fil (company-fuzzy--backend-prefix backend 'filter))
        temp-candidates)
    (when prefix-get
      (setq temp-candidates (company-fuzzy--call-backend backend 'candidates prefix-get)))
    ;; NOTE: Do the very basic filtering for speed up.
    ;;
    ;; The function `company-fuzzy--match-string' does the very first basic
    ;; filtering in order to lower the performance before sending to function
    ;; scoring engine.
    (when (and (not company-fuzzy--is-trigger-prefix-p)
               (company-fuzzy--valid-candidates-p temp-candidates)
               prefix-fil)
      (setq temp-candidates (company-fuzzy--match-string prefix-fil temp-candidates)))
    ;; NOTE: Made the final completion.
    (company-fuzzy--collect-candidates backend temp-candidates)))

(defun company-fuzzy--register-candidates (backend candidates)
  "Register CANDIDATES with BACKEND id."
  (delete-dups candidates)
  (ht-set company-fuzzy--candidates backend (copy-sequence candidates)))

(defun company-fuzzy--collect-candidates (backend candidates)
  "Collect BACKEND's CANDIDATES by it's type."
  (cond
   ;; NOTE: Asynchronous
   ((company-fuzzy--async-candidates-p candidates)
    (ignore-errors
      (funcall (cdr candidates)
               (lambda (async-candidates)
                 (company-fuzzy--register-candidates backend async-candidates)))))
   ;; NOTE: Synchronous
   ;;
   ;; This is the final ensure step before processing it to scoring phase.
   ;; We confirm candidates by adding it to `company-fuzzy--candidates'.
   ;; The function `company-fuzzy--valid-candidates-p' is use to ensure the
   ;; candidates returns a list of strings, which this is the current only valid
   ;; type to this package.
   ((company-fuzzy--valid-candidates-p candidates)
    (company-fuzzy--register-candidates backend candidates))))

(defun company-fuzzy--get-prefix ()
  "Set the prefix just right before completion."
  (setq company-fuzzy--is-trigger-prefix-p nil
        company-fuzzy--prefix (or (ignore-errors (company-fuzzy--furthest-prefix))
                                  (ignore-errors (company-fuzzy--generic-prefix))
                                  (ffap-guesser))
        company-fuzzy--prefix-first (ignore-errors (substring company-fuzzy--prefix 0 1)))
  company-fuzzy--prefix)  ; make sure return it

(defun company-fuzzy-all-other-backends (command &optional arg &rest ignored)
  "Backend source for all other backend except this backend, COMMAND, ARG, IGNORED."
  (interactive (list 'interactive))
  (cl-case command
    (`interactive (company-begin-backend 'company-fuzzy-all-other-backends))
    (`prefix (company-fuzzy--get-prefix))
    (`annotation (company-fuzzy--extract-annotation arg))
    (`candidates (company-fuzzy-all-candidates))
    (`pre-render (company-fuzzy--pre-render arg (nth 0 ignored)))
    (t (company-fuzzy--backend-command arg command))))

;;
;; (@* "Users" )
;;

(defun company-fuzzy--ensure-local ()
  "Ensure modified variable effect locally."
  (make-local-variable 'company-fuzzy--backends)
  (make-local-variable 'company-fuzzy--recorded-backends)
  (make-local-variable 'company-backends))

(defun company-fuzzy--backend-organize ()
  "Organize backend after modified the backend list."
  (if company-fuzzy-mode
      (setq company-fuzzy--backends (delete-dups company-fuzzy--backends)
            company-fuzzy--recorded-backends (delete-dups company-fuzzy--recorded-backends))
    (setq company-backends (delete-dups company-backends))))

;;;###autoload
(defun company-fuzzy-backend-add (backend)
  "Safe way to add BACKEND."
  (company-fuzzy--ensure-local)
  (if company-fuzzy-mode
      (progn
        (add-to-list 'company-fuzzy--backends backend t)
        (add-to-list 'company-fuzzy--recorded-backends backend t))
    (add-to-list 'company-backends backend t))
  (company-fuzzy--backend-organize))

;;;###autoload
(defun company-fuzzy-backend-remove (backend)
  "Safe way to remove BACKEND."
  (company-fuzzy--ensure-local)
  (if company-fuzzy-mode
      (progn
        (setq company-fuzzy--backends (cl-remove backend company-fuzzy--backends)
              company-fuzzy--recorded-backends (cl-remove backend company-fuzzy--recorded-backends)))
    (setq company-backends (cl-remove backend company-backends)))
  (company-fuzzy--backend-organize))

(defun company-fuzzy--insert-to (list elm n)
  "Insert into list LIST an element ELM at index N.

If N is 0, ELM is inserted before the first element.

The resulting list is returned.  As the list contents is mutated
in-place, the old list reference does not remain valid."
  (let* ((padded-list (cons nil (copy-sequence list)))
         (c (nthcdr n padded-list)))
    (setcdr c (cons elm (cdr c)))
    (cdr padded-list)))

(defun company-fuzzy--insert-before (list elm new-elm)
  "Add a NEW-ELM to the LIST before ELM."
  (let ((position (or (cl-position elm list :test 'equal) 0)))
    (company-fuzzy--insert-to list new-elm position)))

(defun company-fuzzy--insert-after (list elm new-elm)
  "Add a NEW-ELM to the LIST after ELM."
  (let ((position (or (cl-position elm list :test 'equal) 0)))
    (company-fuzzy--insert-to list new-elm (1+ position))))

;;;###autoload
(defun company-fuzzy-backend-add-before (backend target)
  "Add the BACKEND before the TARGET backend."
  (company-fuzzy--ensure-local)
  (if company-fuzzy-mode
      (setq company-fuzzy--backends
            (company-fuzzy--insert-before company-fuzzy--backends
                                          target backend)
            company-fuzzy--recorded-backends
            (company-fuzzy--insert-before company-fuzzy--recorded-backends
                                          target backend))
    (setq company-backends
          (company-fuzzy--insert-before company-backends
                                        target backend)))
  (company-fuzzy--backend-organize))

;;;###autoload
(defun company-fuzzy-backend-add-after (backend target)
  "Add the BACKEND after the TARGET backend."
  (company-fuzzy--ensure-local)
  (if company-fuzzy-mode
      (setq company-fuzzy--backends
            (company-fuzzy--insert-after company-fuzzy--backends
                                         target backend)
            company-fuzzy--recorded-backends
            (company-fuzzy--insert-after company-fuzzy--recorded-backends
                                         target backend))
    (setq company-backends
          (company-fuzzy--insert-after company-backends
                                       target backend)))
  (company-fuzzy--backend-organize))

;;
;; (@* "Plugins" )
;;

(defun company-fuzzy--lsp-connected-p ()
  "Return non-nil if lsp is connected."
  (or (bound-and-true-p lsp-managed-mode)
      (bound-and-true-p eglot--managed-mode)))

(defun company-fuzzy--lsp-after-enabled (&rest _)
  "Hook run after LSP is enabled."
  (when (company-fuzzy--lsp-connected-p)
    ;; No need to check for `company-fuzzy-mode' is on or not since this
    ;; is hook only added when `company-fuzzy-mode' is on.
    (setq-local company-backends '(company-fuzzy-all-other-backends))))

(defun company-fuzzy--lsp-passthrough (backend)
  "Respect `capf' BACKEND when LSP is available."
  (when (memq backend '(company-capf))
    (company-fuzzy--lsp-connected-p)))

(defun company-fuzzy-yasnippet--completions-for-prefix (fnc &rest args)
  "Wrap around `company-yasnippet--completions-for-prefix' function in order to
get all possible candidates.

Arguments FNC and ARGS are used to apply original operations."
  (when company-fuzzy-mode
    ;; `prefix' came from `company-fuzzy--backend-prefix-get', so we simply
    ;; replace set `key-prefix' to `prefix'.
    (setf (nth 1 args) (nth 0 args)))
  (apply fnc args))

(provide 'company-fuzzy)
;;; company-fuzzy.el ends here
