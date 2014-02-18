;;; m-buffer.el --- Buffer Manipulation Functions -*- lexical-binding: t -*-

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Version: 0.2-SNAPSHOT

;; The contents of this file are subject to the GPL License, Version 3.0.
;;
;; Copyright (C) 2014, Phillip Lord, Newcastle University
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file provides a set of list orientated functions for operating over
;; the contents of buffers. Functions are generally purish: i.e. they may
;; change the state of one buffer by side-effect, but should not affect point,
;; current buffer, match data or so forth. Generally, markers are returned
;; rather than point locations, so that it is possible for example, to search
;; for regexp matches, and then replace them all without the early replacement
;; invalidating the location of the later ones.
;;

;;; Status:
;;
;; This library is early release at the moment. I write it become I got fed up
;; with writing (while (re-search-forward) do-stuff) forms. I found that it
;; considerably simplified writing `linked-buffer'. I make no guarantees about
;; the API at the moment.

;;; Code:
(require 'dash)

;;
;; Regexp matching
;;
(defun m-buffer-match-data (&rest match-with)
  "Return a list of `match-data' for all matches based on MATCH-WITH.
MATCH-WITh may be of the forms:
BUFFER REGEXP &optional BEGINNING END POST-MATCH
WINDOW REGEXP &optional POST-MATCH

Matches between BEGINNING and END are returned or for windows
matches within the visible portion of the window. After a match
the function POST-MATCH is called. The buffer is searched
forward."
  (let* ((norm (m-buffer-normalize-region match-with))
         (buffer (nth 0 norm))
         (regexp (nth 1 norm))
         (beginning (nth 2 norm))
         (end (nth 3 norm))
         (post-match (nth 4 norm)))
    (save-match-data
      (save-excursion
        (with-current-buffer
            buffer
          (let ((rtn nil)
                (post-match-return t))
            (goto-char
             (or beginning
                 (point-min)))
            (while
                (and
                 post-match-return
                 (re-search-forward
                  regexp
                  (or end (point-max))
                  t))
              (setq rtn
                    (cons
                     (match-data)
                     rtn))
              (when post-match
                (setq post-match-return (funcall post-match))))
            (reverse rtn)))))))

(defun m-buffer-normalize-region (match-with)
  (cond
   ((bufferp (car match-with))
    match-with)
   ((windowp (car match-with))
    (let ((window (car match-with)))
      (list
       (window-buffer window)
       (cadr match-with)
       (window-start window)
       (window-end window)
       (caadr match-with))))
   (t (error "Invalid arguments"))))

(defun m-buffer-ensure-match (&rest match-on)
  "Ensure that we have match data.
If a single arg, assume it is match data and return. If multiple
args, and the first argument is a buffer, assume MATCH-ON is of
form buffer regexp &optional beginning end. If the first argument
is a window assume MATCH-ON is of form window regexp and search
only in the visible part of the window."
  (cond
   ;; we have match data
   ((= 1 (length match-on))
    (car match-on))
   ((< 1 (length match-on))
    (apply 'm-buffer-match-data match-on))
   (t
    (error "Invalid arguments"))))

(defun m-buffer-match-beginning-n (n &rest match-on)
  "Return markers to the start of the match to the nth group.
MATCH-ON may be of any form accepted by
`m-buffer-ensure-match'. Use `m-buffer-nil-markers' after the
markers have been finished with or they will slow future use of
the buffer."
  (-map
   (lambda (match)
     (nth
      (* 2 n) match))
   (apply 'm-buffer-ensure-match match-on)))

(defun m-buffer-match-beginning-n-pos (n &rest match-on)
  "Return positions of the start of the match to the nth group.
MATCH-ON may be of any form accepted by `m-buffer-ensure-match'.
If `match-data' is passed markers will be set to nil after this
function. See `m-buffer-nil-markers' for details."
  (m-buffer-markers-to-pos-nil
   (apply 'm-buffer-match-beginning-n
          n match-on)))

(defun m-buffer-match-beginning (&rest match-on)
  "Returns a list of markers to the start of matches.
MATCH-ON may of any form accepted by `m-buffer-ensure-match'. Use
`m-buffer-nil-markers' after the markers have been used or they
will slow future changes to the buffer."
  (apply 'm-buffer-match-beginning-n 0 match-on))

(defun m-buffer-match-beginning-pos (&rest match-on)
  "Returns a list of positions at the start of matcher.
MATCH-ON may be of any form accepted by `m-buffer-ensure-match'.
If `match-data' is passed markers will be set to nil after this
function. See `m-buffer-nil-markers' for details."
  (apply 'm-buffer-match-beginning-n-pos 0 match-on))

(defun m-buffer-match-end-n (n &rest match-on)
  "Returns markers to the end of the match to the nth group.
MATCH-ON may be of any form accepted by `m-buffer-ensure-match'.
If `match-data' is passed markers will be set to nil after this
function. See `m-buffer-nil-markers' for details."
  (-map
   (lambda (match)
     (nth
      (+ 1 (* 2 n))
      match))
   (apply 'm-buffer-ensure-match match-on)))

(defun m-buffer-match-end-n-pos (n &rest match-on)
  "Return positions of the end of the match to the nth group.
MATCH-ON may be of any form accepted by `m-buffer-ensure-match'.
If `match-data' is passed markers will be set to nil after this
function. See `m-buffer-nil-markers' for details."
  (m-buffer-markers-to-pos-nil
   (apply 'm-buffer-match-end-n-pos
          n match-on)))

(defun m-buffer-match-end (&rest match-on)
  "Returns a list of markers to the end of matches to regexp in buffer.
MATCH-ON may be of any form accepted by `m-buffer-ensure-match'.
Use `m-buffer-nil-markers' after the markers have been used or
they will slow future changes to the buffer."
  (apply 'm-buffer-match-end-n 0 match-on))

(defun m-buffer-match-end-pos (&rest match-on)
  "Returns a list of positions to the end of the matches.
MATCH-ON may be of any form accepted by `m-buffer-ensure-match'.
If `match-data' is passed markers will be set to nil after this
function. See `m-buffer-nil-markers' for details."
  (m-buffers-markers-to-pos-nil
   (apply 'm-buffer-match-end match-on)))

;; marker/position utility functions
(defun m-buffer-nil-markers (markers)
  "Takes a (nested) list of markers and nils them all.
Markers slow buffer movement while they are pointing at a
specific location, until they have been garbage collected. Niling
them prevents this. See Info node `(elisp) Overview of Markers'."
  (-map
   (lambda (marker)
     (set-marker marker nil))
   (-flatten markers)))

(defun m-buffer-markers-to-pos (markers &optional postnil)
  "Transforms a list of markers to a list of positions.
If the markers are no longer needed, set postnil to true, or call
`m-buffer-nil-markers' manually after use to speed future buffer
movement. Or use `m-buffer-markers-to-pos-nil'."
  (-map
   (lambda (marker)
     (prog1
         (marker-position marker)
       (when postnil
         (set-marker marker nil))))
   markers))

(defun m-buffer-markers-to-pos-nil (markers)
  "Transforms a list of MARKERS to a list of positions then nils.
See also `m-buffer-nil-markers'"
  (m-buffer-markers-to-pos markers t))

(defun m-buffer-pos-to-markers (buffer positions)
  "In BUFFER translates a list of POSITIONS to markers."
  (-map
   (lambda (pos)
     (set-marker
      (make-marker) pos buffer))
   positions))

(defun m-buffer-replace-match (matches replacement &optional subexp)
  "Given a list of MATCHES, replace with REPLACEMENT.
SUBEXP should be a number indicating the regexp group to replace."
  (-map
   (lambda (match)
     (with-current-buffer
         (marker-buffer (car match))
       (save-match-data
         (set-match-data match)
         (replace-match
          replacement nil nil nil
          (or subexp 0)))))
   matches))


(defun m-buffer-match-string (matches &optional subexp)
  "Given a list of MATCHES return the string matches optionally
of group SUBEXP."
  (-map
   (lambda (match)
     (with-current-buffer
         (marker-buffer (car match))
       (save-match-data
         (set-match-data match)
         (match-string
          (or subexp 0)))))
   matches))

(defun m-buffer-match-string-no-properties (matches &optional subexp)
  "Given a list of MATCHES return string matches without properties
optionally of group SUBEXP."
  (-map
   'substring-no-properties
   (m-buffer-match-string
    matches subexp)))

;;;
;;; Block things detection
;;;
(defun m-buffer-match-data-regexp-first (regexp &rest match-with)
  "Internal convienience function and not part of the API.
The same as m-buffer-match-data but with the regexp first."
  (apply 'm-buffer-match-data
         (-insert-at 1 regexp match-with)))


(defun m-buffer-match-page (&rest match-with)
  "Return a list of match data to all pages in MATCH-WITH.
MATCH-WITH may be in any form accepted by `m-buffer-normalize-region'."
  (apply 'm-buffer-match-data-regexp-first
         page-delimiter match-with))

(defun m-buffer-match-paragraph-separate (&rest match-with)
  "Returns a list of match data to all pages in MATCH-WITH.
MATCH-WITH may be in any form accepted by
`m-buffer-normalize-region'."
  (apply 'm-buffer-match-data-regexp-first
         paragraph-separate match-with))

(defun m-buffer-match-line-start (&rest match-with)
  "Returns a list of matches to all line start."
  (apply 'm-buffer-match-data-regexp-first
         "^"
         (-snoc match-with 'm-buffer-post-match-forward-char)))

(defun m-buffer-match-line-end (&rest match-with)
  "Returns a list of matches to line end."
  (apply 'm-buffer-match-data-regexp-first
         "$"
         (-snoc match-with
                'm-buffer-post-match-forward-char)))

(defun m-buffer-match-sentence-end (&rest match-with)
  "Returns a list of matches to sentence end."
  (apply 'm-buffer-match-data-regexp-first
         (sentence-end) match-with))

(defun m-buffer-match-word (&rest match-with)
  "Returns a list of matches to all words."
  (apply 'm-buffer-match-data-regexp-first
         "\\\w+" match-with))

;; Useful post-match functions
(defun m-buffer-post-match-forward-line ()
  "Attempts to move forward one line and returns true if succeeds."
  (= 0 (forward-line)))

(defun m-buffer-post-match-forward-char ()
  "Attempts to move forward one char and returns true if succeeds."
  (condition-case e
      (progn
        (forward-char)
        t)
    (error 'end-of-buffer
           nil)))

(provide 'm-buffer)
;;; m-buffer.el ends here
