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
(defun m-buffer-match-data (buffer regexp &optional beginning end post-match)
  "Return a list of `match-data' for all matches in BUFFER to REGEXP.
Only matches between BEGINNING and END are returned. After a
match the function POST-MATCH is called. The buffer is searched
forward."
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
          (reverse rtn))))))

(defun m-buffer-ensure-match (&rest matchers)
  "Ensure that we have match data.
If a single arg, assume it is match data and return. If multiple
args, assume they are of the form \"buffer regexp &optional
beginning end\", and convert them into matches."
  (cond
   ;; we have match data
   ((= 1 (length matchers))
    (car matchers))
   ((< 1 (length matchers))
    (apply 'm-buffer-match-data matchers))
   (t
    (error "Wrong number of arguments"))))

(defun m-buffer-match-beginning-n (n &rest matchers)
  "Return markers to the start of the match to the nth group.
MATCHERS may be of any form accepted by
`m-buffer-ensure-match'. Use `m-buffer-nil-markers' after the
markers have been finished with or they will slow future use of
the buffer."
  (-map
   (lambda (match)
     (nth
      (* 2 n) match))
   (apply 'm-buffer-ensure-match matchers)))

(defun m-buffer-match-beginning-n-pos (n &rest matchers)
  "Return positions of the start of the match to the nth group.
MATCHERS may be of any form accepted by `m-buffer-ensure-match'.
If `match-data' is passed markers will be set to nil after this
function. See `m-buffer-nil-markers' for details."
  (m-buffer-markers-to-pos-nil
   (apply 'm-buffer-match-beginning-n
          n matchers)))

(defun m-buffer-match-beginning (&rest matchers)
  "Returns a list of markers to the start of matches.
MATCHERS may of any form accepted by `m-buffer-ensure-match'. Use
`m-buffer-nil-markers' after the markers have been used or they
will slow future changes to the buffer."
  (apply 'm-buffer-match-beginning-n 0 matchers))

(defun m-buffer-match-beginning-pos (&rest matchers)
  "Returns a list of positions at the start of matcher.
MATCHERS may be of any form accepted by `m-buffer-ensure-match'.
If `match-data' is passed markers will be set to nil after this
function. See `m-buffer-nil-markers' for details."
  (apply 'm-buffer-match-beginning-n-pos 0 matchers))

(defun m-buffer-match-end-n (n &rest matchers)
  "Returns markers to the end of the match to the nth group.
MATCHERS may be of any form accepted by `m-buffer-ensure-match'.
If `match-data' is passed markers will be set to nil after this
function. See `m-buffer-nil-markers' for details."
  (-map
   (lambda (match)
     (nth
      (+ 1 (* 2 n))
      match))
   (apply 'm-buffer-ensure-match matchers)))

(defun m-buffer-match-end-n-pos (n &rest matchers)
  "Return positions of the end of the match to the nth group.
MATCHERS may be of any form accepted by `m-buffer-ensure-match'.
If `match-data' is passed markers will be set to nil after this
function. See `m-buffer-nil-markers' for details."
  (m-buffer-markers-to-pos-nil
   (apply 'm-buffer-match-end-n-pos
          n matchers)))

(defun m-buffer-match-end (&rest matchers)
  "Returns a list of markers to the end of matches to regexp in buffer.
MATCHERS may be of any form accepted by `m-buffer-ensure-match'.
Use `m-buffer-nil-markers' after the markers have been used or
they will slow future changes to the buffer."
  (apply 'm-buffer-match-end-n 0 matchers))

(defun m-buffer-match-end-pos (&rest matchers)
  "Returns a list of positions to the end of the matches.
MATCHERS may be of any form accepted by `m-buffer-ensure-match'.
If `match-data' is passed markers will be set to nil after this
function. See `m-buffer-nil-markers' for details."
  (m-buffers-markers-to-pos-nil
   (apply 'm-buffer-match-end matchers)))

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
  "In BUFFER ranslates a list of POSITIONS to markers."
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

;;;
;;; Block things detection
;;;
(defun m-buffer-page-match (buffer)
  "Return a list of all page delimiters."
  (m-buffer-match-data
   buffer page-delimiter))

(defun m-buffer-paragraph-separate (buffer)
  (m-buffer-match-data
   buffer paragraph-separate nil nil
   'm-buffer-post-match-forward-line))

(defun m-buffer-line-start (buffer)
  (m-buffer-match-beginning
   buffer "^" nil nil
   'm-buffer-post-match-forward-char))

(defun m-buffer-line-end (buffer)
  (m-buffer-match-beginning
   buffer "$" nil nil
   'm-buffer-post-match-forward-char))

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
