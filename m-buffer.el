;;; m-buffer.el --- Buffer Manipulation Functions -*- lexical-binding: t -*-

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Version: 0.1

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


(defun m-buffer-matches-data (buffer regexp &optional beginning end)
  "Return a list of markers to all matches in BUFFER to REGEXP.
After use, call `m-buffer-nil-markers' to ensure that markers no longer
point to anything which may otherwise slow buffer movement down."
  (save-match-data
    (save-excursion
      (with-current-buffer
          buffer
        (let ((rtn nil))
          (goto-char
           (or beginning
               (point-min)))
          (while
              (re-search-forward
               regexp
               (or end (point-max))
               t)
            (setq rtn
                  (cons
                   (match-data)
                   rtn)))
          (reverse rtn))))))

(defun m-buffer-matches-beginning-n (matches n)
  "Given match-data returns beginning of nth group.
Use `m-buffer-matches' to generate matches and
`m-buffer-nil-markers' after the markers have been finished with
or they will slow future use of the buffer."
  (-map
   (lambda (match)
     (nth
      (* 2 n) match))
   matches))

(defun m-buffer-matches-beginning (buffer regexp &optional beginning end)
  "Returns a list of markers to the start of matches to regexp in buffer.
Use `m-buffer-nil-markers' after the markers have been used or
they will slow future changes to the buffer."
  (-map
   (lambda (match-data)
     (car match-data))
   (m-buffer-matches-data buffer regexp beginning end)))

(defun m-buffer-matches-beginning-pos (buffer regexp &optional beginning end)
  (m-buffer-markers-to-pos
   (m-buffer-matches-beginning buffer regexp beginning end)))

(defun m-buffer-matches-end-n (matches n)
  "Given match-data returns end of nth group.
Use `m-buffer-matches' to generate matches."
  (-map
   (lambda (match)
     (nth
      (+ 1 (* 2 n))
      match))
   matches))

(defun m-buffer-matches-end (buffer regexp &optional beginning end)
  "Returns a list of markers to the end of matches to regexp in buffer.
Use `m-buffer-nil-markers' after the markers have been used or they will slow
future changes to the buffer."
  (-map
   (lambda (match-data)
     (nth 1 match-data))
   (m-buffer-matches-data buffer regexp beginning end)))

(defun m-buffer-matches-end-pos (buffer regexp &optional beginning end)
  "Returns a list of positions of the end of matches in BUFFER to
REGEXP."
  (m-buffers-markers-to-pos
   (m-buffer-matches-end buffer regexp beginning end)))

;; marker/position utility functions
(defun m-buffer-nil-markers (markers)
  "Takes a (nested) list of markers and nils them all."
  (-map
   (lambda (marker)
     (set-marker marker nil))
   (-flatten markers)))

(defun m-buffer-markers-to-pos (markers &optional postnil)
  "Transforms a list of markers to a list of positions.
If the markers are no longer needed, set postnil to true, or call
`m-buffer-nil-markers' manually after use to speed future buffer
movement."
  (-map
   (lambda (marker)
     (prog1
         (marker-position marker)
       (when postnil
         (set-marker marker nil))))
   markers))

(defun m-buffer-pos-to-markers (buffer positions)
  "Translates a set of positions to markers."
  (-map
   (lambda (pos)
     (set-marker
      (make-marker) pos buffer))
   positions))

(defun m-buffer-replace-matches (matches replacement &optional subexp)
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
