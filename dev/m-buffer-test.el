;;; m-buffer-test.el --- Tests for m-buffer

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


(require 'm-buffer)

;;; Code:
(defmacro with-temp-buffer-of-file (file &rest body)
  "Run BODY in a temp buffer with the contents of FILE inserted."
  `(with-temp-buffer
     (insert-file-contents ,file)
     ,@body))

(ert-deftest m-with-temp-buffer-of-file ()
  (should
   (equal
    "one\ntwo\nthree\n"
    (with-temp-buffer-of-file
     "with-temp-buffer.txt"
     (buffer-string)))))

(ert-deftest m-buffer-matches ()
  (should
   (= 3
      (length
       (with-temp-buffer-of-file
        "match-data.txt"
        (m-buffer-matches-data
         (current-buffer)
         "^one$")))))
  (should
   (-every?
    'markerp
    (-flatten
     (with-temp-buffer-of-file
      "match-data.txt"
      (m-buffer-matches-data
       (current-buffer)
       "^one$"))))))

(ert-deftest m-buffer-matches-beginning ()
  (should
   (-every?
    'markerp
    (with-temp-buffer-of-file
     "match-data.txt"
     (m-buffer-matches-beginning
      (current-buffer)
      "^one$")))))

(ert-deftest m-buffer-matches-beginning-pos ()
  (should
   (equal
    '(1 9 17)
    (with-temp-buffer-of-file
     "match-data.txt"
     (m-buffer-matches-beginning-pos
      (current-buffer)
      "^one$")))))

(ert-deftest m-buffer-nil-markers ()
  (should
   (with-temp-buffer-of-file
    "match-data.txt"
    (-all?
     (lambda (marker)
       (and
        (marker-position marker)
        (marker-buffer marker)))
     (m-buffer-matches-beginning (current-buffer) "^one$"))))
  (should
   (with-temp-buffer-of-file
    "match-data.txt"
    (-all?
     (lambda (marker)
       (and
        (not (marker-position marker))
        (not (marker-buffer marker))))
     (m-buffer-nil-markers
      (m-buffer-matches-beginning (current-buffer) "^one$"))))))


(ert-deftest replace-matches ()
  (should
   (equal
    "three\ntwo\nthree\ntwo\nthree\ntwo\n"
    (with-temp-buffer-of-file
     "match-data.txt"
     (m-buffer-replace-matches
      (m-buffer-matches-data
       (current-buffer) "^one$") "three")
     (buffer-string)))))

(provide 'm-buffer-test)
;;; m-buffer-test.el ends here
