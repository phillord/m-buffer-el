;; Return information at location.
;; Stateless version of emacs code
(require 'm-buffer-macro)

(defun m-buffer-at-eolp (&rest location)
  "Return t if LOCATION is at the end of a line.
See also `eolp'."
  (m-buffer-with-current-location
      location
    (eolp)))
  

(provide 'm-buffer-at)
