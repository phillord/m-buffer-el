(require 'load-relative)

(require-relative "../m-buffer-macro")
(require-relative "../m-buffer")
(require-relative "../m-buffer-at")

(require 'lentic-doc)

(defun doc-gen ()
  (lentic-doc-htmlify-package 'm-buffer))
