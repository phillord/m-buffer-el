;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

(require 'load-relative)

(require-relative "../m-buffer-macro")
(require-relative "../m-buffer")
(require-relative "../m-buffer-at")

(require 'lentic-doc nil t)

(defun doc-gen ()
  (lentic-doc-htmlify-package 'm-buffer))
