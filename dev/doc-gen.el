;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Free Software Foundation, Inc.

(require 'm-buffer-macro)
(require 'm-buffer)
(require 'm-buffer-at)

(require 'lentic-doc nil t)

(defun doc-gen ()
  (lentic-doc-htmlify-package 'm-buffer))
