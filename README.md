m-buffer.el
===========
[![Build Status](https://travis-ci.org/phillord/m-buffer-el.png?branch=master)](https://travis-ci.org/[![Build Status](https://travis-ci.org/phillord/m-buffer-el.png?branch=master)](https://travis-ci.org/phillord/m-buffer-el)phillord/m-buffer-el)

## Introduction

This package provides a set of list-orientated functions for operating over
the contents of Emacs buffers. Functions are generally purish: i.e. they may
change the state of one buffer by side-effect, but should not affect point,
current buffer, match data or so forth. Generally, markers are preferred over
point locations so that it is possible, for example, to search for regexp
matches and then replace them all without the early replacement invalidating
the location of the later ones.

For full commentary, please see [m-buffer.el](m-buffer.el).

## Status

Early release, all APIs subject to change.

## Contributions

Contributions are welcome. However, I would like to keep the option of hosting
m-buffer.el on ELPA, therefore, contributors should have
[Copyright Assignment papers](https://www.gnu.org/prep/maintain/html_node/Copyright-Papers.html)
with the FSF.


## Change Log

### 0.2

#### New Functions
 - Functions for matching block things -- line start and end, sentence end,
   paragraph separators, words.
 - `m-buffer-match-string` and `m-buffer-match-substring` for extracting
   match-strings. 
 

#### Name changes
 - Functions now use singular rather than plural -- so `m-buffer-matches-data`
   has become `m-buffer-match-data`.
 - All uses of `beginning` have been changed to `begin` -- it is shorter and
   matches `end`

#### Matchers
 - Regexp functions are now overloaded and take either a buffer and regexp or
   match-data (except for `m-buffer-match-data` for which it makes no sense to
   pass in match-data). This allows easy chaining of methods.
 - Matchers now also overloaded for windows -- searching in the visible
   portion of window. `m-buffer-match-data-visible-window` access this feature
   directly.
 - Match Options are now keyword rather than positional which considerably
   simplifies the implementation, especially with an eye to future expansion.

#### Build and Test
 - Reworked tests and build scripts.
