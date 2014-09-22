---
layout: post
title:  Extending Jumpotron
date:   2014-09-22
---

[Jumpotron][] is slowly turning into something useful, so I'd like to talk about
how to extend it.

Jumpotron, by default, contains functionality to redirect to somewhere else if a
trigger word occurs in the query. This is what it was made to do, but the API is
a little more flexible in that it allows you to implement your own actions for
when trigger words are found in the query.

I've used this to add (very) basic bookmark functionality. The implementation
takes up a total of 30 lines in a source file, the first 10 of which define the
package it's in. The actual implementation looks like this:

```commonlisp
(in-package #:jumpotron.bookmarks)

(defvar *bookmarks* (make-hash-table :test #'equal))

(defclass bookmarking-jump (jump) ())
(defclass bookmark-jump (jump) ())

(defmethod jump ((jump bookmarking-jump) query-parts)
  (if (= 2 (length query-parts))
      (setf (gethash (first query-parts) *bookmarks*)
            (second query-parts))
      "Can't add bookmark, wrong number of arguments"))

(defmethod jump ((jump bookmark-jump) query-parts)
  (if (gethash (first query-parts) *bookmarks*)
      (redirect *response* (gethash (first query-parts) *bookmarks*))
      "Bookmark doesn't exist."))

(defmethod suggest ((jump bookmark-jump) query-parts)
  (hash-table-keys *bookmarks*))
```

`BOOKMARKING-JUMP` adds a bookmark to the global bookmark table `*BOOKMARKS*`
and `BOOKMARK-JUMP` redirects to it. `BOOKMARK-JUMP` also implements suggestions
for bookmark names.

You'd use it like this:

```commonlisp
;; !bma <name> <url> will add a bookmark under <name>
(jumpotron:add-jump "!bma" (make-instance 'jumpotron.bookmarks:bookmarking-jump))
;; !bm <name> will redirect to the bookmark under <name>
(jumpotron:add-jump "!bm" (make-instance 'jumpotron.bookmarks:bookmark-jump))
```

This is all very barebones (just like the rest of Jumpotron), but it illustrates
how to extend Jumpotron to add functionality.

Granted, this bookmark jump isn't really all that useful. It allows anyone with
access to the URL to add bookmarks and bookmarks are stored in a global table, I
made the basics of it to test the suggestion functionality and decided to extend
it a bit and keep it. Maybe it'll come in useful later.

Documentation about [Jumpotron][] can be found on its Github page. There's a lot
that can (and should) be made better, but it's already pretty usable. I've been
using it as my browser's default search engine for a while now.

[Jumpotron]: https://github.com/jorams/jumpotron
