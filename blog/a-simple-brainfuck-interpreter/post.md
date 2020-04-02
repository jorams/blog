---
title: A Simple Brainfuck Interpreter
published: 2014-09-10
---

I made a simple interpreter for [Brainfuck][] today. It was an interesting
little project. There are only 8 different commands, 6 of which are trivial to
implement. The problems only start when implementing `[` and `]`.

`[` And `]` come in pairs, and those can be nested. When walking through the
code you have to somehow know where the matching character is. There's a variety
of ways in which you can do that, but I decided to just walk through the
characters before interpreting and storing the position of the matching matching
bracket in a hash table.

That was it, all problems solved. Kind of. I threw a bunch of Brainfuck examples
at it. [This][numwarp] pretty printer for numbers worked great, even when
passing it through [this][bfint] Brainfuck interpreter written in Brainfuck.

I decided to keep it all in a single function. This is what it looks like:

```commonlisp
(defun brainfuck (input)
  (let (([] (loop for char across input
                  for i from 0
                  with result = (make-hash-table)
                  with stack = ()
                  do (case char
                       (#\[ (push i stack))
                       (#\]
                        (setf (gethash i result) (first stack))
                        (setf (gethash (pop stack) result) i)))
                  finally (return result))))

    (loop with ip = 0                   ; the instruction pointer
          with dp = 0                   ; the data pointer
          with data = (make-array 30000 :element-type 'integer
                                        :initial-element 0)
          for char = (if (< ip (length input))
                         (char input ip)
                         (return (values)))
          do (symbol-macrolet ((value (elt data dp)))
               (case char
                 (#\> (incf dp))
                 (#\< (decf dp))

                 (#\+ (incf value))
                 (#\- (decf value))

                 (#\. (princ (code-char value)))
                 (#\, (setf value (char-code (read-char))))

                 (#\[ (when (zerop value)
                        (setf ip (gethash ip []))))
                 (#\] (unless (zerop value)
                        (setf ip (gethash ip []))))
                 (t (values))))
             (incf ip))))
```

(Calling the hashmap with `[]` positions "[]" gave me some problems, with
[Paredit][] jumping to after the next `)` when pressing `]`.)

[Brainfuck]: https://en.wikipedia.org/wiki/Brainfuck
[numwarp]: http://www.hevanet.com/cristofd/brainfuck/numwarp.b
[bfint]: http://www.hevanet.com/cristofd/brainfuck/dbfi.b
[Paredit]: http://www.emacswiki.org/emacs/ParEdit
