---
title: What if... Programming Language Ideas
published: 2015-01-12
---

## Background

I'm a bit of a programming language freak. About 1.5 years ago I "landed" on
[Common Lisp][], which has since been my default choice for a lot of things.
It's not the only language I use, but it's my favorite primarily because of the
simplicity underlying it all and the incredible flexibility it allows. It
basically allows every programmer that uses it to be a language designer.

But it's not perfect. Common Lisp is a language that has basically stopped
evolving in 1994. The ecosystem around it has changed/improved a lot, but the
language has stayed the same. In some cases this is awesome. It means code
written many years ago looks mostly the same as code written recently, and it
still works (as long as it doesn't rely on implementation-specific behavior that
has changed).

It also has downsides. Common Lisp has a couple of flaws. It has operators that
don't follow the naming conventions set forth by others. It has features that,
while flexible, sometimes complicate code that shouldn't be complicated.

Several times in the past I've tried to "fix" things, to clean things up a bit.
I would create a new [package][] designed to be imported instead of the default
`COMMON-LISP` package, exporting (or not exporting) some things that followed
more consistent naming conventions, changed some parameter orders, things like
that. It worked reasonably well, even though renaming functions and macros isn't
as simple as it should, in my opinion, be.

Then, after a while, I'd run into a problem. The symbol `LAMBDA` is special. You
can't really give it a different name that works everywhere, because the special
operator `FUNCTION` special-cases the symbol itself.

I've decided that, while I really like Common Lisp, I simply can't morph it into
the "perfect" programming language for me. Instead I've started the thought
process of designing a "perfect" programming language that doesn't have to live
within the constraints set forth by Common Lisp. A few ideas I've been playing
with follow.

## What if

1. What if the lowest-level function type only takes a single argument?

    In Common Lisp, argument lists for a function are a magical thing. The list
    of arguments is destructured before being passed to the function. Wouldn't
    it be interesting to have that destructuring happen in user code? An
    operator for defining a function that does it automatically using some
    destructuring operator could be defined, but I really like the idea of
    keeping the low-level construct as simple as possible.

2. What if every function was a generic function?

    Common Lisp provides, through the [Common Lisp Object System][CLOS], the
    incredibly powerful concept of a generic function, a function whose
    definition basically depends on the arguments passed to it. In Common Lisp
    generic function aren't used everywhere throughout the language. This
    probably has a lot to do with the fact that CLOS was added to the standard
    relatively late in the process.

    Using generic functions for everything would enable a lot of flexibility,
    but it could also cause problems. The code from one package could specialize
    `+` on string arguments to concatenate strings, while code from another
    package could want to specialize it on strings arguments to try to convert
    them to numbers, effectively making it work like in a weakly typed language.

    To solve this I've been thinking about making packages not only work on the
    symbol level, but also on the level of methods. This could solve the
    problem, but it could also open a whole can of worms in terms of complexity.
    Another way could be to organize packages into a tree and only enable
    methods on generic functions if the package they're being used in is
    somewhere downstream from the package they were defined in. This also
    doesn't seem ideal, because what if you want to use only a few methods
    defined in a package?

    To be continued.

3. What if the types a generic function could specialize on could be any sort of
   type?

    In Common Lisp, there are basically two different type systems. On one hand
    there is the system of, well, types, that allows incredibly flexible and
    expressive type definitions. A type can even be defined as a predicate (a
    function that returns `t` or `nil` (`true` or `false`) depending on its
    argument(s)), meaning all objects that satisfy that predicate have that
    type.

    On the other hand there's the system of "classes". A class is, much like in
    many other programming languages, basically inherent to an object. They work
    pretty much like in languages like [Java][], except that they also allow
    [multiple inheritance][] and don't normally "contain" functionality, just
    data.

    Generic functions can only specialize their arguments on classes. This
    means, for example, that you can only specialize an argument on the class
    `integer`, not on the type `(integer 12 24)`, which would limit the set of
    objects to integers between 12 and 24. This is a simple example, but it
    illustrates the difference in expressiveness quite well.

    The problem with allowing arbitrary types as specializers is that it would
    come down to [predicate dispatch][]. The problem is that predicates can
    overlap and don't have an inherent relationship, making it impossible to
    determine which method is more specific and thus should be called first.

    This problem can only be resolved by either introducing a relationship
    between predicates or by limiting the conditions that can be used, which is
    what "normal" type (class) dispatch does.

4. What if there were no variables, only functions?

    This is a tough one. A variable is, basically, a function that doesn't take
    any arguments and whose result has already been determined. If the use of a
    variable would be an ordinary function call, the distinction between
    variables and functions would be gone, thereby also mostly getting rid of
    the debate about [Lisp-1 vs Lisp-2][l1l2].

    There are, unfortunately, some problems. There's a good reason for the
    distinction into computations to run (functions) and the stored results of
    such computations (variables). If every variable reference would really be a
    function call, you wouldn't be able to store the results of computations. A
    purely functional programming language without side-effects can get away
    with it most of the time because the result of a function call wouldn't
    change, but otherwise it'd be a problem.

    One possible solution could be to introduce an operator of some sort that
    would only evaluate its arguments once and store the results for future
    references. Some sort of local, explicit variable mechanism.

    To be continued.

I don't know if I will ever turn my ideas into a real programming language, but
the field is filled with interesting questions. Every once in a while I come
across some language I find incredibly interesting (such as [Magpie][]) and some
of them have very good ideas. Let's see where the future takes us.

[Common Lisp]: https://en.wikipedia.org/wiki/Common_Lisp
[package]: http://www.gigamonkeys.com/book/programming-in-the-large-packages-and-symbols.html
[CLOS]: http://www.dreamsongs.com/CLOS.html
[Java]: https://en.wikipedia.org/wiki/Java_%28programming_language%29
[multiple inheritance]: https://en.wikipedia.org/wiki/Multiple_inheritance
[predicate dispatch]: http://homes.cs.washington.edu/~mernst/pubs/dispatching-ecoop98-abstract.html
[l1l2]: http://www.nhplace.com/kent/Papers/Technical-Issues.html
[Magpie]: http://magpie.stuffwithstuff.com/index.html
