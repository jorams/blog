---
title: Introducing Jumpotron
published: 2014-09-03
---

I have been using [DuckDuckGo][] as my default search engine for a while now.
Its search isn't as good as Google's, but after coming for the privacy, I stayed
for the goodies.

The feature that is most relevant for this post comes in the form of [!bangs][]. A
!bang is a word somewhere in your query that makes the query redirect to
somewhere else. "Somewhere else" can be many different things: !g redirects to
Google, !w redirects to Wikipedia, !cliki redirects to [Cliki][], etc.

If, for example, you're looking for "Common Lisp", and you want a Wikipedia
page, you search for "Common Lisp !w". The query will be redirected to
Wikipedia, and you'll immediately be on the [page][cl-wiki] you're looking for.

There are thousands of !bangs built-in, and you'll probably find most of them to
be very intuitive. But there's a problem: You can only use !bangs that
DuckDuckGo has added. You can [suggest][newbang] new !bangs, but you could get
much more out of this if you had more control. Jumpotron is designed to give you
that control.

[Jumpotron][jumpotron] is a self-hosted web service written in Common Lisp that
is designed to be used as your primary search engine. You can define your own
trigger words to do different things, and if no trigger word is found you can
make the query fall through to another search engine. You can use it as a
replacement for bookmarks, or to add your own !bang-like words. If that's not
enough you can extend Jumpotron to do other things when a query with a certain
trigger word comes in.

Configuration of "jumps", as the actions Jumpotron takes are called, are defined
at the [REPL][] of your favourite Common Lisp implementation. Jumpotron is
developed on [SBCL][], but it should work on other implementations too.

Basic jumps can be defined like this:

```commonlisp
;; Make any query containing "@jumpotron" redirect to Jumpotron's GitHub page
(jumpotron:define-redirect "@jumpotron" "https://github.com/jorams/jumpotron")

;; Redirect all queries that don't match anything to DuckDuckGo
(jumpotron:define-redirect nil "https://duckduckgo.com/?q=~@{~A~^+~}")
```

Then you start Jumpotron:

```commonlisp
;; Starts Jumpotron on port 5000
(jumpotron:start)

;; Starts Jumpotron on port 8765
(jumpotron:start 8765)
```

After that, when you go to `http://localhost:5000/jump?q=@jumpotron`, you'll be
redirected to [Jumpotron's GitHub page][jumpotron], and going to
`http://localhost:5000/jump?q=anything+else` will redirect to
[DuckDuckGo][ddg-anything].

It's also possible to place the query directly after the `/`, but the
`/jump?q=<query>` format should be preferred.

The only type of jump currently implemented is a jump that redirects you to
somewhere else based on a [FORMAT][] control string. The words in the query will
be passed as arguments to `FORMAT`.

Documentation about extending Jumpotron can be found on its
[GitHub page][jumpotron].

```commonlisp
(jumpotron:stop)
```


[jumpotron]: https://github.com/jorams/jumpotron
[DuckDuckGo]: https://duckduckgo.com/
[ddg-anything]: https://duckduckgo.com/?q=anything+else
[!bangs]: https://duckduckgo.com/bang.html
[Cliki]: http://cliki.net/
[cl-wiki]: https://en.wikipedia.org/wiki/Common_Lisp
[newbang]: https://duckduckgo.com/newbang
[REPL]: https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop
[SBCL]: http://sbcl.org/
[FORMAT]: http://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm
