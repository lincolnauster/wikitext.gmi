# wikitext.gmi
an unstable state machine to convert WikiText to Gemtext

## What on earth is this?
Wikitext is the markup language used by MediaWiki websites like Wikipedia.
Gemtext is the primary markup language for content syndicated over the Gemini
protocol. Wikipedia is is my only braincell, and Gemini is pretty great, too: it
provides a simple, fast, and quiet alternative to (though not replacement for)
the Web. This tool allows transforming a stream of Wikitext, say, a Wikipedia
article, into a stream of Gemtext, allowing for convenient viewing over Gemini.

Note that this is *not* a complete proxy for Wikipedia, though one utilizing
this will pop up soon!

## How broken is it?
There are two challenges here:

1. Wikitext is an awful markup language. Seriously, it's bad. I would source a
   spec or a grammar document to justify these claims, but there is no spec.
   Wikitext appears to be a bunch of ad-hoc rules piled onto a massive PHP
   script. The fact that it works at all is really quite something.

2. A conversion from Wikitext to Gemtext is necessarily lossy. Wikitext just has
   more features, and that's what the rest of this section addresses.

There are a very large number of 'features' that WikiText supports that gemtext
doesn't. Here's how we handle them:

- [x] Headers more than 3 levels of importance down (i.e., markdown's `####`
  and onward): for headers of lesser importance, we write out the header text
  surrounded by brackets, i.e., `[A Very Unimportant Section]`.

- [ ] Tables:  we write out a plain text version of the table encased in
  triple-backtics, probably with no caption. This is lossless the majority of
  the time, but will lose out on colors and inline formatting and images/etc.
  That's not changing within the bounds of gemtext.

- [ ] Inline links: [unimplemented] we just transform them to reference-style
  out-of-line links at the end of the paragraph. This might be ugly, and is
  subject to change.

- [ ] References: we transform these to out-of-line links at the end of the
  document.

- [ ] Images: [unimplemented] we transform these into WWW links after
  paragraphs.

- [x] Horizontal Rules: These are just displayed verbatim as `---`.

- [x] Tables of Contents: If wikitext requests a table of contents, it's
  ignored. ToC's are clients' problems in Geminispace.

- [ ] Nested Lists: these are just flattened.

- [ ] Ordered lists: The number is prepended to the list text in
  brackets.

- [ ] Bold and italic text: this is transformed to markdown-esque formatting.
  That seems to be the convention in Gemini-space (though it's of course not
  coded into the Gemtext spec).

- [ ] bunch more stuff that I'll get to later!

## How fast is it?
Short answer: O(n) \
Long answer: it's a work in progress. Algorithmically speaking, the core FSM is
*fine* (basically O(n) except for some reconstructions; you could probably coax
*O(n^2)* out of it in very degenerate unrealistic cases), but it needs some
real-world-perf work in IO and strings. Did you know that the default Haskell
string type is actually a linked list? Neither did I! It's exactly as bad as it
sounds! Even once those are replaced with Texts or something similar, IO in
Haskell (especially lazy IO, like the type we *definitely need* here) is a bit
of a mess. Feel free to help out! If you want to take it upon yourself to
optimize in the case that I don't get around to it first, here's what I'm
looking for:

+ Keep memory usage low & lazy.
+ Try for contiguous memory (linked lists are definitely a mistake!).
+ Assume that hundreds of instances could be running at once (as it would be in
  the proxy-server environment this targets)
+ Encapsulate it.
+ Keep it idiomatic.

The [Pipes](https://hackage.haskell.org/package/pipes) package looks like it
could work quite well.

## Usage
wikitext.gmi compiles to a binary (`wikitext-gmi`) which reads WikiText from
stdin and writes Gemtext to stdout. Some C bindings for FFI might exist at some
point, but do note the GPL license; don't redistribute with anything
closed-source.

To build from source, build it as a Flake (`nix build`) or with cabal (`cabal
build`). There are no external dependencies.
