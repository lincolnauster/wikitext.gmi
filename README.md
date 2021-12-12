# wikitext.gmi
an unstable state machine to convert WikiText to Gemtext

## Why?
Why not! I need *something* to learn Haskell for, and I also *really* want a
Wikipedia-over-Gemini proxy, so I'm killing two birds with one stone! For an
integration into a full proxy, see Instrumental.

## How broken is it?
It's actually not that bad. WikiText is an *awful* markup language, but it's
workable in production, as obviously evidenced by MediaWiki. A lot of issues
will arise if you want to create (and then, gasp, manipulate!) an AST, but we
dodge those here by just not bothering and operating over the raw file stream,
with only one output format. Also note that there's no formal spec for wikitext,
so nothing's keeping the parser honest outside of its own tests.

There are a very large number of 'features' that WikiText supports that gemtext
doesn't. Here's how we handle them:

* Headers more than 3 levels of importance down (i.e., markdown's `####` and
  onward): for headers of lesser importance, we write out the header text
  surrounded by brackets, i.e., `[A Very Unimportant Section]`. This loses
  information, but, let's be fair, you're asking for this. If anyone has any
  elegant ideas here for a lossless conversion, go ahead and suggest/implement
  them, but I'm more than willing to stick to Gemtext's implicit style guide.
* Tables: [unimplemented] we write out a plain text version of the table encased
  in triple-backtics. This is lossless the majority of the time, but will lose
  out on colors/etc. That's not realistically changing.
* Inline links: [unimplemented] we just transform them to out-of-line links at
  the end of the paragraph. This might be ugly, and is subject to change.
* References: [unimplemneted] we transform these to out-of-line links at the
  end of the document.
* Images: [unimplemented] we transform these into links between paragraphs.
* Horizontal Rules: These are just displayed verbatim as `---`.
* Tables of Contents: [unimplemented] we just ignore these. Wikipedia has a
  bunch of directives to work with them, and none of them map well to
  Geminispace: for one, there are no nested links, and, for another, if a user
  wants a table of contents, they can just use a client that provides it.
* Nested Lists: [unimplemented] these are just flattened. I don't know how I
  feel about this, alternate strategies are welcome.
* Ordered Lists: The number is prepended to the list text in brackets.
* [bunch more stuff that I'll get to later!]

## How fast is it?
It's a work in progress. The core algorithms (mostly just a DFA) look sound
enough to me, and Haskell seems to be a good language to play with different IO
techniques due to its handling of polymorphism and its equational reasoning
capabilities, but my own theoretical knowledge of 'fast Haskell' is not much.
This is written first for idiomaticity and *then for speed*; all to be used in
Instrumental, my Wikipedia-over-Gemini proxy. If you look at that README, you'll
see that speed is not *it's* primary goal either, though 'fast-enough' is a
concern.

If you want to take it upon yourself to optimize (in case I don't get around to
it first!), here's what I'm looking for:
+ Keep memory usage low & lazy.
+ Keep contiguous memory.
+ Assume that hundreds of instances could be running at once.
+ Keep it all encapsulated.
+ Keep it all idiomatic.

The [Pipes](https://hackage.haskell.org/package/pipes) package looks like it
could work quite well.

## Usage
wikitext.gmi compiles to a binary which reads WikiText from stdin and writes
Gemtext to stdout. Some C bindings for FFI might exist at some point, but do
note the GPL license.

## Building & Contributing
Cabal and Nix Flakes are used to manage dependencies. Run `nix build` to just
get the damn binary already, or run `nix develop` to get a shell where you can
`cabal build` yourself a binary.
