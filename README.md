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
* [bunch more stuff that I'll get to later!]

## Usage
wikitext.gmi compiles to a binary which reads WikiText from stdin and writes
Gemtext to stdout. Some C bindings for FFI might exist at some point, but do
note the GPL license.

## Building & Contributing
Cabal and Nix Flakes are used to manage dependencies. Run `nix build` to just
get the damn binary already, or run `nix develop` to get a shell where you can
`cabal build` yourself a binary.
