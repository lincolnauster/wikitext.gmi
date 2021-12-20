# wikitext.gmi
an unstable state machine to convert WikiText to Gemtext

## What the hell is this?
Wikitext is the markup language used by MediaWiki sites like Wikipedia. Gemtext
is the primary markup language for content syndicated along the Gemini protocol.
Wikipedia is fantastic --- rustdoc for the real world --- and Gemini is, too: it
provides a simple and quiet alternative (though not replacement) for the Web.
This allows transforming a stream of Wikitext, say, a Wikipedia article, into a
stream of Gemtext, allowing for convenient viewing over Gemini.

Note that this is *not* a complete proxy for Wikipedia, though one utilizing
this is in the works!

## How broken is it?
There are two challenges here:

1. Wikitext is awful. Seriously, it's bad. I would source a spec to justify
   these claims, but there is no spec.
2. A conversion from Wikitext to Gemtext is necessarily lossy, which is what the
   rest of this section addresses.

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
  out on colors/inline formatting/images/etc. I don't see a good way to change
  that.
* Inline links: [unimplemented] we just transform them to reference-style
  out-of-line links at the end of the paragraph. This might be ugly, and is
  subject to change.
* References: [unimplemented] we transform these to out-of-line links at the
  end of the document.
* Images: [unimplemented] we transform these into links after paragraphs.
* Horizontal Rules: These are just displayed verbatim as `---`.
* Tables of Contents: [unimplemented] we just ignore these. In Geminispace, this
  is proudly the problem of clients.
* Nested Lists: [unimplemented] these are just flattened. I don't know how I
  feel about this, alternate strategies are welcome.
* Ordered lists [unimplemented]: The number is prepended to the list text in
  brackets.
* Bold and italic text: this is transformed to markdown-esque formatting. That
  seems to be the convention in Gemini-space (though it's of course not coded
  into the Gemtext spec).
* [bunch more stuff that I'll get to later!]

## How fast is it?
Short answer: O(n) \
Long answer: it's a work in progress. Algorithmically speaking, the core FSM is
*fine* (basically O(n) except for some reconstructions; you could probably coax
O(n^2) out of it in degenerate cases), but it needs some real-world-perf work in
IO and strings. Did you know that the default Haskell string type is actually a
linked list? Neither did I! It's exactly as bad as it sounds! If you're seeing
this, that means I've un-privated the repository and we're not actually using
Strings any more (it's probably Texts instead), but IO in Haskell (especially
lazy IO, like the type we *definitely need* here) is a bit of a mess. Feel
free to help out! If you want to take it upon yourself to optimize in the case
that I don't get around to it first, here's what I'm looking for:

+ Keep memory usage low & lazy.
+ Try for contiguous memory (linked lists are probably a mistake!).
+ Assume that hundreds of instances could be running at once.
+ Encapsulate it.
+ Keep it idiomatic.

The [Pipes](https://hackage.haskell.org/package/pipes) package looks like it
could work quite well.

## Usage
wikitext.gmi compiles to a binary (`wikitext-gmi`) which reads WikiText from
stdin and writes Gemtext to stdout. Some C bindings for FFI might exist at some
point, but do note the GPL license.

To build it from source, build it as a Flake (`nix build`) or with cabal (`cabal
build`).
