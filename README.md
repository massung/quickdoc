# QuickDoc

The `quickdoc` package is a text markup implementation that is designed for simplicity, unambiguity, and to be aesthetically pleasing when rendered. It steals mostly from [Creole](http://www.wikicreole.org/) and [MakeDoc](http://www.rebol.net/docs/makedoc.html), but also adds features that I've wanted in various markup implementations and removes things as well.

The initial implementation is written in Common Lisp using [LispWorks](http://www.lispworks.com), but there's nothing about the QuickDoc markup that is specific to Lisp.

To get a sense of what QuickDoc can do, check out the [example document](http://massung.github.io/quickdoc/example.html) created with it. And the source for the document is [here](https://raw.githubusercontent.com/massung/quickdoc/master/example.txt).

## Quickstart

The `quickdoc` package has three main functions:

	(parse-quickdoc string)                       ;=> quickdoc
	(render-quickdoc quickdoc &optional stream)   ;=> nil
	(compile-quickdoc pathname &optional target)  ;=> quickdoc

Simply pass a string to `parse-quickdoc`, which will create a quickdoc object, which can then be rendered to HTML with `render-quickdoc`.

The `compile-quickdoc` function is merely a helpful wrapper that will read the contents of a source file, parse it, and render it back out to a (new) target HTML file. If no target is provided, a file with the same name as the source, but with an HTML extension is written out. When done, the quickdoc object that was parsed is returned.
