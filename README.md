# QuickDoc

The `quickdoc` package is a text markup implementation that is designed for simplicity, unambiguity, and to be aesthetically pleasing when rendered. It steals mostly from [Creole](http://www.wikicreole.org/) and [MakeDoc](http://www.rebol.net/docs/makedoc.html), but also adds features that I've wanted (e.g. meta tags, CSV tables, aligned images) and removes things as well.

The initial implementation is written in Common Lisp using [LispWorks](http://www.lispworks.com), but there's nothing about the QuickDoc markup that is specific to Lisp.

To get a sense of what QuickDoc can do, check out the [example document](http://massung.github.io/quickdoc/example.html) created with it. And the source for the document is [here](https://raw.githubusercontent.com/massung/quickdoc/master/example.qd).

## Quickstart

The `quickdoc` package has two main functions:

	(parse-quickdoc string)                                 ;=> doc
	(render-quickdoc quickdoc &key stream title stylesheet) ;=> nil

Simply pass a string to `parse-quickdoc`, which will create a quickdoc object, which can then be rendered to HTML with `render-quickdoc`.

The optional arguments to `render-quickdoc` allow you to output to a stream, override the document title, and embed a stylesheet `<LINK>` into the document.

A helper function `compile-quickdoc` is used to parse the contents of a QuickDoc file, and render to an output file in one call.

	(compile-quickdoc file &rest render-args &key output-file) ;=> doc

Any keyword arguments that `render-quickdoc` accepts may also be sent to `compile-quickdoc` and are forwarded.
