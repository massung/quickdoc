# QuickDoc

The `quickdoc` package is a text markup implementation that is designed for simplicity, unambiguity, and to be aesthetically pleasing when rendered. It steals mostly from [Creole](http://www.wikicreole.org/) and [MakeDoc](http://www.rebol.net/docs/makedoc.html), but also adds features that I've wanted in various markup implementations and removes things as well.

The initial implementation is written in Common Lisp using [LispWorks](http://www.lispworks.com), but there's nothing about the QuickDoc markup that is specific to Lisp.

To get a sense of what QuickDoc can do, check out the [example document](https://massung.github.io/quickdoc/example.html) created with it. And the source for the document is [here](https://raw.githubusercontent.com/massung/quickdoc/master/example.txt).

## Quickstart

The `quickdoc` package has two main functions:

	(parse-quickdoc string)                     ;=> quickdoc
	(render-quickdoc quickdoc &optional stream) ;=> nil

Simply pass a string to `parse-quickdoc`, which will create a quickdoc object, which can then be rendered to HTML with `render-quickdoc`.
