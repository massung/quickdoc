# QuickDoc

The `quickdoc` package is a text markup implementation that is designed for simplicity, unambiguity, and to be aesthetically pleasing when rendered. It steals mostly from [Creole](http://www.wikicreole.org/) and [MakeDoc](http://www.rebol.net/docs/makedoc.html), but also adds features that I've wanted (e.g. meta tags, CSV tables, aligned images, embedded videos) and removes things as well.

The initial implementation is written in Common Lisp using [LispWorks](http://www.lispworks.com), but there's nothing about the QuickDoc markup that is specific to Lisp.

To get a sense of what QuickDoc can do, check out the [example document](http://massung.github.io/quickdoc/example.html) created with it. And the source for the document is [here](https://raw.githubusercontent.com/massung/quickdoc/master/example.qd).

## Quickstart

The `quickdoc` package has a few functions:

```
(quickdoc-parse pathname)                                   ;=> quickdoc
(quickdoc-render doc &optional stylesheet embed)            ;=> HTML
(quickdoc-compile doc pathname &optional stylesheet embed)  ;=> nil
```

Pass a pathname to `quickdoc-parse`, which will read the file and parse it into a `quickdoc` object and return it.

If you just want to get the final HTML of a `quickdoc` body, simply call `quickdoc-render`. This will return a complete `<html>` tag with a `<head>` and `<body>`. If `stylesheet` is provided it should be a pathname to a CSS file. The `embed` flag indicates whether or not the stylesheet provided is embedded with a `<style>` tag or linked to with a `<link>` tag. If a stylesheet is not provided, then the default stylesheet is used and it is embedded into the document.

The `quickdoc-compile` method will render the HTML produced by `quickdoc-render` to a file, overwriting if it already exists.

## fin.
