# QuickDoc

The `quickdoc` package is a text markup implementation that is designed for simplicity, unambiguity, and to be aesthetically pleasing when rendered. It steals mostly from [Creole](http://www.wikicreole.org/) and [MakeDoc](http://www.rebol.net/docs/makedoc.html), but also adds features that I've wanted (e.g. meta tags, CSV tables, aligned images, embedded videos) and removes things as well.

The initial implementation is written in Common Lisp using [LispWorks](http://www.lispworks.com), but there's nothing about the QuickDoc markup that is specific to Lisp.

To get a sense of what QuickDoc can do, check out the [example document](http://massung.github.io/quickdoc/example.html) created with it. And the source for the document is [here](https://raw.githubusercontent.com/massung/quickdoc/master/example.qd).

## Quickstart

The `quickdoc` package has a few functions:

```
(parse-quickdoc pathname)                                   ;=> quickdoc
(compile-quickdoc doc pathname &optional stylesheet embed)  ;=> nil
(render-quickdoc doc &optional stream)                      ;=> nil
```

Pass a pathname to `parse-quickdoc`, which will read the file and parse it into a `quickdoc` object and return it.

The `compile-quickdoc` method will compile a `quickdoc` into HTML and write it the the pathname file on disk (overwriting if it already exists). If `stylesheet` is provided it should be a pathname and `embed` is a flag indicating whether or not the stylesheet provided is embedded with a `<style>` tag or linked to with a `<link>` tag. If a stylesheet is not provided, then the default stylesheet is used and it is embedded into the document.

If you just want to get the final HTML of a `quickdoc` body, simply call `render-quickdoc`. This will not render the `<HEAD>` or `<BODY>` tags, but will just output all the final HTML within the `<BODY>` tag.

## fin.
