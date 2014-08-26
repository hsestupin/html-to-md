# html-to-md

A Clojure library for converting html to md. Tiny and simple. Many thanks to https://github.com/domchristie/to-markdown - lots of regular expressions and test cases were grabbed from there.

## Usage

How to convert html string:

```clojure
    (use 'html-to-md.core)

    (html-to-md-string "<img src='http://example.com/logo.png' alt='Example logo' />")
    => "![Example logo](http://example.com/logo.png)"
```

For more complex usecases there is function `html-to-md [in out]`:

* **in** could be everything that can be eaten by [net.cgrand.enlive-html/html-resource](https://github.com/cgrand/enlive)
* **out** is used as an argument to [clojure.java.io/writer](http://clojuredocs.org/clojure_core/clojure.java.io/writer)
    
    (Default implementations are provided for Writer, BufferedWriter, OutputStream, File, URI, URL, Socket, and String)

## Leiningen plugin

Leiningen plugin for converting html files to md is also available https://github.com/hsestupin/lein-html2md

## Dependency

```clojure
[org.clojars.hsestupin/html-to-md "0.1.1"]
```

```xml
<dependency>
  <groupId>org.clojars.hsestupin</groupId>
  <artifactId>html-to-md</artifactId>
  <version>0.1.1</version>
</dependency>
```

## License

Copyright (C) 2014 Sergey Stupin

Distributed under the Eclipse Public License, the same as Clojure.
