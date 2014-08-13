# html-to-md

A Clojure library for converting html to md. Tiny and simple.

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

## Dependency

```clojure
[org.clojars.hsestupin/html-to-md "0.1.0"]
```

```xml
<dependency>
  <groupId>org.clojars.hsestupin</groupId>
  <artifactId>html-to-md</artifactId>
  <version>0.1.0</version>
</dependency>
```

## License

Copyright (C) 2014 Sergey Stupin

Distributed under the Eclipse Public License, the same as Clojure.
