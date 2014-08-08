(ns html-to-md.core-test
  (:use [clojure.template])
  (:require [clojure.test :refer :all]
            [html-to-md.core :refer :all]
            [clojure.string :as str]))

(defmacro check [input expected msg]
  `(is (= (html-to-md-string ~input) ~expected) ~msg))

(deftest converts
  (testing "converting p elements"
    (check "<p>Lorem ipsum</p>" "Lorem ipsum" "We expect p tags to be wrapped with two line breaks")
    (check "<p class='intro'>Lorem ipsum</p>" "Lorem ipsum" "We expect p tags to be wrapped with two line breaks"))

  (testing "converting emphasis elements"
    (check "<b>Hello world</b>" "**Hello world**" "We expect <b>Hello world</b> to be converted to **Hello world**")
    (check "<strong>Hello world</strong>"
           "**Hello world**"
           "We expect <strong>Hello world</strong> to be converted to **Hello world**")
    (check "<b></b>" "" "We expect b tags to be removed")
    (check "<i>Hello world</i>" "_Hello world_" "We expect <i>Hello world</i> to be converted to _Hello world_")
    (check "<em>Hello world</em>" "_Hello world_" "We expect <em>Hello world</em> to be converted to _Hello world_")
    (check "<em id='one' class='cowabunga'>Hello world</em>"
           "_Hello world_"
           "We expect <em id='one' class='cowabunga'>Hello world</em> to be converted to _Hello world_")
    (check "<em id='one' class='cowabunga'></em>" "" "We expect empty em tags to be removed"))

  (testing "converting inline code elements"
    (check "<code>print()</code>" "`print()`" "We expect inline code tags to be converted to backticks")
    (check "<code></code>" "" "We expect empty code tags to be removed")
    (check "<code>&lt;video&gt;</code>" "`<video>`" "We expect HTML character references to be decoded")
    (check "<code>foo&#x1D306;bar</code>" "`foo\uD834\uDF06bar`" "We expect HTML character references to be decoded"))

  (testing "converting hr elements"
    (check "<hr />" "* * *" "We expect hr elements to be converted to * * *")
    (check "<hr/>" "* * *" "We expect hr elements to be converted to * * *")
    (check "<hr>" "* * *" "We expect hr elements to be converted to * * *")
    (check "<hr class='fancy' />" "* * *" "We expect hr elements to be converted to * * *"))

  (testing "converting br elements"
    (check "Hello<br />world" "Hello\nworld" "We expect br elements to be converted to valid line separator")
    (check "Hello<br/>world" "Hello\nworld" "We expect br elements to be converted to valid line separator")
    (check "Hello<br>world" "Hello\nworld" "We expect br elements to be converted to valid line separator"))

  (testing "converting img elements"
    (check "<img src='http://example.com/logo.png' />" "![](http://example.com/logo.png)" "We expect img elements to b e converted properly")
    (check "<img src= \"http://example.com/logo.png\" / >" "![](http://example.com/logo.png)" "We expect img elements to be converted properly")
    (check "<img src='http://example.com/logo.png'>" "![](http://example.com/logo.png)" "We expect img elements to be converted properly")
    (check "<img src=http://example.com/logo.png>" "![](http://example.com/logo.png)" "We expect img elements to be converted properly")
    (check "<img src='http://example.com/logo.png' alt='Example logo' />"
           "![Example logo](http://example.com/logo.png)"
           "We expect img elements to be converted properly with alt attrs")
    (check "<img src='http://example.com/logo.png' alt='Example logo' title='Example title' />"
           "![Example logo](http://example.com/logo.png \"Example title\")" "We expect img elements to be converted properly with alt and title attrs"))

  (testing "converting anchor elements"
    (check "<a href='http://example.com/about'>About us</a>"
           "[About us](http://example.com/about)"
           "We expect anchor elements to be converted properly")
    (check "<a href= \"http://www.example.com/about\" title= \"About this company\" >About us</a>"
           "[About us](http://www.example.com/about \"About this company\")"
           "We expect an anchor element with a title tag to have correct markdown")
    (check "<a class= \"some really messy stuff\" href= \"/about\" id= \"donuts3\" title= \"About this company\" >About us</a>"
           "[About us](/about \"About this company\")"
           "We expect an anchor element with a title tag to have correct markdown")
    (check "<a id=\"donuts3\">About us</a>"
           "<a id=\"donuts3\">About us</a>"
           "Anchor tags without an href should not be converted"))

  (testing "converting code blocks"
    (let [html-code
          (str/join "\n"
                    ["<pre><code>def foo"
                     "  # 42 &lt; 9001"
                     "\t'Hello world!'"
                     "end</code></pre>"])
          md-code
          (str/join "\n"
                    ["    def foo"
                     "      # 42 < 9001",
                     "      'Hello world!'"
                     "    end"])]
      (check html-code md-code "We expect code blocks to be converted")))

  (testing "converting list elements"
    (check "1986. What a great season." "1986\\. What a great season." "We expect numbers that could trigger an ol to be escaped")
    (check "<ol>\n\t<li>Hello world</li>\n\t<li>Lorem ipsum</li>\n</ol>"
           (str "1.  Hello world\n"
                "2.  Lorem ipsum")
           "We expect ol elements to be converted properly")
    (check "<ul>\n\t<li>Hello world</li>\n\t<li>Lorem ipsum</li>\n</ul>"
           (str "*   Hello world\n"
                "*   Lorem ipsum")
           "We expect ul elements with line breaks and tabs to be converted properly")
    (check "<ul class='blargh'><li class='first'>Hello world</li><li>Lorem ipsum</li></ul>"
           (str "*   Hello world\n"
                "*   Lorem ipsum")
           "We expect ul elements with attributes to be converted properly")
    (check "<ul><li>Hello world</li><li>Lorem ipsum</li></ul><ul><li>Hello world 2</li><li>Lorem ipsum 2</li></ul>"
           (str "*   Hello world\n"
                "*   Lorem ipsum\n\n"
                "*   Hello world 2\n"
                "*   Lorem ipsum 2")
           "We expect consecutive ul elements to be converted properly")
    (check "<ul><li><p>Hello world</p></li><li>Lorem ipsum</li></ul>"
           (str "*   Hello world\n\n"
                "*   Lorem ipsum")
           "We expect li elements with ps to be converted properly")
    (let [list-with-ps-html
          (str/join "\n"
                    ["<ol>"
                     "  <li>"
                     "    <p>This is a list item with two paragraphs. Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aliquam hendrerit mi posuere lectus.</p>"
                     "    <p>Vestibulum enim wisi, viverra nec, fringilla in, laoreet vitae, risus. Donec sit amet nisl. Aliquam semper ipsum sit amet velit.</p>"
                     "  </li>"
                     "  <li>"
                     "    <p>Suspendisse id sem consectetuer libero luctus adipiscing.</p>"
                     "  </li>"
                     "</ol>"])
          list-with-ps-md
          (str/join "\n"
                    ["1.  This is a list item with two paragraphs. Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aliquam hendrerit mi posuere lectus."
                     ""
                     "    Vestibulum enim wisi, viverra nec, fringilla in, laoreet vitae, risus. Donec sit amet nisl. Aliquam semper ipsum sit amet velit."
                     ""
                     "2.  Suspendisse id sem consectetuer libero luctus adipiscing."])]
      (check list-with-ps-html list-with-ps-md "We expect lists with paragraphs to be converted"))

    (let [nested-list-html
          (str/join "\n"
                    ["<ul>"
                     "  <li>This is a list item at root level</li>"
                     "  <li>This is another item at root level</li>"
                     "  <li>"
                     "    <ul>"
                     "      <li>This is a nested list item</li>"
                     "      <li>This is another nested list item</li>"
                     "      <li>"
                     "        <ul>"
                     "          <li>This is a deeply nested list item</li>"
                     "          <li>This is another deeply nested list item</li>"
                     "          <li>This is a third deeply nested list item</li>"
                     "        </ul>"
                     "      </li>"
                     "    </ul>"
                     "  </li>"
                     "  <li>This is a third item at root level</li>"
                     "</ul>"])
          nested-list-md
          (str/join "\n"
                    ["*   This is a list item at root level"
                     "*   This is another item at root level"
                     "*   *   This is a nested list item"
                     "    *   This is another nested list item"
                     "    *   *   This is a deeply nested list item"
                     "        *   This is another deeply nested list item"
                     "        *   This is a third deeply nested list item"
                     "*   This is a third item at root level"])]
      (check nested-list-html nested-list-md "We expect nested lists to be converted properly"))
    )

  )
