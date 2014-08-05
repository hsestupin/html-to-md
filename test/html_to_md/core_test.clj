(ns html-to-md.core-test
  (:require [clojure.test :refer :all]
            [html-to-md.core :refer :all]))

(deftest simple-convert

  (testing "converting p elements"
    (is (= (html-to-md-string "<p>Lorem ipsum</p>") "Lorem ipsum") "We expect p tags to be wrapped with two line breaks"))

  (testing "Simple convert html to md"
    (let [line-sep (System/lineSeparator)]
      (is (= (str line-sep line-sep "### Some HTML" line-sep)
             (html-to-md-string "<h3>Some HTML</h3>"))))))
