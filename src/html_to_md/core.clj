(ns html-to-md.core
  (:import (java.io StringReader StringWriter Writer)
           (org.xml.sax ContentHandler)
           (org.xml.sax.ext DefaultHandler2))
  (:require [net.cgrand.enlive-html :as html]
            [net.cgrand.xml :as xml]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defmulti convert "Converts html tag to markdown format writing result to output (the second arg)"
          (fn [el _ _] (:tag el)))

(declare html-to-md*)

(defn- clean-up [^String result]
  (-> result
      (str/replace #"^[\t\r\n]+|[\t\r\n ]+$" "")            ; trim leading/trailing whitespace. Preserve spaces at the beginning
      (str/replace #"\n\s+\n" "\n\n")
      (str/replace #"\n{3,}" "\n\n")))                      ;limit consecutive linebreaks to 2

(defmethod convert :body [{:keys [content]} ^Writer out state]
  (html-to-md* out content state))

(defmethod convert :html [{:keys [content]} ^Writer out state]
  (html-to-md* out content state))

(defmethod convert :p [{:keys [content]} ^Writer out state]
  (when content
    (doto out
      (.write "\n")
      (.write "\n")
      (html-to-md* content state)
      (.write "\n"))))

(defmethod convert :br [_ ^Writer out _]
  (.write out "\n"))

(defn- write-h [content out h-level _]
  (doto out
    (.write "\n")
    (.write "\n")
    (.write (apply str (repeat h-level \#)))
    (.write " ")
    (html-to-md* content {})
    (.write "\n")))

(defmacro defheading [h-level]
  `(defmethod convert (keyword (str "h" ~h-level))
              [el# ^Writer out# state#]
     (write-h (:content el#) out# ~h-level state#)))

(dotimes [i 6]
  (defheading (inc i)))

(defmethod convert :hr [_ ^Writer out _]
  (doto out
    (.write "\n")
    (.write "\n")
    (.write "* * *")
    (.write "\n")))

(defn- quoted-title [title]
  (if title (str " \"" title \") ""))

(defmethod convert :a [{{:keys [href title]} :attrs content :content :as node} ^Writer out state]
  (if href
    (doto out
      (.append \[)
      (html-to-md* content state)
      (.append \])
      (.append \()
      (.write href)
      (.write (quoted-title title))
      (.append \)))
    (.write out (apply str (html/emit* node)))))

(defn- write-b-or-strong [content out state]
  (when content
    (doto out
      (.write "**")
      (html-to-md* content state)
      (.write "**"))))

(defmethod convert :b [{:keys [content]} ^Writer out state]
  (write-b-or-strong content out state))

(defmethod convert :strong [{:keys [content]} ^Writer out state]
  (write-b-or-strong content out state))

(defn- write-i-or-em [content out state]
  (when content
    (doto out
      (.append \_)
      (html-to-md* content state)
      (.append \_))))

(defmethod convert :i [{:keys [content]} ^Writer out state]
  (write-i-or-em content out state))

(defmethod convert :em [{:keys [content]} ^Writer out state]
  (write-i-or-em content out state))

(defmethod convert :code [{:keys [content]} ^Writer out state]
  (when content
    (doto out
      (.append \`)
      (html-to-md* content state)
      (.append \`))))

(defmethod convert :img [{{:keys [src alt title]} :attrs} ^Writer out _]
  (doto out
    (.write "![")
    (.write (if alt alt ""))
    (.write "](")
    (.write (str src))
    (.write (quoted-title title))
    (.append \))))

(defn- trim-code [code]
  (let [length (.length code)]
    (if (and
          (.endsWith code "`")
          (.startsWith code "`")
          (> length 1))
      (.substring code 1 (dec length)))))

(defmethod convert :pre [{:keys [content]} ^Writer out state]
  (let [indent (apply str (repeat 4 \space))
        code-block (-> (html-to-md* content state)
                       trim-code
                       (str/replace #"(?m)^\t+" "  ")       ; convert tabs to spaces
                       (str/replace #"\n" (str "\n" indent))
                       )]                                   ; add indents
    (doto out
      (.write "\n")
      (.write "\n")
      (.write (str indent code-block))
      (.write "\n"))))

(defn- with-li-index [coll]
  (loop [indexed-coll [] [head & tail] coll li-index 1]
    (if (nil? head)
      indexed-coll
      (if (= :li (:tag head))
        (recur (conj indexed-coll (assoc-in head [:attrs :li-index] li-index)) tail (inc li-index))
        (recur (conj indexed-coll (if (string? head) (str/trim head) head)) tail li-index)))))

(defn- convert-list [list-type content out state]
  (let [indexed-content (with-li-index content)]
    (let [list (-> (html-to-md* indexed-content
                                (-> state
                                    (assoc :list-type list-type))) ; specify current list-type
                   (str/replace #"([ \t]*\n|\s+)$" ""))]
      (doto out
        (.write "\n")
        (.write "\n")
        (.write list)))))

(defmethod convert :ol [{:keys [content]} ^Writer out state]
  (convert-list :ol content out state))

(defmethod convert :ul [{:keys [content]} ^Writer out state]
  (convert-list :ul content out state))

(defn- ol-prefix [li-index]
  (if (< li-index 10)
    (str li-index ".  ")
    (str li-index ". ")))

(defmethod convert :li
           [{{:keys [li-index]} :attrs content :content} ^Writer out {:keys [list-type] :as state}]
  (let [indent (apply str (repeat 4 \space))
        li-block (-> (html-to-md* content (assoc state :inside-li true))
                     (str/replace #"^\s+" "")               ;trim beginning whitespaces for correct list rendering
                     (str/replace #"\n" (str "\n" indent))  ;prefix this <li> content with indent
                     )]
    (doto out
      (.write (if (= :ol list-type)
                (ol-prefix li-index)
                "*   "))
      (.write li-block)
      (.write "\n"))))

(defmethod convert :blockquote [{:keys [content]} ^Writer out state]
  (let [blockquoted (-> (html-to-md* content state)
                        (str/replace #"^\s+|\s+$" "")
                        clean-up
                        (str/replace #"(?m)^" "> ")         ; add prefix "> " before each line
                        (str/replace #"(?m)^(>([ \t]{2,}>)+)" "> >") ;handle nested blockquotes
                        )]
    (.write out "\n")
    (.write out blockquoted)))

(defmethod convert :default [node ^Writer out _]
  (.write out (apply str (html/emit* node))))

(defn html-to-md*
  ([html-seq state]
   (with-open [^Writer out (StringWriter.)]
     (html-to-md* out html-seq state)
     (.flush out)
     (.toString out)))
  ([out html-seq state]
   (letfn [(backslash-escape-period
             [s]
             (str/replace s #"(?m)^(\s{0,3}\d+)\. " "$1\\\\. "))
           (write-string
             [s out state]
             (let [s (backslash-escape-period s)]
               (if (contains? state :list-type)             ; carefully write strings inside list
                 (if (re-matches #"^\n[ \t]+" s)
                   (when (re-matches #"\n+" s)
                     (.write out s))
                   (.write out s))
                 (.write out s))))]
     (doall (map #(if (map? %)
                   (convert % out state)
                   (write-string % out state))
                 html-seq))
     out)))

(defn- html-seq-to-md-string
  [html-seq]
  (with-open [output (StringWriter.)]
    (-> output
        (html-to-md* html-seq {})
        (.toString)
        clean-up)))

(defn html-to-md-string
  "Taking html string as input parses it to md string"
  ([^String html-str]
   (html-seq-to-md-string (html/html-resource (StringReader. html-str)))))

(defn html-to-md
  "Parses input html stream and writes result to out stream"
  [in out]
  (with-open [writer (io/writer out)]
    (.write writer (html-seq-to-md-string (html/html-resource in)))
    (.flush writer)
    out))
