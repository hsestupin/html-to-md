(ns html-to-md.core
  (:import (java.io StringReader StringWriter Writer))
  (:require [net.cgrand.enlive-html :as html]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defmulti convert "Converts html tag to markdown format writing result to output (the second arg)"
          (fn [el _ _] (:tag el)))

(declare html-to-md*)

(defn new-line [^Writer out {:keys [blockquote-nested-level]}]
  "Every time you need to insert new line into output this function must be called.
  It takes into account :blockquote tags and inserts '> ' if needed."
  (doto out
    (.write "\n")
    (.write (apply str (repeat blockquote-nested-level "> ")))))

(defmethod convert :body [{:keys [content]} ^Writer out state]
  (html-to-md* out content state))

(defmethod convert :html [{:keys [content]} ^Writer out state]
  (html-to-md* out content state))

(defmethod convert :p [{:keys [content]} ^Writer out state]
  (when content
    (doto out
      (new-line state)
      (new-line state)
      (html-to-md* content state)
      (new-line state))))

(defmethod convert :br [_ ^Writer out state]
  (new-line out state))

(defn write-h [content out h-level state]
  (doto out
    (new-line state)
    (new-line state)
    (.write (apply str (repeat h-level \#)))
    (.write " ")
    (html-to-md* content {})
    (new-line state)))

(defmacro defheading [h-level]
  `(defmethod convert (keyword (str "h" ~h-level))
              [el# ^Writer out# state#]
     (write-h (:content el#) out# ~h-level state#)))

(dotimes [i 6]
  (defheading (inc i)))

(defmethod convert :hr [_ ^Writer out state]
  (doto out
    (new-line state)
    (new-line state)
    (.write "* * *")
    (new-line state)))

(defn quoted-title [title]
  (if title (str " \"" title \") ""))

(defmethod convert :a [{{:keys [href title]} :attrs content :content} ^Writer out state]
  (if href
    (doto out
      (.append \[)
      (html-to-md* content state)
      (.append \])
      (.append \()
      (.write href)
      (.write (quoted-title title))
      (.append \)))))

(defn write-b-or-strong [content out state]
  (when content
    (doto out
      (.write "**")
      (html-to-md* content state)
      (.write "**"))))

(defmethod convert :b [{:keys [content]} ^Writer out state]
  (write-b-or-strong content out state))

(defmethod convert :strong [{:keys [content]} ^Writer out state]
  (write-b-or-strong content out state))

(defn write-i-or-em [content out state]
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

(defn trim-code [code]
  (let [length (.length code)]
    (if (and
          (.endsWith code "`")
          (.startsWith code "`")
          (> length 1))
      (.substring code 1 (dec length)))))

(defmethod convert :pre [{:keys [content]} ^Writer out state]
  (let [indent (apply str (repeat 4 \space))
        code-block (-> (html-to-md* content state)
                       (trim-code)
                       (str/replace #"(?m)^\t+" "  ")       ; convert tabs to spaces
                       (str/replace #"\n" (str "\n" indent))
                       )]                                   ; add indents
    (doto out
      (new-line state)
      (new-line state)
      (.write (str indent code-block))
      (new-line state))))

(defn with-li-index [coll]
  (loop [indexed-coll [] [head & tail] coll li-index 1]
    (if (nil? head)
      indexed-coll
      (if (= :li (:tag head))
        (recur (conj indexed-coll (assoc-in head [:attrs :li-index] li-index)) tail (inc li-index))
        (recur (conj indexed-coll (if (string? head) (str/trim head) head)) tail li-index)))))

(defn convert-list [list-type content out state]
  (let [indexed-content (with-li-index content)]
    (let [list (-> (html-to-md* indexed-content
                                (-> state
                                    (assoc :list-type list-type))) ; specify current list-type
                   (str/replace #"([ \t]*\n|\s+)$" ""))]
      (doto out
        (new-line state)
        (new-line state)
        (.write list)))))

(defmethod convert :ol [{:keys [content]} ^Writer out state]
  (convert-list :ol content out state))

(defmethod convert :ul [{:keys [content]} ^Writer out state]
  (convert-list :ul content out state))

(defn ol-prefix [li-index]
  (cond
    (< li-index 10) (str li-index ".  ")
    (< li-index 100) (str li-index ". ")
    :else (str li-index ".")))

(defmethod convert :li
           [{{:keys [li-index]} :attrs content :content} ^Writer out {:keys [list-type] :as state}]
  (let [indent (apply str (repeat 4 \space))
        li-block (-> (html-to-md* content state)
                     (str/replace #"\n" (str "\n" indent))  ;prefix this <li> content with indent
                     (str/replace #"^\s+" ""))]             ;trim beginning whitespaces for correct list rendering
    ;(println "li-block:" li-block)
    (doto out
      (.write (if (= :ol list-type)
                (ol-prefix li-index)
                "*   "))
      (.write li-block)
      (new-line state))))

(defmethod convert :blockquote [{:keys [content]} ^Writer out state]
  (html-to-md* out content (update-in state [:blockquote-nested-level] inc)))

(defmethod convert :default [{:keys [tag content]} ^Writer out state]
  (doto out
    (.write (str "<" tag ">"))
    (html-to-md* content (update-in state [:blockquote-nested-level] inc))
    (.write (str "</" tag ">"))))

(defn html-to-md*
  ([html state]
   (with-open [^Writer out (StringWriter.)]
     (html-to-md* out html state)
     (.flush out)
     (.toString out)))
  ([out html state]
   (doall (map #(if (map? %)
                 (convert % out state)
                 (do
                   (println "WRITING STRING:'" % "'") (.write out %))) html))))

(defn html-to-md
  "Parses input html stream and writes result to out stream"
  [in out]
  (with-open [writer (io/writer out)]
    (html-to-md* writer (html/html-resource in)
                 {:blockquote-nested-level 0})
    (.flush writer)
    out))

(defn clean-up [^String result]
  (-> result
      (str/replace #"^[\t\r\n]+|[\t\r\n ]+$" "")            ; trim leading/trailing whitespace. Preserve spaces at the beginning
      (str/replace #"\n\s+\n" "\n\n")
      (str/replace #"\n{3,}" "\n\n")))                      ;limit consecutive linebreaks to 2

(defn html-to-md-string
  "Taking html string as input parses it to md string"
  [html-str]
  (with-open [output (StringWriter.)]
    (->
      (html-to-md (StringReader. html-str) output)
      (.toString)
      (clean-up))))