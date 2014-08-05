(ns html-to-md.core
  (:import (java.io StringReader BufferedWriter StringWriter))
  (:require [net.cgrand.enlive-html :as html]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defmulti convert "Converts html tag to markdown format writing result to output (the second arg)"
          (fn [el _ _] (:tag el)))

(declare html-to-md*)

(defn new-line [^BufferedWriter out {:keys [blockquote-nested-level]}]
  "Every time you need to insert new line into output this function must be called.
  It takes into account :blockquote tags and inserts '> ' if needed."
  (doto out
    (.newLine)
    (.write (apply str (repeat blockquote-nested-level "> ")))))

(defmethod convert :body [{:keys [content]} ^BufferedWriter out state]
  (html-to-md* out content state))

(defmethod convert :html [{:keys [content]} ^BufferedWriter out state]
  (html-to-md* out content state))

(defmethod convert :p [{:keys [content]} ^BufferedWriter out state]
  (when content
    (doto out
      (new-line state)
      (new-line state)
      (html-to-md* content state)
      (new-line state))))

(defmethod convert :br [_ ^BufferedWriter out state]
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
              [el# ^BufferedWriter out# state#]
     (write-h (:content el#) out# ~h-level state#)))

(dotimes [i 6]
  (defheading (inc i)))

(defmethod convert :hr [_ ^BufferedWriter out state]
  (doto out
    (new-line state)
    (new-line state)
    (.write "* * *")
    (new-line state)))

(defn quoted-title [title]
  (if title (str " \"" title \") ""))

(defmethod convert :a [{{:keys [href title]} :attrs content :content} ^BufferedWriter out state]
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

(defmethod convert :b [{:keys [content]} ^BufferedWriter out _]
  (write-b-or-strong content out))

(defmethod convert :strong [{:keys [content]} ^BufferedWriter out _]
  (write-b-or-strong content out))

(defn write-i-or-em [content out state]
  (when content
    (doto out
      (.append \_)
      (html-to-md* content state)
      (.append \_))))

(defmethod convert :i [{:keys [content]} ^BufferedWriter out _]
  (write-i-or-em content out))

(defmethod convert :em [{:keys [content]} ^BufferedWriter out _]
  (write-i-or-em content out))

(defmethod convert :img [{{:keys [src alt title]} :attrs} ^BufferedWriter out _]
  (doto out
    (.write "![")
    (.write (if alt alt ""))
    (.write "](")
    (.write (str src))
    (.write (quoted-title title))
    (.append \))))

(defmethod convert :pre [{:keys [content]} ^BufferedWriter out state]
  (let [indent "    "
        code-block (-> (first content)
                       (str/replace #"\t+/g" "  ")          ; convert tabs to spaces
                       (str/replace #"\n" (str "\n" indent)))] ; add indents
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

(defn convert-list [list-type content out list-nested-level state]
  (let [new-nested-level (inc list-nested-level)
        indexed-content (with-li-index content)]
    ;(println "indexed-content: " indexed-content)
    (html-to-md* out
                 indexed-content
                 (-> state
                     (assoc :list-type list-type)           ; specify current list-type
                     (assoc :list-nested-level new-nested-level)))))

(defmethod convert :ol [{:keys [content]} ^BufferedWriter out {:keys [list-nested-level] :as state}]
  (convert-list :ol content out list-nested-level state))

(defmethod convert :ul [{:keys [content]} ^BufferedWriter out {:keys [list-nested-level] :as state}]
  (convert-list :ul content out list-nested-level state))

(defn ol-prefix [li-index]
  (cond
    (< li-index 10) (str li-index ".  ")
    (< li-index 100) (str li-index ". ")
    :else (str li-index ".")))

(defn trim-all-strings [coll]
  (map #(if (string? %) (str/trim %) %) coll))

(defmethod convert :li
           [{{:keys [li-index]} :attrs content :content} ^BufferedWriter out {:keys [list-nested-level list-type] :as state}]
  (doto out
    (new-line state)
    (.write (apply str (repeat (dec list-nested-level) "    ")))
    (.write (if (= :ol list-type)
              (ol-prefix li-index)
              "*   "))
    (html-to-md* (trim-all-strings content) state)))

(defmethod convert :blockquote [{:keys [content]} ^BufferedWriter out state]
  (html-to-md* out content (update-in state [:blockquote-nested-level] inc)))

(defn html-to-md* [out html state]
  (doall (map #(if (map? %)
                (convert % out state)
                (.write out %)) html)))

(defn html-to-md
  "Parses input html stream and writes result to out stream"
  [in out]
  (with-open [^BufferedWriter writer (io/writer out)]
    (html-to-md* writer (html/html-resource in)
                 {:list-nested-level 0
                  :blockquote-nested-level 0})
    (.flush writer)))

(defn html-to-md-string
  "Taking html string as input parses it to md string"
  [html-str]
  (with-open [output (StringWriter.)]
    (html-to-md (StringReader. html-str) output)
    (.toString output)))