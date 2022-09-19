(ns mermaid-clj.flowchart
  (require [clojure.core.match :refer [match]])
  (require [clojure.string :as string]))

;; ============ nodes ============

(defn node [& {:keys [label id]
               :or   {label " " id nil}}]
  {:type  :node/normal
   :label label
   :id    id})

(defn round-edge [& {:keys [label id]
                     :or   {label " " id nil}}]
  {:type  :node/round-edge
   :label label
   :id    id})

(defn pill [& {:keys [label id]
               :or   {label " " id nil}}]
  {:type  :node/pill
   :label label
   :id    id})

(defn subroutine [& {:keys [label id]
                     :or   {label " " id nil}}]
  {:type  :node/subroutine
   :label label
   :id    id})

(defn database [& {:keys [label id]
                   :or   {label " " id nil}}]
  {:type  :node/database
   :label label
   :id    id})

(defn circle [& {:keys [label id]
                 :or   {label " " id nil}}]
  {:type  :node/circle
   :label label
   :id    id})

(defn ribbon [& {:keys [label id]
                 :or   {label " " id nil}}]
  {:type  :node/ribbon
   :label label
   :id    id})

(defn rhombus [& {:keys [label id]
                  :or   {label " " id nil}}]
  {:type  :node/rhombus
   :label label
   :id    id})

(defn hexagon [& {:keys [label id]
                  :or   {label " " id nil}}]
  {:type  :node/hexagon
   :label label
   :id    id})

(defn slanted [& {:keys [label id]
                  :or   {label " " id nil}}]
  {:type  :node/slanted
   :label label
   :id    id})

(defn slanted-alt [& {:keys [label id]
                      :or   {label " " id nil}}]
  {:type  :node/slanted-alt
   :label label
   :id    id})

(defn trapezoid [& {:keys [label id]
                    :or   {label " " id nil}}]
  {:type  :node/trapezoid
   :label label
   :id    id})

(defn trapezoid-alt [& {:keys [label id]
                        :or   {label " " id nil}}]
  {:type  :node/trapezoid-alt
   :label label
   :id    id})

(defn double-circle [& {:keys [label id]
                        :or   {label " " id nil}}]
  {:type  :node/double-circle
   :label label
   :id    id})

;; ============ links ============

(defn line [from to
            {:keys [length message]
             :or   {length 1 message " "}}]
  {:type    :line/normal
   :from    from
   :to      to
   :message message
   :length  (if (<= 0 length 3) length
                (throw (IllegalArgumentException. "Invalid length")))})

(defn arrow [from to
             {:keys [length message]
              :or   {length 1 message " "}}]
  {:type    :arrow/normal
   :from    from
   :to      to
   :message message
   :length  (if (<= 0 length 3) length
                (throw (IllegalArgumentException. "Invalid length")))})

(defn thick-line [from to
                  {:keys [length message]
                   :or   {length 1 message " "}}]
  {:type    :line/thick
   :from    from
   :to      to
   :message message
   :length  (if (<= 0 length 3) length
                (throw (IllegalArgumentException. "Invalid length")))})

(defn thick-arrow [from to
                   {:keys [length message]
                    :or   {length 1 message " "}}]
  {:type    :arrow/thick
   :from    from
   :to      to
   :message message
   :length  (if (<= 0 length 3) length
                (throw (IllegalArgumentException. "Invalid length")))})

(defn dotted-line [from to
                   {:keys [length message]
                    :or   {length 1 message " "}}]
  {:type    :line/dotted
   :from    from
   :to      to
   :message message
   :length  (if (<= 0 length 3) length
                (throw (IllegalArgumentException. "Invalid length")))})

(defn dotted-arrow [from to
                    {:keys [length message]
                     :or   {length 1 message " "}}]
  {:type    :arrow/dotted
   :from    from
   :to      to
   :message message
   :length  (if (<= 0 length 3) length
                (throw (IllegalArgumentException. "Invalid length")))})

;; ============ subgraph ============

(defn subgraph [name & forms])

;; ============ predicates ============

(defn node? [form]
  (= "node" (namespace (:type form))))

(defn line? [form]
  (= "line" (namespace (:type form))))

(defn arrow? [form]
  (= "arrow" (namespace (:type form))))

(defn link? [form]
  (or (link? form) (arrow? form)))

;; ============ renderer ============

(def id-maker
  ;; WARNING This is a stateful function
  (let [number (atom 0)]
    (fn []
      (do
        (swap! number inc)
        (str "id" @number)))))

(defn render-node
  "Render a single node."
  [node]
  (let [type  (name (node :type))
        id    (if (node :id) (node :id) (id-maker))
        label (node :label)]
    (cond (= type "normal")
          (str id "[" label "]")
          (= type "round-edge")
          (str id "(" label ")")
          (= type "pill")
          (str id "([" label "])")
          (= type "subroutine")
          (str id "[[" label "]]")
          (= type "database")
          (str id "[(" label ")]")
          (= type "circle")
          (str id "((" label "))")
          (= type "ribbon")
          (str id ">" label "]")
          (= type "rhombus")
          (str id "{" label "}")
          (= type "hexagon")
          (str id "{{" label "}}")
          (= type "slanted")
          (str id "[/" label "/]")
          (= type "slanted-alt")
          (str id "[\\" label "\\]")
          (= type "trapezoid")
          (str id "[/" label "\\]")
          (= type "trapezoid-alt")
          (str id "[\\" label "/]")
          (= type "double-circle")
          (str id "(((" label ")))"))))

(defn render-line
  "Render a line, together with its including nodes."
  [line]
  (let [type    (name (line :type))
        from    (name (line :from))
        to      (name (line :to))
        length  (name (line :length))
        message (name (line :message))]
    (cond (= type "normal")
          (match length
            1 (str from "---" to)
            2 (str from "----" to)
            3 (str from "-----" to)
            :else (throw (IllegalAccessException. "Link length cannot be over 4")))
          (= type "thick")
          (match length
            1 (str from "===" to)
            2 (str from "====" to)
            3 (str from "=====" to)
            :else (throw (IllegalAccessException. "Link length cannot be over 4")))
          (= type "dotted")
          (match length
            1 (str from "-.-" to)
            2 (str from "-..-" to)
            3 (str from "-...-" to)
            :else (throw (IllegalAccessException. "Link length cannot be over 4"))))))

(defn render-arrow
  "Render a arrow, together with its including nodes."
  [arrow]
  (let [type    (name (arrow :type))
        from    (name (arrow :from))
        to      (name (arrow :to))
        length  (name (arrow :length))
        message (name (arrow :message))]
    (cond (= type "normal")
          (match length
            1 (str from "--->" to)
            2 (str from "---->" to)
            3 (str from "----->" to)
            :else (throw (IllegalAccessException. "Link length cannot be over 4")))
          (= type "thick")
          (match length
            1 (str from "===>" to)
            2 (str from "====>" to)
            3 (str from "=====>" to)
            :else (throw (IllegalAccessException. "Link length cannot be over 4")))
          (= type "dotted")
          (match length
            1 (str from "-.->" to)
            2 (str from "-..->" to)
            3 (str from "-...->" to)
            :else (throw (IllegalAccessException. "Link length cannot be over 4"))))))

(defn render-link
  "Render a link, together with its including nodes."
  [link]
  (let [type    (namespace (arrow :type))
        subtype (name (arrow :type))
        from    (name (arrow :from))
        to      (name (arrow :to))
        length  (name (arrow :length))
        message (name (arrow :message))]
    (cond (= type "line")
          (render-line subtype from to length message)
          (= type "arrow")
          (render-arrow subtype from to length message))))

(defn dispatch-renderer [form]
  (let [type (namespace (node :type))]
    (cond (= type "node") render-node
          (= type "link") render-link)))

(defn render-with-indent [indent-level form]
  (trampoline (partial (dispatch-renderer form) indent-level) form))

(defn render [form]
  (render-with-indent 4 form))

;; ============ main ============

(defn flowchart
  "Make a Flowchart."
  [direction & forms]
  (str (name direction)
       (string/join (interpose "\n" (mapv render forms)))))

;; Declaring shapes and adding messages are coupled together...
;; => using a **clojure let block** would solve the problem elegantly.
;; => what's the point of using an embedded DSL when you can't use the host language
;;    to write the desired software?

;; flowchart TD
;;     A[Start] --> B{Is it?}
;;     B -->|Yes| C[OK]
;;     C --> D[Rethink]
;;     D --> B
;;     B ---->|No| E[End]

;; translates to:

;; (flow-chart :TD
;;    (let [A (node "Start")
;;          B (rhombus "Is it?")
;;          C (node "OK")
;;          D (node "Rethink")
;;          E (node "End")]
;;      (arrow A B)
;;      (arrow B C "Yes")
;;      (arrow C D)
;;      (arrow D B)
;;      (arrow B E "No")))
