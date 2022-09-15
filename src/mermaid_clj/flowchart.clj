(ns mermaid-clj.flowchart)

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

;; ============ flowchart ============

(defn flowchart [direction & forms])

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

;; flowchart TD
;;     A[Start] --> B{Is it?}
;;     B -->|Yes| C[OK]
;;     C --> D[Rethink]
;;     D --> B
;;     B ---->|No| E[End]

;; declaring shapes and adding messages are coupled together...
;; => using a **clojure let block** would solve the problem elegantly.
;; => what's the point of using an embedded DSL when you can't use the host language
;;    to write the desired software?

;; (flow-chart :TD        ; can either be symbol, keyword, or string
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
