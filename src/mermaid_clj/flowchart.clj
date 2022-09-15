(ns mermaid-clj.flowchart)

;; ============ nodes ============

(defn node [& label id]
  {:type  :node/normal
   :label label
   :id    id})

(defn round-edge [& label id]
  {:type  :node/round-edge
   :label label
   :id    id})

(defn pill [& label id]
  {:type  :node/pill
   :label label
   :id    id})

(defn subroutine [& label id]
  {:type  :node/subroutine
   :label label
   :id    id})

(defn database [& label id]
  {:type  :node/database
   :label label
   :id    id})

(defn circle [& label id]
  {:type  :node/circle
   :label label
   :id    id})

(defn ribbon [& label id]
  {:type  :node/ribbon
   :label label
   :id    id})

(defn rhombus [& label id]
  {:type  :node/rhombus
   :label label
   :id    id})

(defn hexagon [& label id]
  {:type  :node/hexagon
   :label label
   :id    id})

(defn slanted [& label id]
  {:type  :node/slanted
   :label label
   :id    id})

(defn slanted-alt [& label id]
  {:type  :node/slanted-alt
   :label label
   :id    id})

(defn trapezoid [& label id]
  {:type  :node/trapezoid
   :label label
   :id    id})

(defn trapezoid-alt [& label id]
  {:type  :node/trapezoid-alt
   :label label
   :id    id})

(defn double-circle [& label id]
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
        label (node :label)]
    (cond (= type "normal")
          (str ())
          (= type "round-edge")
          ()
          (= type "pill")
          ()
          (= type "subroutine")
          ()
          (= type "circle")
          ()
          (= type "ribbon")
          ()
          (= type "rhombus")
          ()
          (= type "hexagon")
          ()
          (= type "slanted")
          ()
          (= type "slanted-alt")
          ()
          (= type "trapezoid")
          ()
          (= type "trapezoid-alt")
          ()
          (= type "double-circle")
          ())))

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
