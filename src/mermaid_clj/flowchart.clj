(ns mermaid-clj.flowchart)

;; ============ nodes ============

(defn node [& message]
  {:type    :node/normal
   :message message})

(defn round-edge [& message]
  {:type    :node/round-edge
   :message message})

(defn pill [& message]
  {:type    :node/pill
   :message message})

(defn subroutine [& message]
  {:type    :node/subroutine
   :message message})

(defn database [& message]
  {:type    :node/database
   :message message})

(defn circle [& message]
  {:type    :node/circle
   :message message})

(defn ribbon [& message]
  {:type    :node/ribbon
   :message message})

(defn rhombus [& message]
  {:type    :node/rhombus
   :message message})

(defn hexagon [& message]
  {:type    :node/hexagon
   :message message})

(defn slanted [& message]
  {:type    :node/slanted
   :message message})

(defn slanted-alt [& message]
  {:type    :node/slanted-alt
   :message message})

(defn trapezoid [& message]
  {:type    :node/trapezoid
   :message message})

(defn trapezoid-alt [& message]
  {:type    :node/trapezoid-alt
   :message message})

(defn double-circle [& message]
  {:type    :node/double-circle
   :message message})

;; ============ links ============

(defn line [from to
            & message {:keys [length]
                       :or   {length 1}}]
  {:type    :line/normal
   :from    from
   :to      to
   :message message})

(defn arrow [from to
             & message {:keys [length]
                        :or   {length 1}}]
  {:type    :arrow/normal
   :from    from
   :to      to
   :message message})

(defn thick-line [from to
                  & message {:keys [length]
                             :or   {length 1}}]
  {:type    :line/thick
   :from    from
   :to      to
   :message message})

(defn thick-arrow [from to
                   & message {:keys [length]
                              :or   {length 1}}]
  {:type    :arrow/thick
   :from    from
   :to      to
   :message message})

(defn dotted-line [from to
                   & message {:keys [length]
                              :or   {length 1}}]
  {:type    :line/dotted
   :from    from
   :to      to
   :message message})

(defn dotted-arrow [from to
                    & message {:keys [length]
                               :or   {length 1}}]
  {:type    :arrow/dotted
   :from    from
   :to      to
   :message message})

;; ============ subgraph ============

(defn flowchart [])

;; ============ flowchart ============

(defn flowchart [])

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
