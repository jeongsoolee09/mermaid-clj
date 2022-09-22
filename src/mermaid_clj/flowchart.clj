(ns mermaid-clj.flowchart
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as string]))

;; ============ nodes ============

(defn node
  ([id]
   {:type  :node/normal
    :id    id
    :label " "})
  ([id label]
   {:type  :node/normal
    :id    id
    :label label}))

(defn round-edge
  ([id]
   {:type  :node/round-edge
    :id    id
    :label " "})
  ([id label]
   {:type  :node/round-edge
    :id    id
    :label label}))

(defn pill
  ([id]
   {:type  :node/pill
    :id    id
    :label " "})
  ([id label]
   {:type  :node/pill
    :id    id
    :label label}))

(defn subroutine
  ([id]
   {:type  :node/subroutine
    :id    id
    :label " "})
  ([id label]
   {:type  :node/subroutine
    :id    id
    :label label}))

(defn database
  ([id]
   {:type  :node/database
    :id    id
    :label " "})
  ([id label]
   {:type  :node/database
    :id    id
    :label label}))

(defn circle
  ([id]
   {:type  :node/circle
    :id    id
    :label " "})
  ([id label]
   {:type  :node/circle
    :id    id
    :label label}))

(defn ribbon
  ([id]
   {:type  :node/ribbon
    :id    id
    :label " "})
  ([id label]
   {:type  :node/ribbon
    :id    id
    :label label}))

(defn rhombus
  ([id]
   {:type  :node/rhombus
    :id    id
    :label " "})
  ([id label]
   {:type  :node/rhombus
    :id    id
    :label label}))

(defn hexagon
  ([id]
   {:type  :node/hexagon
    :id    id
    :label " "})
  ([id label]
   {:type  :node/hexagon
    :id    id
    :label label}))

(defn slanted
  ([id]
   {:type  :node/slanted
    :id    id
    :label " "})
  ([id label]
   {:type  :node/slanted
    :id    id
    :label label}))

(defn slanted-alt
  ([id]
   {:type  :node/slanted-alt
    :id    id
    :label " "})
  ([id label]
   {:type  :node/slanted-alt
    :id    id
    :label label}))

(defn trapezoid
  ([id]
   {:type  :node/trapezoid
    :id    id
    :label " "})
  ([id label]
   {:type  :node/trapezoid
    :id    id
    :label label}))

(defn trapezoid-alt
  ([id]
   {:type  :node/trapezoid-alt
    :id    id
    :label " "})
  ([id label]
   {:type  :node/trapezoid-alt
    :id    id
    :label label}))

(defn double-circle
  ([id]
   {:type  :node/double-circle
    :id    id
    :label " "})
  ([id label]
   {:type  :node/double-circle
    :id    id
    :label label}))

;; ============ links ============

(defn line
  ([from to]
   {:type    :line/normal
    :from    from
    :to      to
    :message " "
    :length  1})
  ([from to message]
   {:type    :line/normal
    :from    from
    :to      to
    :message message
    :length  1})
  ([from to message length]
   {:type    :line/normal
    :from    from
    :to      to
    :message message
    :length  (if (<= 0 length 3) length
                 (throw (IllegalArgumentException. "Invalid length")))}))

(defn arrow
  ([from to]
   {:type    :arrow/normal
    :from    from
    :to      to
    :head    :normal
    :message " "
    :length  1})
  ([from to head]
   {:type    :arrow/normal
    :from    from
    :to      to
    :head    head
    :message " "
    :length  1})
  ([from to head message]
   {:type    :arrow/normal
    :from    from
    :to      to
    :head    head
    :message message
    :length  1})
  ([from to head message length]
   {:type    :arrow/normal
    :from    from
    :to      to
    :head    head
    :message message
    :length  (if (<= 0 length 3) length
                 (throw (IllegalArgumentException. "Invalid length")))}))

(defn thick-line
  ([from to]
   {:type    :line/thick
    :from    from
    :to      to
    :message " "
    :length  1})
  ([from to message]
   {:type    :line/thick
    :from    from
    :to      to
    :message message
    :length  1})
  ([from to message length]
   {:type    :line/thick
    :from    from
    :to      to
    :message message
    :length  (if (<= 0 length 3) length
                 (throw (IllegalArgumentException. "Invalid length")))}))

(defn thick-arrow
  ([from to]
   {:type    :arrow/thick
    :from    from
    :to      to
    :head    :normal
    :message " "
    :length  1})
  ([from to head]
   {:type    :arrow/thick
    :from    from
    :to      to
    :head    head
    :message " "
    :length  1})
  ([from to head message]
   {:type    :arrow/thick
    :from    from
    :to      to
    :head    head
    :message message
    :length  1})
  ([from to head message length]
   {:type    :arrow/thick
    :from    from
    :to      to
    :head    head
    :message message
    :length  (if (<= 0 length 3) length
                 (throw (IllegalArgumentException. "Invalid length")))}))

(defn dotted-line
  ([from to]
   {:type    :line/dotted
    :from    from
    :to      to
    :message " "
    :length  1})
  ([from to message]
   {:type    :line/dotted
    :from    from
    :to      to
    :message message
    :length  1})
  ([from to message length]
   {:type    :line/dotted
    :from    from
    :to      to
    :message message
    :length  (if (<= 0 length 3) length
                 (throw (IllegalArgumentException. "Invalid length")))}))

(defn dotted-arrow
  ([from to]
   {:type    :arrow/dotted
    :from    from
    :to      to
    :head    :normal
    :message " "
    :length  1})
  ([from to head]
   {:type    :arrow/dotted
    :from    from
    :to      to
    :head    head
    :message " "
    :length  1})
  ([from to head message]
   {:type    :arrow/dotted
    :from    from
    :to      to
    :head    head
    :message message
    :length  1})
  ([from to head message length]
   {:type    :arrow/dotted
    :from    from
    :to      to
    :head    head
    :message message
    :length  (if (<= 0 length 3) length
                 (throw (IllegalArgumentException. "Invalid length")))}))

;; ============ predicates ============

(defn node? [form]
  (= "node" (namespace (:type form))))

(defn line? [form]
  (= "line" (namespace (:type form))))

(defn arrow? [form]
  (= "arrow" (namespace (:type form))))

(defn link? [form]
  (or (link? form) (arrow? form)))

(defn js-label? [form]
  (let [type (name (:type form))]
    (or (= type "call") (= type "href"))))

;; ============ positions ============

(defn chain-links [& links]
  {:type  :position/chain-links
   :links links})

(defn parallel-links [& links]
  {:type  :position/parallel-links
   :links links})

(defn parallel-nodes [arrow & node-colls]
  {:type       :position/parallel-nodes
   :arrow      arrow
   :node-colls node-colls})

;; ============ subgraph ============

(defn subgraph
  [id label & forms]
  {:type  :block/subgraph
   :id    id
   :label label
   :forms forms})

;; ============ labels ============

(defn direction [direction]
  {:type      :label/direction
   :direction :direction})

(defn call-on-click [node callback-name message]
  {:type     :label/call
   :node     node
   :callback callback-name
   :message  message})

(defn href-on-click [node href message
                     & {:keys [open-in]
                        :or   {open-in ""}}]
  {:type    :label/href
   :node    node
   :href    href
   :message message
   :open-in open-in})

;; ============ renderer ============

(declare render-with-indent)
(declare dispatch-renderer)

(def id-maker
  ;; WARNING This is a stateful function
  (let [number (atom 0)]
    (fn []
      (do
        (swap! number inc)
        (str "id" @number)))))

(defn make-indent [indent]
  (string/join (for [_ (range indent)] " ")))

(defn render-node
  "Render a single node."
  [indent-level node]
  (let [type   (name (node :type))
        id     (if (node :id) (node :id) (id-maker))
        label  (node :label)
        indent (make-indent indent-level)]
    (str indent
         (match [type]
           ["normal"]        (str id   "[" label "]")
           ["round-edge"]    (str id   "(" label ")")
           ["pill"]          (str id  "([" label "])")
           ["subroutine"]    (str id  "[[" label "]]")
           ["database"]      (str id  "[(" label ")]")
           ["circle"]        (str id  "((" label "))")
           ["ribbon"]        (str id   ">" label "]")
           ["rhombus"]       (str id   "{" label "}")
           ["hexagon"]       (str id  "{{" label "}}")
           ["slanted"]       (str id  "[/" label "/]")
           ["slanted-alt"]   (str id "[\\" label "\\]")
           ["trapezoid"]     (str id  "[/" label "\\]")
           ["trapezoid-alt"] (str id "[\\" label "/]")
           ["double-circle"] (str id "(((" label ")))")))))

(defn- iter-string [num string]
  (string/join (repeat num string)))

(defn render-line
  "Render a line, together with its including nodes."
  [indent-level line]
  (let [type    (name (line :type))
        from    (name (line :from))
        to      (name (line :to))
        length  (name (line :length))
        message (name (line :message))
        indent  (make-indent indent-level)]
    (str indent
         (match [type]
           ["normal"] (str from (iter-string (+ length 2) "-")
                           "|" message "|" to)
           ["thick"]  (str from (iter-string (+ length 2) "=")
                           "|" message "|" to)
           ["dotted"] (str from "-" (iter-string length ".") "-"
                           "|" message "|" to)))))

(defn- arrow-head->string [arrow-head]
  (match [(name arrow-head)]
    ["normal"] ">"
    ["round"]  "o"
    ["cross"]  "x"
    :else      (throw (IllegalArgumentException.
                        (name arrow-head)))))

(defn render-arrow
  "Render a arrow, together with its including nodes."
  [indent-level arrow]
  (let [type    (name (arrow :type))
        from    (name (arrow :from))
        to      (name (arrow :to))
        head    (arrow-head->string (name (arrow :head)))
        length  (name (arrow :length))
        message (name (arrow :message))
        indent  (make-indent indent-level)]
    (str indent
         (match [type]
           ["normal"] (str from (iter-string (+ length 1) "-") head
                           "|" message "|" to)
           ["thick"]  (str from (iter-string (+ length 1) "=") head
                           "|" message "|" to)
           ["dotted"] (str from "-" (iter-string (+ length 1) ".") "-" head
                           "|" message "|" to)))))

(defn render-link
  "Render a link, together with its including nodes."
  [indent-level link]
  (let [type    (namespace (arrow :type))
        subtype (name (arrow :type))
        from    (name (arrow :from))
        to      (name (arrow :to))
        length  (name (arrow :length))
        message (name (arrow :message))
        indent  (make-indent indent-level)]
    (str indent
         (match [type]
           ["line"]  (render-line  indent-level subtype from to length message)
           ["arrow"] (render-arrow indent-level subtype from to length message)))))

(defn- truncate-link [rendered-link]
  (->> rendered-link
       (string/trim)
       (seq)
       (drop-while #(not= \space %))
       (string/join)))

(defn render-position [indent-level position]
  (let [type   (name (position :type))
        indent (make-indent indent-level)]
    (str indent
         (match [type]
           ["chain-links"]
           (let [links          (:links position)
                 rendered-links (map (partial render-with-indent (+ indent-level 4)) links)]
             (str (first rendered-links)
                  (string/join (map truncate-link (rest rendered-links)))))
           ["parallel-links"]
           (let [links (:links position)]
             (string/join " & " (map (partial render-with-indent (+ indent-level 4)) links)))
           ["parallel-nodes"]
           (let [node-colls (:node-colls position)]
             (string/join (str " " (render-arrow arrow) " ")
                          (map (partial string/join " & ")
                               (map render-node node-colls))))))))

(defn render-subgraph [indent-level subgraph]
  (let [id              (:id subgraph)
        label           (:label subgraph)
        following-forms (:forms subgraph)
        indent          (make-indent indent-level)]
    (str indent "subgraph" " " id " " (if label (str "[" label "]") "")
         (string/join "\n" (map (partial render-with-indent (+ )) following-forms))
         "\nend")))

(defn render-js-label [indent-level js-label]
  (let [type    (name (:type js-label))
        node    (:node js-label)
        message (:message js-label)]
    (match [type]
      ["call"]
      (let [callback-name (:callback js-label)]
        (str "click" " " callback-name " " "\"" message "\""))
      ["href"]
      (let [href    (:href js-label)
            open-in (:open-in js-label)]
        (str "click" " " node " " href "\"" message "\"")))))

(defn render-label [indent-level label]
  (let [indent (make-indent indent-level)]
    (if (js-label? label)
      (render-js-label indent-level label)
      (str "direction" " " (name (:direction label))))))

(defn- dispatch-renderer [form]
  (match [(namespace (node :type))]
    ["node"]     render-node
    ["link"]     render-link
    ["position"] render-position))

(defn- render-with-indent [indent-level form]
  (trampoline (partial (dispatch-renderer form) indent-level) form))

(defn render [form]
  (render-with-indent 4 form))

;; ============ main ============

(defn flow-chart
  "Make a Flowchart."
  [direction & forms]
  (str "flowchart" (name direction)
       (string/join (interpose "\n" (mapv render forms)))))

(comment "========================================"
  (render (arrow (node "Start") (rhombus "Is it?")))

  (flow-chart :TD
              (let [A (node "Start")
                    B (rhombus "Is it?")
                    C (node "OK")
                    D (node "Rethink")
                    E (node "End")]
                (arrow A B)
                (arrow B C :message "Yes")
                (arrow C D)
                (arrow D B)
                (arrow B E :message "No")))
  )

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
