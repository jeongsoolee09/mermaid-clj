(ns mermaid-clj.sequence
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as string]))

(defmacro use-like-this [& _])

(defn make-indent [indent]
  (string/join (for [_ (range indent)] " ")))

(defn color->rgb [color]
  (cond (= (name color) :green)  "rgb(0,255,0,0.1)"
        (= (name color) :blue)   "rgb(0,0,255,0.1)"
        (= (name color) :red)    "rgb(255,0,0,0.2)"
        (= (name color) :yellow) "rgb(255,255,0,0.5)"
        (= (name color) :gray)   "rgb(0,0,0,0.1)"
        :else                    "rgb(0,0,0,0)")) ; fall back to black

(defn append-activate [arrow-str activate deactivate]
  (match [activate deactivate]
    [false false] arrow-str
    [true false] (str arrow-str "+")
    [false true] (str arrow-str "-")
    [true true] (throw (IllegalArgumentException.))))

(defn arrow->str [arrow]
  "Get the arrow representation of a given arrow."
  (let [type1      (namespace (arrow :type))
        type2      (name (arrow :type))
        activate   (arrow :activate)
        deactivate (arrow :deactivate)]
    (match [(name type1) (name type2)]
      ["solid" "line"] (append-activate "->" activate deactivate)
      ["solid" "arrow"] (append-activate "->>" activate deactivate)
      ["solid" "cross"] (append-activate "-x" activate deactivate)
      ["solid" "open"] (append-activate "-)" activate deactivate)
      ["dotted" "line"] (append-activate "-->" activate deactivate)
      ["dotted" "arrow"] (append-activate "-->>" activate deactivate)
      ["dotted" "cross"] (append-activate "--x" activate deactivate)
      ["dotted" "open"] (append-activate "--)" activate deactivate))))

;; ============ DSL APIs ============

(defn actor-name
  "Get the name of the actor by replacing the hyphens in an actor name."
  [actor]
  (str "\"" (string/replace (name actor) #"\-" " ") "\""))

;; ============ labels ============

(defn autonumber
  "Autonumber label."
  []
  {:type :label/autonumber})

(defn participants
  "Participants."
  [& participating-actors]
  {:type   :label/participants
   :actors participating-actors})

(defn activate
  "Activate the receiver."
  [actor]
  {:type  :label/activate
   :actor actor})

(defn deactivate
  "Deactivate the receiver."
  [actor]
  {:type  :label/deactivate
   :actor actor})

;; ============ arrows ============

(defn solid-line
  "Solid line without arrowhead from an actor to another."
  [actor-from actor-to message & {:keys [activate deactivate]
                                  :or   {activate   false
                                         deactivate false}}]
  (use-like-this "all data can be a symbol, keyword, or a string."
    (solid-line :alice :bob "hihi")
    (solid-line 'bob 'alice "hoho"))
  {:type       :solid/line
   :from       actor-from
   :to         actor-to
   :message    message
   :activate   activate
   :deactivate deactivate})

(defn solid-arrow
  "Solid line with arrowhead from an actor to another."
  [actor-from actor-to message & {:keys [activate deactivate]
                                  :or   {activate   false
                                         deactivate false}}]
  (use-like-this "all data can be a symbol, keyword, or a string."
    (solid-arrow :alice :bob "hihi")
    (solid-arrow 'bob 'alice "hoho"))
  {:type       :solid/arrow
   :from       actor-from
   :to         actor-to
   :message    message
   :activate   activate
   :deactivate deactivate})

(defn solid-cross
  "Solid line with cross arrowhead from an actor to another."
  [actor-from actor-to message & {:keys [activate deactivate]
                                  :or   {activate   false
                                         deactivate false}}]
  (use-like-this "all data can be a symbol, keyword, or a string."
    (solid-cross :alice :bob "hihi")
    (solid-cross 'bob 'alice "hoho"))
  {:type       :solid/cross
   :from       actor-from
   :to         actor-to
   :message    message
   :activate   activate
   :deactivate deactivate})

(defn solid-open
  "Solid line with open arrowhead from an actor to another."
  [actor-from actor-to message & {:keys [activate deactivate]
                                  :or   {activate   false
                                         deactivate false}}]
  (use-like-this "all data can be a symbol, keyword, or a string."
    (solid-open :alice :bob "hihi")
    (solid-open 'bob 'alice "hoho"))
  {:type       :solid/open
   :from       actor-from
   :to         actor-to
   :message    message
   :activate   activate
   :deactivate deactivate})

(defn dotted-line
  "Dotted line without arrowhead from an actor to another."
  [actor-from actor-to message & {:keys [activate deactivate]
                                  :or   {activate   false
                                         deactivate false}}]
  (use-like-this "all data can be a symbol, keyword, or a string."
    (dotted-line :alice :bob "hihi")
    (dotted-line 'bob 'alice "hoho"))
  {:type       :dotted/line
   :from       actor-from
   :to         actor-to
   :message    message
   :activate   activate
   :deactivate deactivate})

(defn dotted-arrow
  "Dotted line with arrowhead from an actor to another."
  [actor-from actor-to message & {:keys [activate deactivate]
                                  :or   {activate   false
                                         deactivate false}}]
  (use-like-this "all data can be a symbol, keyword, or a string."
    (dotted-arrow :alice :bob "hihi")
    (dotted-arrow 'bob 'alice "hoho"))
  {:type       :dotted/arrow
   :from       actor-from
   :to         actor-to
   :message    message
   :activate   activate
   :deactivate deactivate})

(defn dotted-cross
  "Dotted line with cross arrowhead from an actor to another."
  [actor-from actor-to message & {:keys [activate deactivate]
                                  :or   {activate   false
                                         deactivate false}}]
  (use-like-this "all data can be a symbol, keyword, or a string."
    (dotted-cross :alice :bob "hihi")
    (dotted-cross 'bob 'alice "hoho"))
  {:type       :dotted/cross
   :from       actor-from
   :to         actor-to
   :message    message
   :activate   activate
   :deactivate deactivate})

(defn dotted-open
  "Dotted line with open arrowhead from an actor to another."
  [actor-from actor-to message & {:keys [activate deactivate]
                                  :or   {activate   false
                                         deactivate false}}]
  (use-like-this "all data can be a symbol, keyword, or a string."
    (dotted-open :alice :bob "hihi")
    (dotted-open 'bob 'alice "hoho"))
  {:type       :dotted/open
   :from       actor-from
   :to         actor-to
   :message    message
   :activate   activate
   :deactivate deactivate})

;;; ============ Notes ============

(defn note-left
  "Place the note left-side of a given actor."
  [actor note]
  {:type  :note/left
   :actor actor
   :note  note})

(defn note-right
  "Place the note right-side of a given actor."
  [actor note]
  {:type  :note/right
   :actor actor
   :note  note})

(defn note-over
  "Place the note over possibly two actors."
  ([actor note]
   (note-over actor nil note))
  ([actor1 actor2 note]
   {:type   :note/over
    :actor1 actor1
    :actor2 actor2
    :note   note}))

;;; ============ Blocks ============

(defn loop-
  "Loop block with a label."
  [label & forms]
  {:type            :block/loop
   :label           label
   :following-forms forms})

(defn highlight
  "Highlight the background with a given color."
  [color & forms]
  {:type            :block/rect
   :label           (color->rgb color)
   :following-forms forms})

(defn alternative
  "Alternative block with conditions."
  [& condition-and-forms]
  (use-like-this
    (alternative
      ["x=1" [(solid-arrow :a :b "hoho")
              (dotted-arrow :b :a "hihi")]]
      ["x=2" [(solid-arrow :a :b "hoho")
              (dotted-arrow :b :a "hihi")]]
      ["x=3" [(solid-arrow :a :b "hoho")
              (dotted-arrow :b :a "hihi")]]))
  {:type            :block/alt
   :following-forms condition-and-forms})

(defn parallel
  "Parallel block with a description."
  [& condition-and-forms]
  {:type            :block/par
   :following-forms condition-and-forms})

(defn optional
  "Optional block with a description."
  [description & forms]
  {:type            :block/opt
   :label           description
   :following-forms forms})

;; ============ predicates ============

(defn label? [component]
  (= "label" (namespace (:type component))))

(defn arrow? [component]
  (or (= "solid" (namespace (:type component)))
      (= "dotted" (namespace (:type component)))))

(defn note? [component]
  (= "note" (namespace (:type component))))

(defn block? [component]
  (= "block" (namespace (:type component))))

;; ============ render ============

(declare dispatch-renderer)
(declare render-with-indent)

(defn render-label [indent-level label]
  (let [label-type (name (label :type))
        indent     (make-indent indent-level)]
    (cond (= "autonumber" label-type)   (str indent "autonumber")
          (= "participants" label-type) (str indent
                                             (string/join
                                               (interpose " "
                                                          (flatten
                                                            ["participant" (:actors label)]))))
          (= "activate" label-type)     (str indent "activate " (:actor label))
          (= "deactivate" label-type)   (str indent "deactivate " (:actor label)))))

(defn render-arrow [indent-level arrow]
  (let [from      (name (:from arrow))
        to        (name (:to arrow))
        message   (:message arrow)
        arrow-str (arrow->str arrow)
        indent    (make-indent indent-level)]
    (str indent from arrow-str to ": " message)))

(defn render-note [indent-level note]
  (let [note-type (name (note :type))
        indent    (make-indent indent-level)]
    (cond (= "left" note-type)
          (str indent (string/join
                        (interpose " " ["Note left of"
                                        (:actor note) ":"
                                        (:note note)])))
          (= "right" note-type)
          (str indent (string/join
                        (interpose " " ["Note right of"
                                        (:actor note) ":"
                                        (:note note)])))
          (= "over" note-type)
          (str indent (string/join
                        (interpose " " ["Note over"
                                        (:actor1 note)
                                        (when (:actor2 note)
                                          (str "," (:actor2 note))) ":"
                                        (:note note)]))))))

(defn render-simple-block [indent-level simple-block]
  (let [block-name (name (simple-block :type))
        indent     (make-indent indent-level)]
    (str indent
         (string/join
           (interpose " "
                      (flatten [block-name (:label simple-block) "\n    "
                                (mapv (render-with-indent (+ indent-level 4))
                                      (:following-forms simple-block))
                                ["\nend"]]))))))

(defn render-complex-block [indent-level complex-block block-name clause-name]
  (if (>= 0 alt-form-nums) (throw (IllegalArgumentException.))
      (letfn [(alt-first [indent-level complex-block]
                (let [alt-clause (first (:following-forms complex-block))]
                  (string/join
                    (flatten [block-name " " (first alt-clause) "\n"
                              (interpose "\n" (mapv (partial render-with-indent
                                                             (+ indent-level 4))
                                                    (second alt-clause)))]))))
              (alt-rest [indent-level complex-block]
                (loop [current-clauses (rest (:following-forms complex-block))
                       acc             ""]
                  (if (empty? current-clauses) acc
                      (let [current-clause       (first current-clauses)
                            condition            (first current-clause)
                            clause-form-rendered (string/join (interpose "\n"
                                                                         (mapv (partial render-with-indent
                                                                                        (+ indent-level 4))
                                                                               (second current-clause))))
                            clause-rendered      (str clause-name " " condition "\n" clause-form-rendered)]
                        (recur (rest current-clauses) (str acc clause-rendered "\n"))))))]
        (str (alt-first indent-level complex-block) "\n" (alt-rest indent-level complex-block)))))

(defn render-block [indent-level block]
  (let [block-type (name (block :type))]
    (cond (or (= "loop" block-type)
              (= "rect" block-type)
              (= "opt" block-type)) (render-simple-block indent-level block)
          (= "alt" block-type)      (render-complex-block indent-level block "alt" "else")
          (= "par" block-type)      (render-complex-block indent-level block "par" "and"))))


(defn render-with-indent [indent-level component]
  (trampoline (partial (dispatch-renderer component) indent-level) component))

(defn render [component]
  (render-with-indent component 0))

(defn sequence-diagram
  "Make a Sequence Diagram."
  [& forms])

(comment "========================================"
  )
