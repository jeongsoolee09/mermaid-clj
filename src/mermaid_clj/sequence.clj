(ns mermaid-clj.sequence
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as string]))

(defmacro use-like-this [& _])

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

(defn loop
  "Loop block with a label."
  [label & forms]
  {:type            :block/loop
   :label           label
   :following-forms forms})

(defn highlight
  "Highlight the background with a given color."
  [color & forms]
  {:type            :block/highlight
   :color           (color->rgb color)
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
  {:type            :block/alternative
   :following-forms condition-and-forms})

(defn parallel
  "Parallel block with a description."
  [description forms & others]
  {:type            :block/parallel
   :description     description
   :following-forms forms
   :others          others})

(defn optional
  "Optional block with a description."
  [description & forms]
  {:type            :block/optional
   :description     description
   :following-forms forms})

(defn sequence-diagram
  "Make a Sequence Diagram."
  [& forms]
  {:type            :sequence-diagramam
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

;; ============ renderers ============

(defn render-label [label]
  (let [label-type (name (label :type))]
    (cond (= "autonumber" label-type)
          "autonumber"
          (= "participants" label-type)
          (apply str (interpose " " (flatten ["participant" (:actors label)])))
          (= "activate" label-type)
          (str "activate " (:actor label))
          (= "deactivate" label-type)
          (str "deactivate " (:actor label)))))

(defn render-arrow [arrow])

(defn render-note [note]
  (let [note-type (name (note :type))]
    (cond (= "left" note-type)
          (apply str (interpose " " ["Note left of"
                                     (:actor note) ":"
                                     (:note note)]))
          (= "right" note-type)
          (apply str (interpose " "
                                ["Note right of"
                                 (:actor note) ":"
                                 (:note note)]))
          (= "over" note-type)
          (apply str (interpose " "
                                ["Note over"
                                 (:actor1 note)
                                 (when (:actor2 note)
                                   "," (:actor2 note)) ":"
                                 (:note note)])))))

(defn render-block [block]
  ;;; TODO
  (let [block-type (name (block :type))]
    (cond (= "loop" block-type)
          (apply str (interpose " " (flatten ["loop" (:label block) "\n"
                                              (mapv assemble (:following-forms block))
                                              ["\nend"]])))
          (= "highlight" block-type)
          (apply str (interpose " " (flatten ["rect" (:color block) "\n"
                                              (mapv assemble (:following-forms block))
                                              "\nend"])))
          (= "alternative" block-type)
          (apply str (interpose " " (flatten ["alt" (:condition block) "\n"
                                              (mapv assemble (:following-forms block))
                                              "\nend"])))
          (= "parallel" block-type)
          (apply str (interpose " " (flatten ["par" (:condition block) "\n"
                                              (mapv assemble (:following-forms block))
                                              "\nend"])))
          (= "optional" block-type)
          (apply str (interpose " " (flatten ["opt" (:condition block) "\n"
                                              (mapv assemble (:following-forms block))
                                              "\nend"])))
          (= "sequence-diagram" block-type)
          (apply str (interpose " " (flatten ["opt" (:condition block) "\n"
                                              (mapv assemble (:following-forms block))
                                              "\nend"]))))))

(defn dispatch-renderer [component]
  (cond (label? component) render-label
        (arrow? component) render-arrow
        (note? component)  render-note
        (block? component) render-block))

(defn render [component]
  ((dispatch-renderer component) component))
