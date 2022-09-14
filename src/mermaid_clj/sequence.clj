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
  [description forms & others]
  {:type            :block/par
   :label           description
   :following-forms forms
   :others          others})

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

(defn render-label [label indent-level]
  (let [label-type (name (label :type))
        indent     (make-indent indent-level)]
    (cond (= "autonumber" label-type)   (str indent "autonumber")
          (= "participants" label-type) (str indent
                                             (apply str (interpose " "
                                                                   (flatten
                                                                     ["participant" (:actors label)]))))
          (= "activate" label-type)     (str indent "activate " (:actor label))
          (= "deactivate" label-type)   (str indent "deactivate " (:actor label)))))

(defn render-arrow [arrow indent-level]
  (let [from      (name (:from arrow))
        to        (name (:from arrow))
        message   (:message arrow)
        arrow-str (arrow->str arrow)
        indent    (make-indent indent-level)])
  (str indent from arrow->str to ": " message))

(defn render-note [note indent-level]
  (let [note-type (name (note :type))
        indent    (make-indent indent-level)]
    (cond (= "left" note-type)
          (str indent (apply str (interpose " " ["Note left of"
                                                 (:actor note) ":"
                                                 (:note note)])))
          (= "right" note-type)
          (str indent (apply str (interpose " "
                                            ["Note right of"
                                             (:actor note) ":"
                                             (:note note)])))
          (= "over" note-type)
          (str indent (apply str (interpose " "
                                            ["Note over"
                                             (:actor1 note)
                                             (when (:actor2 note)
                                               (str "," (:actor2 note))) ":"
                                             (:note note)]))))))a

(defn render-simple-block [simple-block indent-level]
  ;; NOTE deals with loop, rect, alt, and opt
  (let [block-name (name (simple-block :type))
        indent     (make-indent indent-level)]
    (str indent
         (apply str
                (interpose " "
                           (flatten [block-name (:label simple-block) "\n    "
                                     (mapv render (:following-forms simple-block))
                                     ["\nend"]]))))))

(defn render-complex-block [complex-block indent-level]
  ;; NOTE deals with alternative, parallel
  )

(defn render-block [block indent-level]
  (let [block-type (name (block :type))]
    (cond (or (= "loop" block-type)
              (= "highlight" block-type)
              (= "optional" block-type)) (render-simple-block block indent-level)
          (or (= "alternative" block-type)
              (= "parallel" block-type)) (render-complex-block block indent-level))))


(defn render-with-indent [component indent]
  (trampoline (partial (dispatch-renderer component) indent) component))


(defn render [component]
  (render-with-indent component 0))

(defn sequence-diagram
  "Make a Sequence Diagram."
  [& forms])

(comment "========================================"
  (def alt (alternative
             ["x=1" [(solid-arrow :a :b "hoho")
                     (dotted-arrow :b :a "hihi")]]
             ["x=2" [(solid-arrow :a :b "hoho")
                     (dotted-arrow :b :a "hihi")]]
             ["x=3" [(solid-arrow :a :b "hoho")
                     (dotted-arrow :b :a "hihi")]]))

  (def alt-form-nums (count (:following-forms alt)))
  (def alt-forms (:following-forms alt))

  (cond (>= 0 alt-form-nums) (throw (IllegalArgumentException.))
        (= 1 alt-form-nums)  'todo1
        :else                'todo2)

  (apply str (interpose " " (flatten ["alt" (first (first alt-forms))
                                      "\n    "
                                      (interleave "\n    " (mapv render (second (first alt-forms))))]))))
