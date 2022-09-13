(ns mermaid-clj.sequence
  (:require [clojure.data.json :as json]
            [clojure.java.shell :as shell]
            [clj-http.client :as client]
            [clojure.string :as string]))

(defmacro use-like-this [& _])

(defn color->rgb [color]
  (cond (= (name color) :green)  "rgb(0,255,0,0.1)"
        (= (name color) :blue)   "rgb(0,0,255,0.1)"
        (= (name color) :red)    "rgb(255,0,0,0.2)"
        (= (name color) :yellow) "rgb(255,255,0,0.5)"
        (= (name color) :gray)   "rgb(0,0,0,0.1)"
        :else                    "rgb(0,0,0,0)")) ; fall back to black



;; ============ DSL APIs ============

(defn actor-name
  "Get the name of the actor by replacing the hyphens in an actor name."
  [actor]
  (str "\"" (string/replace (name actor) #"\-" " ")))

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

;; ============ User Utility Functions ============

(defn assemble [component]
  (let [component-type    (namespace (component :type))
        component-subtype (name (component :type))]
    (cond (= "label" component-type)
          (cond (= "autonumber" component-subtype)
                "autonumber"
                (= "participants" component-subtype)
                (apply str (interpose " " (flatten ["participant" (:actors component)])))
                (= "activate" component-subtype)
                (str "activate " (:actor component))
                (= "deactivate" component-subtype)
                (str "deactivate " (:actor component)))
          (= "solid" component-type)
          (cond (= "line" component-subtype)
                ()
                (= "arrow" component-subtype)
                ()
                (= "cross" component-subtype)
                ()
                (= "open" component-subtype)
                ())
          (= "dotted" component-type)
          (cond (= "line" component-subtype)
                ()
                (= "arrow" component-subtype)
                ()
                (= "cross" component-subtype)
                ()
                (= "open" component-subtype)
                ())
          (= "note" component-type)
          (cond (= "left" component-subtype)
                (apply str (interpose " " ["Note left of"
                                           (:actor component) ":"
                                           (:note component)]))
                (= "right" component-subtype)
                (apply str (interpose " "
                                      ["Note right of"
                                       (:actor component) ":"
                                       (:note component)]))
                (= "over" component-subtype)
                (apply str (interpose " "
                                      ["Note over"
                                       (:actor1 component)
                                       (when (:actor2 component)
                                         "," (:actor2 component)) ":"
                                       (:note component)])))
          (= "block" component-type)
          (cond (= "loop" component-subtype)
                (apply str (interpose " " (flatten ["loop" (:label component) "\n"
                                                    (mapv assemble (:following-forms component))
                                                    ["\nend"]])))
                (= "highlight" component-subtype)
                (apply str (interpose " "
                                      (flatten ["rect" (:color component) "\n"
                                                (mapv assemble (:following-forms component))
                                                "\nend"])))
                (= "alternative" component-subtype)
                (apply str (interpose " "
                                      (flatten ["alt" (:condition component) "\n"
                                                (mapv assemble (:following-forms component))])))
                (= "parallel" component-subtype)
                ()
                (= "optional" component-subtype)
                ()))))
