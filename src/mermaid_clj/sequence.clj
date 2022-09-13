(ns mermaid-clj.sequence
  (:require [clojure.data.json :as json]
            [clojure.java.shell :as shell]
            [clj-http.client :as client]
            [clojure.string :as string]))

(defmacro use-like-this [& _])

;; ============ Lookup Tables ============

(defn color->rgb [color]
  (cond [(= (name color) :green)  "rgb(0,255,0,0.1)"
         (= (name color) :blue)   "rgb(0,0,255,0.1)"
         (= (name color) :red)    "rgb(255,0,0,0.2)"
         (= (name color) :yellow) "rgb(255,255,0,0.5)"
         (= (name color) :gray)   "rgb(0,0,0,0.1)"
         :else                    "rgb(0,0,0,0)"])) ; fall back to black


;; ============ DSL APIs ============

(defn actor-name
  "Get the name of the actor by replacing the hyphens in an actor name."
  [actor]
  (str "\"" (string/replace (name actor) #"\-" " ")))

;; ============ labels ============

(defn autonumber
  "Add an Autonumber label."
  []
  {:type :label/autonumber})

(defn participants
  "Add the following actors as participants."
  [& participating-actors]
  {:type   :participants
   :actors :label/participating-actors})

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
  {:type    :solid/arrow
   :from    actor-from
   :to      actor-to
   :message message
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
  {:type    :solid/cross
   :from    actor-from
   :to      actor-to
   :message message
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
  {:type    :solid/open
   :from    actor-from
   :to      actor-to
   :message message
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
  {:type    :dotted/line
   :from    actor-from
   :to      actor-to
   :message message
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
  {:type    :dotted/arrow
   :from    actor-from
   :to      actor-to
   :message message
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
  {:type    :dotted/cross
   :from    actor-from
   :to      actor-to
   :message message
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
  {:type    :dotted/open
   :from    actor-from
   :to      actor-to
   :message message
   :activate   activate
   :deactivate deactivate})

;;; ============ back and forth ============

(defn with-sync
  ([actor-from actor-to & forms]
   (with-sync actor-from actor-to "calls" "replies") forms)
  ([actor-from actor-to
    message-from message-to & forms]
   (use-like-this "all data can be a symbol, keyword, or a string."
     (with-sync :alice :bob "hihi" "hoho"
       (call-sync :bob :alice "hehe")))
   {:type            :with/sync
    :from            actor-from
    :to              actor-to
    :message-from    message-from
    :message-to      message-to
    :following-forms forms}))

(defn with-async
  ([actor-from actor-to & forms]
   (with-async actor-from actor-to "calls" "replies") forms)
  ([actor-from actor-to
    message-from message-to & forms]
   {:type            :with/async
    :from            actor-from
    :to              actor-to
    :message-from    message-from
    :message-to      message-to
    :following-forms forms}))

;;; ============ Blocks ============

(defn loop
  "Mark the following content as a loop block."
  [label & forms]
  {:type            :block/loop
   :label           label
   :following-forms forms})

(defn note-left
  "Place the note left-side of a given actor."
  [actor-to-put-text-on-left note]
  {:type  :block/note-left
   :actor actor-to-put-text-on-left
   :note  note})

(defn note-right
  "Place the note right-side of a given actor."
  [actor-to-put-text-on-right note]
  {:type  :block/note-right
   :actor actor-to-put-text-on-right
   :note  note})

(defn note-over
  "Place the note over possibly two actors."
  ([actor note]
   (note-over actor nil note))
  ([actor1 actor2 note]
   {:type   :block/note-over
    :actor1 actor1
    :actor2 actor2
    :note   note}))

(defn background-highlight
  "Highlight the background for the following content."
  [color & forms]
  {:type            :block/highlight
   :color           (color->rgb color)
   :following-forms forms})

(defn alternative
  "Mark the following content as an Alternative block."
  [& condition-description-and-forms]
  (use-like-this
    (alternative
      ["x=1" [(call-sync :a :b "hoho")
              (reply-sync :b :a "hihi")]]
      ["x=2" [(call-activate :a :b "hoho")
              (reply-activate :b :a "hihi")]]
      ["x=3" [(call-async :a :b "hoho")
              (reply-async :b :a "hihi")]]))
  {:type            :block/alternative
   :following-forms condition-description-and-forms})

(defn parallel
  "Mark the following content as an Parallel block."
  [description forms & others]
  {:type            :block/parallel
   :description     description
   :following-forms forms
   :others          others})

(defn optional
  "Mark the following content as an Optional block."
  [description & forms]
  {:type            :block/optional
   :description     description
   :following-forms forms})

;;; ============ Mermaid.js URL Templates ============

(def mermaid-edit "https://mermaid-js.github.io/mermaid-live-editor/#/edit/%s")

(def mermaid-view "https://mermaid-js.github.io/mermaid-live-editor/#/view/%s")

(def mermaid-img "https://mermaid.ink/img/%s")

;; ============ User Utility Functions ============

(defn ^:string assemble [component]
  (let [component-type    (namespace (component :type))
        component-subtype (name (component :type))]
    (cond (= "label" component-type)
          (cond (= "autonumber" component-subtype)
                "autonumber"
                (= "participants" component-subtype)
                (apply str (interpose " "
                                      (flatten ["\n" "participant"
                                                (mapv actor-name (:actor component))]))))
          (= "block" component-type)
          (cond (= "loop" component-subtype)
                (apply str (interpose " "
                                      (flatten ["loop" (:label component) "\n"
                                                (mapv assemble (:following-forms component))
                                                ["\nend"]])))
                (= "note-left" component-subtype)
                (apply str (interpose " "
                                      ["Note left of"
                                       (:actor component) ":"
                                       (:note component)]))
                (= "note-right" component-subtype)
                (apply str (interpose " "
                                      ["Note right of"
                                       (:actor component) ":"
                                       (:note component)]))
                (= "note-over" component-subtype)
                (apply str (interpose " "
                                      ["Note over"
                                       (:actor1 component)
                                       (when (:actor2 component)
                                         "," (:actor2 component)) ":"
                                       (:note component)]))
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
                ())
          (= "call" component-type)
          (cond (= "sync" component-subtype)
                ()
                (= "activate" component-subtype)
                ()
                (= "cross" component-subtype)
                ()
                (= "async" component-subtype)
                ())
          (= "reply" component-type)
          (cond
            (= "sync" component-subtype)
            ()
            (= "activate" component-subtype)
            ()
            (= "cross" component-subtype)
            ()
            (= "async" component-subtype)
            ())
          (= "with" component-type)
          (cond
            (= "sync" component-subtype)
            ()
            (= "async" component-subtype)
            ()))))
