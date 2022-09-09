(ns mermaid-clj.sequence
  (:require [clojure.data.json :as json]
            [clojure.java.shell :as shell]
            [clj-http.client :as client]
            [clojure.string :as string])
  (:import java.util.Base64
           [java.io File]))


(defmacro use-like-this [& _])


;; ============ Lookup Tables ============

;; (def direction->mermaid
;;   {:call/sync        "->>"
;;    :call/activate    "->>+"
;;    :call/cross       "-x"
;;    :call/async       "-)"
;;    :reply/sync       "-->>"
;;    :reply/deactivate "-->>-"
;;    :reply/cross      "--x"
;;    :reply/async      "--)"})

(def color->rgb
  {:green  "rgb(0,255,0,0.1)"
   :blue   "rgb(0,0,255,0.1)"
   :red    "rgb(255,0,0,0.2)"
   :yellow "rgb(255,255,0,0.5)"
   :gray   "rgb(0,0,0,0.1)"})

;; ============ Helpers ============

(defn- multi-line
  "Breaks a long text into multiple lines,
  while reducing all whitespaces into a single space."
  [s length]
  (let [words (string/split s #"\s+")
        data  (reduce (fn [acc word]
                        (cond (empty? (acc :current))
                              (assoc acc :current word)
                              (>= (count (str (acc :current) " " word))
                                  length)
                              (-> acc
                                  (update :lines conj (acc :current))
                                  (assoc :current word))
                              :else
                              (update acc :current str " " word)))
                      {:lines   []
                       :current ""}
                      words)]
    (string/join "<br>" (conj (data :lines) (data :current)))))

;; ============ DSL APIs ============

(defn actor-name
  "Get the name of the actor by replacing the hyphens in an actor name."
  [actor]
  (str "\"" (string/replace (name actor) #"\-" " ")))


;; ============ blocks ============

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
   :note  (multi-line note 20)})


(defn note-right
  "Place the note right-side of a given actor."
  [actor-to-put-text-on-right note]
  {:type  :block/note-right
   :actor actor-to-put-text-on-right
   :note  (multi-line note 20)})


(defn note-over
  "Place the note over possibly two actors."
  ([actor1 note]
   (note-over actor1 nil note))
  ([actor1 actor2 note]
   {:type   :block/note-over
    :actor1 actor1
    :actor2 actor2
    :note   (multi-line note 60)}))


(defn background-highlight
  "Highlight the background for the following content."
  [color & forms]
  {:type            :block/highlight
   :color           (color->rgb color)
   :following-forms forms})


(defn alternative
  "Mark the following content as an Alternative block."
  [if-condition-description if-forms & others]
  (use-like-this
    (alternative
      ["x=1" [(call-sync :a :b "hoho")
              (reply-sync :b :a "hihi")]]
      ["x=2" [(call-activate :a :b "hoho")
              (reply-activate :b :a "hihi")]]
      ["x=3" [(call-async :a :b "hoho")
              (reply-async :b :a "hihi")]]))
  {:type            :block/alternative
   :condition       if-condition-description
   :following-forms if-forms
   :others          others})


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


;; ============ calls ============


(defn call-sync
  "Make a synchronous call from an actor to another."
  [actor-from actor-to message]
  (use-like-this "all data can be a symbol, keyword, or keyword."
    (call-sync :alice :bob "hihi")
    (call-sync 'bob 'alice "hoho"))
  {:type    :call/sync
   :from    actor-from
   :to      actor-to
   :message message})


(defn call-activate
  "Make a call from an actor to another and activate the receiver."
  [actor-from actor-to message]
  (use-like-this "all data can be a symbol, keyword, or keyword."
    (call-activate :alice :bob "hihi")
    (call-activate 'bob 'alice "hoho"))
  {:type    :call/activate
   :from    actor-from
   :to      actor-to
   :message message})


(defn call-cross
  "Make a call from an actor to another with a cross-shaped arrow."
  [actor-from actor-to message]
  (use-like-this "all data can be a symbol, keyword, or keyword."
    (call-cross :alice :bob "hihi")
    (call-cross 'bob 'alice "hoho"))
  {:type    :call/cross
   :from    actor-from
   :to      actor-to
   :message message})


(defn call-async
  "Make an asynchronous call from an actor to another with a cross-shaped arrow."
  [actor-from actor-to message]
  (use-like-this "all data can be a symbol, keyword, or keyword."
    (call-async :alice :bob "hihi")
    (call-async 'bob 'alice "hoho"))
  {:type    :call/async
   :from    actor-from
   :to      actor-to
   :message message})


;; ============ replys ============


(defn reply-sync
  "Make a synchronous reply from an actor to another."
  [actor-from actor-to message]
  (use-like-this "all data can be a symbol, keyword, or keyword."
    (call-async :alice :bob "hihi")
    (call-async 'bob 'alice "hoho"))

  {:type    :reply/sync
   :from    actor-from
   :to      actor-to
   :message message})


(defn reply-activate
  "Make a reply from an actor to another and terminate the activation."
  [actor-from actor-to message]
  (use-like-this "all data can be a symbol, keyword, or keyword.")
  {:type    :reply/activate
   :from    actor-from
   :to      actor-to
   :message message})


(defn reply-cross
  "Make a reply from an actor to another with a cross-shaped arrow."
  [actor-from actor-to message]
  (use-like-this "all data can be a symbol, keyword, or keyword.")
  {:type    :reply/async
   :from    actor-from
   :to      actor-to
   :message message
   :message message})


(defn reply-async
  "Make an asynchronous reply from an actor to another."
  [actor-from actor-to message]
  (use-like-this "all data can be a symbol, keyword, or keyword.")
  {:type    :call/async
   :from    actor-from
   :to      actor-to
   :message message})


;; ============ back and forth ============


(defn with-sync
  ([actor-from actor-to & forms]
   (with-sync actor-from actor-to "calls" "replies"))
  ([actor-from actor-to
    message-from message-to & forms]
   {:type            :with/sync
    :from            actor-from
    :to              actor-to
    :message-from    message-from
    :message-to      message-to
    :following-forms forms}))


(defn with-async
  ([actor-from actor-to & forms]
   (with-async actor-from actor-to "calls" "replies"))
  ([actor-from actor-to
    message-from message-to & forms]
   {:type            :with/async
    :from            actor-from
    :to              actor-to
    :message-from    message-from
    :message-to      message-to
    :following-forms forms}))


;; ============ Mermaid.js URL Templates ============

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
                                                (mapv assemble (:following-forms component))
                                                ])))
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
            ()))))


(defn encode-base64
  "Encode the given string into UTF-8."
  [string]
  (.encodeToString (Base64/getEncoder) (.getBytes string "UTF-8")))

(defn browser-view
  [diagram]
  (let [current-env (into {} (System/getenv))]
    (shell/sh "xdg-open"
              (format mermaid-view
                      (encode-base64
                        (json/write-str
                          {:code         diagram
                           :mermaid      {:theme "default"}
                           :updateEditor false})))
              :env (assoc current-env "BROWSER" "firefox"))))

(defn browser-edit
  [diagram]
  (let [current-env (into {} (System/getenv))]
    (shell/sh "xdg-open"
              (format mermaid-edit
                      (encode-base64
                        (json/write-str
                          {:code         diagram
                           :mermaid      {:theme "default"}
                           :updateEditor false})))
              :env (assoc current-env "BROWSER" "firefox"))))

(defn download-image
  ([diagram]
   (download-image diagram "/tmp/output.png"))
  ([diagram destination]
   (let [url (format mermaid-img
                     (encode-base64
                       (json/write-str
                         {:code    diagram
                          :mermaid {:theme "default"}})))]
     (clojure.java.io/copy
       (:body (client/get url {:as :stream}))
       (File. destination)))))
