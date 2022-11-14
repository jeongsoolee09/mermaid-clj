(ns mermaid-clj.class-diagram
  (:require [clojure.string :as string]))


(comment
  (class-diagram
    (extends :Duck :Animal)
    (extends :Fish :Animal)
    (extends :Zebra :Animal)
    (class :Animal
           (attributes
             (public :String 'beakColor)) ; type should be keyword
           (methods
             (public :void 'swim [])
             (public :void 'quack [])))
    (class :Fish
           (attributes
             (private :int 'sizeInFeet))
           (methods
             (protected :bool 'canEat)))
    (class :Zebra
           (attributes
             (public :bool 'isWild))
           (methods
             (private 'run)))))


(defn class [name & forms])

(defn interface [name & forms])

(defn abstract-class [name & forms])

(defn service-class [name & forms])

(defn public-class [name & forms])

(defn private-class [name & forms])

(defn protected-class [name & forms])

(defn enum [name & members])

(defn attributes [& forms])

(defn methods [& forms])

;; ================ members ================

(defn member-builder [visibility]
  (fn
    ([method-name]
     {:type    visibility
      :rtntype :void
      :id      method-name
      :args    []})
    ([method-name args]
     {:type    visibility
      :rtntype :void
      :id      method-name
      :args    args})
    ([rtntype method-name args]
     {:type    visibility
      :rtntype rtntype
      :id      method-name
      :args    args})))

(def public (member-builder :member/public))
(def private (member-builder :member/private))
(def protected (member-builder :member/protected))
(def internal (member-builder :member/internal))
(def abstract (member-builder :member/abstract))
(def static (member-builder :member/static))

(defn extends [subclass superclass & {:keys [head direction relationship]
                                      :or   {head         ""
                                             direction    :RL ; :LR :RL :TW
                                             relationship :inherit}}]
  
  )

(defn class-diagram [])

(comment
  :0..n
  :Square<Shape>)
