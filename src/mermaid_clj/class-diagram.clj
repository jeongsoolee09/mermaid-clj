(ns mermaid-clj.class-diagram
  (:require [clojure.string :as string]))


(comment
  (class-diagram
    (extends :Duck :Animal)
    (extends :Fish :Animal)
    (extends :Zebra :Animal)
    (class :Animal
           (attributes
             (public :String 'beakColor))
           (methods
             (public :void 'swim [])
             (public :void 'quack [])))
    (class- :Fish
            (attributes
              (private :int 'sizeInFeet))
            (methods
              (protected :bool 'canEat)))
    (class- :Zebra
            (attributes
              (public :bool 'isWild))
            (methods
              (private 'run)))))

;; type should be keyword

(defn class [])

(defn interface [])

(defn abstract-class [])

(defn service-class [])

(defn enum [])

(defn atrributes [])

(defn methods- [])

(defn public [])

(defn private [])

(defn protected [])

(defn internal [])

(defn abstract [])

(defn static [])

(defn extends [subclass superclass & {:keys [head direction relationship]
                                      :or {head ""
                                           direction :RL
                                           relationship :inherit}}]
  ;; :LR :RL :TW
  )

(defn class-diagram [])

(comment
  :0..n
  :Square<Shape>)
