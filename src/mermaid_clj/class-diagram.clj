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

;; ================ classes ================

(defn class-builder [class-kind]
  (fn [name & forms]
    {:type  :class
     :kind  class-kind
     :id    name
     :forms forms}))

(def class (class-builder :class/public))
(def interface (class-builder :class/interface))
(def public-class (class-builder :class/public))
(def private-class (class-builder :class/private))
(def protected-class (class-builder :class/protected))
(def abstract-class (class-builder :class/abstract))
(def service-class (class-builder :class/service))
(def enum (class-builder :class/enum))

(def attributes [& forms]
  {:type :attributes
   :forms forms})

(def methods [& forms]
  {:type :methods
   :forms forms})

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

(def public    (member-builder :member/public))
(def private   (member-builder :member/private))
(def protected (member-builder :member/protected))
(def internal  (member-builder :member/internal))
(def abstract  (member-builder :member/abstract))
(def static    (member-builder :member/static))

(defn extends [subclass superclass
               & {:keys [head direction relationship]
                  :or   {head         ""
                         direction    :RL ; :LR :RL :TW
                         relationship :inherit}}])

(defn class-diagram [])

(comment
  :0..n
  :Square<Shape>)
