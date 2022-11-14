(ns mermaid-clj.class-diagram
  (:require [clojure.string :as string]))

(defn type-normalize [type]
  (string/replace (name :List<Integer>) #"(<|>)" "~" ))

;; ================ classes ================

(defn class-builder [visibility]
  (fn [name & forms]
    {:type  :class
     :kind  visibility
     :id    name
     :forms forms}))

(def class           (class-builder :public)) ; class defaults to public
(def public-class    (class-builder :public))
(def private-class   (class-builder :private))
(def protected-class (class-builder :protected))
(def abstract-class  (class-builder :abstract))
(def service-class   (class-builder :service))
(def interface       (class-builder :interface))
(def enum            (class-builder :enum))

(defn attributes [& forms]
  {:type  :attributes
   :forms forms})

(defn methods [& forms]
  {:type  :methods
   :forms forms})

;; ================ members ================

(defn member-builder [visibility]
  (fn
    ([method-name]
     {:type    :member
      :kind    visibility
      :rtntype :void
      :id      method-name
      :args    []})
    ([method-name args]
     {:type    :member
      :kind    visibility
      :rtntype :void
      :id      method-name
      :args    args})
    ([rtntype method-name args]
     {:type    :member
      :kind    visibility
      :rtntype (type-normalize rtntype)
      :id      method-name
      :args    args})))

(def public    (member-builder :public))
(def private   (member-builder :private))
(def protected (member-builder :protected))
(def internal  (member-builder :internal))
(def abstract  (member-builder :abstract))
(def static    (member-builder :static))

;; ================ relationship ================

(defn extends [subclass superclass
               & {:keys [head direction relationship cardinality]
                  :or   {head         ""
                         direction    :RL ; :LR :RL :TW
                         relationship :inheritance
                         cardinality  :1}}]
  ;; available directions:
  ;;     :LR :RL :TW
  ;; available cardinalities:
  ;;     :1 :0..1 :1..* :*
  ;;     and :n :0..n :1..n where n>1 integer
  {:type         extends
   :subclass     subclass
   :superclass   superclass
   :head         head
   :direction    direction
   :relationship relationship
   :cardinality  cardinality})

;; ================ render ================

(defn render-class [class])

(defn render-member [member])

(defn render-extends [extends])

(defn dispatch-renderer [form]
  (condp = (namespace (form :type))
    "class"   render-class
    "member"  render-member
    "extends" render-extends))

(defn- render-with-indent [indent-level form]
  (trampoline (partial (dispatch-renderer form) indent-level) form))

(defn- render [form]
  (render-with-indent 4 form))

;; ================ main ================

(defn class-diagram [& forms]
  (str "classDiagram" "\n"
       (string/join (interpose "\n" (map render forms)))))

(comment
  :0..n
  :Square<Shape>)

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
