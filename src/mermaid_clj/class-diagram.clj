(ns mermaid-clj.class-diagram
  (:require [clojure.string :as string]))

(defn- type-normalize [type]
  (string/replace (name :List<Integer>) #"(<|>)" "~" ))

;; ================ classes ================

(defn- class-builder [visibility]
  (fn [classname & forms]
    {:type  :class
     :kind  (classname visibility)
     :id    (name classname)
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

(defn- member-builder [visibility]
  (fn
    ([method-name]
     {:type    :member
      :kind    (name visibility)
      :rtntype :void
      :id      (name method-name)
      :args    []})
    ([method-name args]
     {:type    :member
      :kind    (name visibility)
      :rtntype :void
      :id      (name method-name)
      :args    args})
    ([rtntype method-name args]
     {:type    :member
      :kind    (name visibility)
      :rtntype (type-normalize rtntype)
      :id      (name method-name)
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
  {:type         :extends
   :subclass     (name subclass)
   :superclass   (name superclass)
   :head         (name head)
   :direction    (name direction)
   :relationship (name relationship)
   :cardinality  (name cardinality)})

;; ================ render ================

(declare render-with-indent)
(declare dispatch-renderer)

(defn- make-indent [indent]
  (string/join (for [_ (range indent)] " ")))

(defn- render-class [indent-level class]
  (let [{:keys
         [kind id forms]} class
        indent            (make-indent indent-level)])
  ;; TODO
  )

(defn- render-member [indent-level member]
  (let [{:keys
         [kind rtntype
          id args]} member
        indent      (make-indent indent-level)])
  ;; TODO
  )

(defn- render-extends [indent-level extends]
  (let [{:keys
         [kind subclass superclass
          head direction relationship
          cardinality]} extends
        indent          (make-indent indent-level)])
  ;; TODO
  )

(defn- dispatch-renderer [form]
  (condp = (name (form :type))
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

(comment
  (render (public-class :Animal))
  (name (public-class :Animal) :type))
