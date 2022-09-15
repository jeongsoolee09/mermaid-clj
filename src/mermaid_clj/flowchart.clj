(ns mermaid-clj.flowchart)

;; ============ nodes ============

(defn node [])

(defn round-edge [])

(defn pill [])

;; ============ links ============

(defn normal-line [from to & {:keys [length]
                              :or {length 1}}]
  )

(defn normal-arrow [from to & {:keys [length]
                               :or   {length 1}}])

(defn thick-line [from to & {:keys [length]
                             :or   {length 1}}])

(defn thick-arrow [from to & {:keys [length]
                              :or   {length 1}}])

(defn dotted-line [from to & {:keys [length]
                              :or   {length 1}}])

(defn dotted-arrow [from to & {:keys [length]
                               :or   {length 1}}])

;; ============ normal ============

;; flowchart TD
;;     A[Start] --> B{Is it?}
;;     B -->|Yes| C[OK]
;;     C --> D[Rethink]
;;     D --> B
;;     B ---->|No| E[End]

;; declaring shapes and adding messages are coupled together...
;; => using a **clojure let block** would solve the problem elegantly.
;; => what's the point of using an embedded DSL when you can't use the host language
;;    to write the desired software?

(flow-chart :TD
   (let ))
