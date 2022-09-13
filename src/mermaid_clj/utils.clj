(ns mermaid-clj.utils
  (:import java.util.Base64
           [java.io File]))


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

;;; ============ Mermaid.js URL Templates ============

(def mermaid-edit "https://mermaid-js.github.io/mermaid-live-editor/#/edit/%s")

(def mermaid-view "https://mermaid-js.github.io/mermaid-live-editor/#/view/%s")

(def mermaid-img "https://mermaid.ink/img/%s")
