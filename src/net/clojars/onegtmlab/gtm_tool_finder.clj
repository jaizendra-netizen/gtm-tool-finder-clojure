(ns net.clojars.onegtmlab.gtm-tool-finder
  "Search the OneGTMLab GTM tool directory (https://www.onegtmlab.dev/tool-directory):
  API reviews of sales and marketing tools, head-to-head comparisons and GTM glossary terms.
  The list ships inside the jar, so it works offline and sends no data anywhere."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def kinds ["Tool" "Category" "Comparison" "Glossary"])

(def entries
  "Every entry as a map with :name :kind :url :description."
  (delay (with-open [r (java.io.PushbackReader. (io/reader (io/resource "net/clojars/onegtmlab/gtm_tool_finder_data.edn")))]
           (edn/read r))))

(defn search
  "Entries whose name or description contains every word of query.
  Options: :kind (e.g. \"Tool\") and :limit (default 20)."
  [query & {:keys [kind limit] :or {limit 20}}]
  (let [words (str/split (str/lower-case (str query)) #"\s+")
        words (remove str/blank? words)
        text  (fn [e] (str/lower-case (str (:name e) " " (:description e))))
        order (fn [e] [(if (and (seq words) (str/starts-with? (str/lower-case (:name e)) (first words))) 0 1)
                       (or (first (keep-indexed #(when (= %2 (:kind e)) %1) kinds)) 9)])]
    (->> @entries
         (filter #(and (or (nil? kind) (.equalsIgnoreCase ^String kind ^String (:kind %)))
                       (every? (fn [w] (str/includes? (text %) w)) words)))
         (sort-by order)
         (take limit))))

(defn -main [& args]
  (let [[kind args] (if (= "--kind" (first args)) [(second args) (drop 2 args)] [nil args])
        results (search (str/join " " args) :kind kind)]
    (if (empty? results)
      (do (binding [*out* *err*] (println "No matches. Browse the full directory at https://www.onegtmlab.dev/tool-directory"))
          (System/exit 1))
      (doseq [r results] (println (format "%-10s %s\n           %s" (:kind r) (:name r) (:url r)))))))
