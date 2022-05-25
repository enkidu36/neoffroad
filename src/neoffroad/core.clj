(ns neoffroad.core
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv])
  (:import  java.io.Console)
    (:gen-class))

(def rider-header "Registrant(s)")
(def team-header "Team Name")
(def chip-header "Chip #")
(def bib-header "Bib #")
(def transponder-header "Transponder #")
(def category-header "Category")
(def division-header "Division")
(def signature-header "Signature")
(def header-names [rider-header team-header category-header division-header])

(defn read-file [infile]
  (with-open [r (io/reader infile)]
    (vec (csv/read-csv r))))

(defn write-file [outfile data ]
  (with-open [w (io/writer outfile)]
    (doseq [line data]
      (.write w (str line "\n")))))

 (defn filter-cols [filter row]
   (map #(nth row %) filter))

(defn get-ndx [col-names [header]]
  (map #(.indexOf header %) col-names))

(defn remove-columns [header-names data]
  (let [indexes (get-ndx header-names data)]
    (map #(filter-cols indexes %) data)))

 (defn add-columns  [[hdr & rest]]
   (let [header (-> hdr
                    (conj transponder-header)
                    (conj chip-header)
                    (conj bib-header))
         body (map #(-> % (conj "--") (conj "--") (conj "")) rest)]
     (conj body header)))

(defn sort-by-name [name [hdr & rest]]
  ;; Sorts data header column name
  (let [col-ndx (.indexOf hdr name)
        body (sort #(compare (nth %1 col-ndx) (nth %2 col-ndx))  rest)]
    (conj body hdr)))

 (defn add-col [col-name [header & rest]]
  (let [body (map #(conj % "") (vec rest))]
    (conj body (conj (vec header) col-name))))

(defn remove-null [col-name [header & rest]]
  (let [c-ndx (.indexOf header col-name)
        body  (for [ctr (range (count rest))]
                (let [row (nth rest ctr)
                      value (nth row c-ndx)]
                      (assoc row c-ndx (if (= value "null") "" value))))]
    (conj body header)))

(defn merge-cat-div-columns [[header & rest]]
  (let [c-ndx (.indexOf header category-header)
        d-ndx (.indexOf header division-header)
        body (map #(assoc (vec %1) c-ndx (str (nth % c-ndx) " - " (nth % d-ndx))) rest)]
    (conj (map (fn [row] (filter #(not= (nth row d-ndx) %) row)) (vec body))
     (filter #(not= division-header %) header))))

(defn add-chip-data [chip-data rider-data]
 (let [chip-ndx (.indexOf (first rider-data) chip-header)
       transponder-ndx (.indexOf (first rider-data) transponder-header)
       name-ndx (.indexOf (first rider-data) rider-header)]
    (loop [acc [(vec (first rider-data))]
           recs (rest rider-data)
           prev-rec ""
           chip-ctr -1
           dup-ctr 0]
     (if (or (empty? recs))
       (do
         (prn (str  dup-ctr " riders signed up for two races"))
         acc)
       (let [prev-name (when (not (empty? prev-rec)) (nth prev-rec name-ndx))
             rec (vec (first recs))
             name (nth rec name-ndx)
             previous? (= name prev-name)
             dups (if previous? (+ 1 dup-ctr) dup-ctr)
             counter (if previous? chip-ctr (+ 1 chip-ctr))
             upd-rec (if previous?
                       (-> rec
                           (assoc chip-ndx (nth prev-rec chip-ndx))
                           (assoc transponder-ndx (nth prev-rec transponder-ndx)))
                       (-> rec
                           (assoc chip-ndx (+ 1 counter))
                           (assoc transponder-ndx (first (nth chip-data counter)))))]

         (recur (conj acc upd-rec) (rest recs) upd-rec counter dups)))))
 )

(defn -main [in-file out-file chip-file]
  (prn (str "Hello Nebraska Offroad Series"))
   (let [data (read-file in-file)
        chips (seq (read-file chip-file))]
    (->> data
         (remove-columns header-names)
         (add-columns)
         (sort-by-name rider-header)
         (merge-cat-div-columns)
         (add-chip-data chips)
         (remove-null team-header)
         (add-col signature-header)
         (map #(clojure.string/join "," %))
         (write-file out-file))))

(comment
  (def reg-file "./resources/2022_Lewis_Clark.csv")
  (def sys-file "./resources/2022_LC_Timing_Registration.csv")
  (def chip-file "./resources/arkfeld-chip-ids.csv")

  (-main reg-file sys-file chip-file)
 
  ;; paredit practice
  ;; https://calva.io/paredit/
  (let [s [1 2 3 4 5]]
    (->> s
         (map (partial * (apply + [1 2 3])))
         (repeat 4)
         (zipmap (range 4))))
  ()
  )


