(ns neoffroad.core
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.string :as str])
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
(def missing-list [26 28 35 42 57])

(defn read-file [infile]
  (with-open [r (io/reader infile)]
    (vec (csv/read-csv r))))

(defn write-file [outfile data]
  (with-open [w (io/writer outfile)]
    (doseq [line data]
      (.write w (str line "\n")))))

(defn add-columns  [[header & rest]]
  (let [hdr (-> header
                (conj transponder-header)
                (conj chip-header)
                (conj bib-header))
        body (map #(-> % (conj "--") (conj "--") (conj "")) rest)]
    (conj body hdr)))

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

(defn same? [val1 val2]
  (= (->> val1 (str/trim) (str/lower-case))
     (->> val2 (str/trim) (str/lower-case))))

(defn add-chip-data [chip-data [header & body]]
  (let [chip-col (.indexOf header chip-header)
        transponder-col (.indexOf header transponder-header)
        name-col (.indexOf header rider-header)]
    (loop [acc [(vec header)]
           recs body
           prev-rec ""
           chip-ctr -1
           dup-ctr 0]
      (if  (empty? recs)
        (do
          (prn (str  dup-ctr " riders signed up for two races"))
          acc)
        (let [prev-name (if (seq prev-rec) (nth prev-rec name-col) "")
              rec (vec (first recs))
              name (nth rec name-col)
              previous? (same? name prev-name)
              dups (if previous? (+ 1 dup-ctr) dup-ctr)
              counter (if previous? chip-ctr (+ 1 chip-ctr))
              upd-rec (if previous?
                        (-> rec
                            (assoc chip-col (nth prev-rec chip-col))
                            (assoc transponder-col (nth prev-rec transponder-col)))
                        (-> rec
                            (assoc chip-col (first (nth chip-data counter)))
                            (assoc transponder-col (second (nth chip-data counter)))))]

          (recur (conj acc upd-rec) (rest recs) upd-rec counter dups))))))

(defn assign-chip-ids [chips]
  (let [ids (range 1 (+ 1 (count chips)))]
    (map #(conj (vector %1) (first %2)) ids chips)))

(defn filter-chips [chips]
  (let [list (assign-chip-ids chips)]
    (filterv (fn [x]
               (not-any? #(= (first x) %) missing-list)) list)))

(defn filter-by-header-names [header-names data]
  (let [indexes (map #(.indexOf (first data) %) header-names)]
    (map (fn [row]
           (map #(nth row %) indexes)) data)))

(defn -main [in-file out-file chip-file]
  (prn (str "Hello Nebraska Offroad Series"))
  (let [data (read-file in-file)
        chips (filter-chips (seq (read-file chip-file)))]
    (->> data
         (filter-by-header-names header-names)
         (add-columns)
         (sort-by-name rider-header)
         (merge-cat-div-columns)
         (add-chip-data chips)
         (remove-null team-header)
         (add-col signature-header)
         (map #(str/join "," %))
         (write-file out-file))))

(comment
  (def reg-file "./resources/2022_calvin-crest.csv")
  (def sys-file "./resources/2022_calvin-crest-master.csv")
  (def chip-file "./resources/arkfeld-chip-ids.csv")

  (-main reg-file sys-file chip-file))


