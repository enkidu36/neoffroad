(ns neoffroad.bikereg
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.string :as str])
  (:gen-class))

(def registrant "Registrant")
(def first-name "First Name")
(def last-name "Last Name")
(def category "Category Entered / Merchandise Ordered")
(def team "Team")
(def chip "Chip")
(def bib "Bib")
(def transponder "Transponder")
(def signature "Signature")
(def filter-headers [first-name last-name category team])
(def missing-list [26 28 35 57])

(defn read-file [infile]
  (with-open [r (io/reader infile)]
    (vec (csv/read-csv r))))

(defn write-file [outfile data]
  (with-open [w (io/writer outfile)]
    (doseq [line data]
      (.write w (str line "\n")))))

(defn add-system-columns  [[header & rest]]
  (let [hdr (-> header
                (conj transponder)
                (conj chip)
                (conj bib))
        body (map #(-> % (conj "--") (conj "--") (conj "--")) rest)]
    (conj body hdr)))

(defn sort-by-name [name [hdr & rest]]
  ;; Sorts data header column name
  (let [col-ndx (.indexOf hdr name)
        body (sort #(compare (nth %1 col-ndx) (nth %2 col-ndx))  rest)]
    (conj body hdr)))

(defn add-col [col-name col-value [header & rest]]
  (let [body (map #(conj % col-value) rest)]
    (conj body (conj header col-name))))

(defn remove-null [col-name [header & rest]]
  (let [c-ndx (.indexOf header col-name)
        body  (for [ctr (range (count rest))]
                (let [row (nth rest ctr)
                      value (nth row c-ndx)]
                  (assoc row c-ndx (if (= value "null") "" value))))]
    (conj body header)))




(defn merge-name-columns [to-col [header & rest]]
  (let [last-ndx (.indexOf header last-name)
        first-ndx (.indexOf header first-name)
        merge-ndx (.indexOf header to-col)
        body (map #(assoc (vec %1) merge-ndx (str (nth % first-ndx) " " (nth % last-ndx))) rest)]
    (conj (map (fn [row] (filter #(not= (nth row first-ndx) %) row)) (vec body))
          (filter #(not= first-name %) header))))

(defn same? [val1 val2]
  (= (->> val1 (str/trim) (str/lower-case))
     (->> val2 (str/trim) (str/lower-case))))

(defn add-chip-data [chip-data [header & body]]
  (let [chip-col (.indexOf header chip)
        transponder-col (.indexOf header transponder)
        name-col (.indexOf header registrant)]
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

(defn filter-by-header-names [data header-names]
  (prn header-names)
  (let [indexes (map #(.indexOf (first data) %) header-names)]
    (map (fn [row]
           (map #(nth row %) indexes)) data)))

(defn remove-header-values [data value-set]
  (let [filter-header (remove #(contains? value-set %) (first data))]
    (prn "test" filter-header)
    (filter-by-header-names data filter-header)))

(defn rename-col [col-name new-name [header & rest]]
  (let [c-ndx (.indexOf header col-name)
        new-header (assoc (vec header) c-ndx new-name)]
    (conj rest new-header)))

(defn -main [in-file out-file chip-file]
  (prn (str "Bike Reg file process"))
  (let [data (read-file in-file)
        chips (filter-chips (seq (read-file chip-file)))]
    (as-> data $
      (filter-by-header-names $ filter-headers)
      (add-col registrant "" $)
      (merge-name-columns registrant $)
      (remove-header-values $ #{first-name last-name})
      (sort-by-name registrant $)
      (add-system-columns $)
      (add-chip-data chips $)
      (remove-null team $)
      (rename-col category "Category" $)
      (add-col signature "*" $)
      (map #(str/join "," %) $)
      (write-file out-file $))))

(comment
  (def infile "./resources/test.csv")
  (def outfile "./resources/test-out")
  (def chipfile "./resources/arkfeld-chip-ids.csv")
  (def chips (filter-chips (seq (read-file chipfile))))
  (-main infile outfile chipfile)

  (def t-data (read-file infile))
  (def headers (first t-data))

  (defn process [data chips]
    (as-> data $
      (rename-col category "Category" $))
        ;;  (write-file outfile $)
    )
  (prn (process t-data chips)))




