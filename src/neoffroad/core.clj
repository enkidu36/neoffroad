(ns neoffroad.core
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv])
  (:import  java.io.Console)
    (:gen-class))

(def rider-header-name "Registrant(s)")
(def chip-header-name "Chip #")
(def bib-header-name "Bib #")
(def transponder-header-name "Transponder #")
(def category-header-name "Category")
(def division-header-name "Division")
(def filter-by ["Registrant(s)" "Team Name" "Category" "Division"])

(defn read-file [infile]
  (with-open [r (io/reader infile)]
    (vec (csv/read-csv r))))

(defn write-file [outfile data ]
  (with-open [w (io/writer outfile)]
    (doseq [line data]
      (.write w (str line "\n")))))

 (defn add-rows  [[hdr & rest]]
   (let [header (-> hdr
                    (conj transponder-header-name)
                    (conj chip-header-name)
                    (conj bib-header-name))
         body (map #(-> % (conj "--") (conj "--") (conj "")) rest)]
     (conj body header)))

(defn comp-names [r1 r2 col-ndx]
  (compare (nth r1 col-ndx) (nth r2 col-ndx)))

(defn sort-col [col-name [hdr & rest]]
  (let [col-ndx (.indexOf hdr col-name)
        body (sort #(comp-names %1 %2 col-ndx)  rest)]
    (conj body hdr)))

(defn get-col-indexes [col-names [header]]
  (map #(.indexOf header %) col-names))

 (defn filter-cols [filter row]
   (map #(nth row %) filter))
 
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

(defn merge-cat-div [[header & rest]]
  (let [c-ndx (.indexOf header category-header-name)
        d-ndx (.indexOf header division-header-name)
        body (map #(assoc (vec %1) c-ndx (str (nth % c-ndx) " - " (nth % d-ndx))) rest)]
    (conj (map (fn [row] (filter #(not= (nth row d-ndx) %) row)) (vec body))
     (filter #(not= division-header-name %) header))))

(defn add-chips [chip-data rider-data]
 (let [chip-ndx (.indexOf (first rider-data) chip-header-name)
       transponder-ndx (.indexOf (first rider-data) transponder-header-name)
       name-ndx (.indexOf (first rider-data) rider-header-name)]
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
             chip (nth rec chip-ndx)
             transponder (nth rec transponder-ndx)
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
        chips (seq (read-file chip-file))
        indexes (get-col-indexes filter-by data)]
    (->> data
         (map #(filter-cols indexes %))
         (add-rows)
         (sort-col "Registrant(s)")
         (merge-cat-div)
         (add-chips chips)
         (remove-null "Team Name")
         (add-col "Signature")
         (map #(clojure.string/join "," %))
         (write-file out-file))))


(comment
  (def reg-file "./resources/2022_Lewis_Clark.csv")
  (def sys-file "./resources/2022_LC_Timing_Registration.csv")
  (def chip-file "./resources/arkfeld-chip-ids.csv")


  (-main reg-file sys-file chip-file)

  (defn de-dup-rider [in-file out-file chip-file]
    (prn (str "Hello Nebraska Offroad Series"))
    (let [data (read-file in-file)
          chips (seq (read-file chip-file))
          indexes (get-col-indexes filter-by data)]
      (->> data
           (map #(filter-cols indexes %))
           (add-rows)
           (sort-col "Registrant(s)")
          ;;  (add-chip-ids chips)
           )))



  (def chips (read-file chip-file))
  (def sorted-data (de-dup-rider reg-file sys-file chip-file))
  (add-chip-ids-from-list chips sorted-data)
  
  
 
  ;; paredit practice
  ;; https://calva.io/paredit/
  (let [s [1 2 3 4 5]]
    (->> s
         (map (partial * (apply + [1 2 3])))
         (repeat 4)
         (zipmap (range 4))))
  ()
  )


