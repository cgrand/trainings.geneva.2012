(ns formation.core)

(def persons [{:fname "Adam" :lname "Smith"}
              {:fname "John" :lname "Smith"}
              {:fname "Joe" :lname "Black"}
              {:fname "Sirius" :lname "Black"}
              {:fname "Stephane" :lname "Tavera"}
              {:fname "Stephane" :lname "Guillon"}])

; attendu :
; {"Smith" ["Adam" "John"] ... ...}

(defn agg-by-lname [persons]
 (reduce (fn [m {:keys [lname fname]}]
             (assoc m lname
                    (conj (get m lname []) fname))) 
          {} persons))

(defn agg-by-lname2 [persons]
  (into {}
        (map (fn [[lname persons]]
               [lname (vec (map :fname persons))]) 
             (group-by :lname persons))))

#_
(reduce-by :lname (fn [others person]
                    (conj others (:fname person)))
           [] persons)

(defn reduce-by [keyfn f init coll]
  (persistent!
    (reduce (fn [m item]
            (let [k (keyfn item)]
              (assoc! m k
                     (f (get m k init) item)))) 
          (transient {}) coll)))

;; TRON














