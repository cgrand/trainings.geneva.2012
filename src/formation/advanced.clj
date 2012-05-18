(ns formation.advanced)

(deftype MaDate [y m d]
  clojure.lang.Associative
  (containsKey [this k]
    (contains? #{:year :month :day} k))
  (assoc [this k v]
    (case k
      :year (MaDate. v m d)
      :month (MaDate. y v d)
      :day (MaDate. y m v)))
  (entryAt [this G__2805])
  clojure.lang.IPersistentCollection
  (count [this] 3)
  (cons [this G__2806])
  (equiv [this G__2807])
  clojure.lang.Seqable
  (seq [this])
  clojure.lang.ILookup
  (valAt [this k])
  (valAt [this k default]))

(def add nil)
(defmulti add (fn [& xs] 
                (set (map class xs))))

(defmethod add #{Long} [& xs]
  "C'est un Long!")

(defmethod add #{Long Double} [& xs]
  "C'est un Double!")

(defmethod add #{Number} [& xs]
  "C'est un Nombre!")

;;;;;;;;;;

;; Implémenter un protocole sur un autre

(defprotocol List
  (car [this])
  (cdr [this]))

(defprotocol RandomAccess
  (at [this n]))

(defn slow-at [list n]
  (if (pos? n)
    (recur (cdr list) (dec n))
    (car list)))

(extend-protocol RandomAccess
  String
  (at [s n] (nth s n))
  Object
  (at [this n] (slow-at this n)))

(extend clojure.lang.APersistentVector
  RandomAccess
  {:at (fn [v n]
         (println "rapide")
         (nth v n))})

(extend-protocol List
  String
  (car [s] (first s))
  (cdr [s] (when (seq s) (subs s 1)))
  clojure.lang.APersistentVector
  (car [v] (first v))
  (cdr [v] (when (seq v) (subvec v 1)))
  Object
  (car [o] (first o))
  (cdr [o] (next o)))


