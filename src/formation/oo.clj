(ns formation.oo)

#_(defmulti nom-méthode fonction-dispatch)

(defmulti add (fn [a b] (set [(class a) (class b)])))

(defmethod add #{Long} [a b]
  "C'est un Long")

(defmethod add #{Long Double} [a b]
  "C'est un Double")

(defmulti say (fn [x & xs] (:race x)))

(defmethod say :dog [_ s]
  (str "ouaf " s))