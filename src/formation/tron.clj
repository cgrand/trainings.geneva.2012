(ns formation.tron)

(def size 20)

(def arena
  (vec
    (repeatedly 
      size 
      (fn []
        (vec
          (repeatedly size #(ref nil)))))))

(defn str-row [row]
  (apply str (map (fn [x]
                    (or @x \.)) 
                  row)))

(defn show []
  (doseq [row arena]
    (println (str-row row))))

(defn valid-pos? [i j]
  (and (< -1 i size) (< -1 j size)))

(def dirs {:right [0 1] 
           :down [1 0] 
           :left [0 -1]
           :up [-1 0]})

(defn next-pos [i j dir]
  (let [[di dj] (dirs dir)
        i (+ i di)
        j (+ j dj)]
    [i j]))

(defn biker [strategy n i j]
  (loop [[i j] [i j], last-dir (init strategy)]
    (if-let 
      [[dir pos]
       (dosync 
         (when-let [dir (decide strategy i j last-dir)]
           (let [pos (next-pos i j dir)
                 cell (get-in arena pos)]
             (ref-set cell n)
             [dir pos])))]
      (do
        (Thread/sleep 1000)
        (recur pos dir))
      (println "arghhh" n))))

(defn try-move [i j dir]
  (let [[i j :as pos] (next-pos i j dir)
        cell (get-in arena pos)]
    (when (and (valid-pos? i j)
               (nil? @cell))
      dir)))

(defn kamikaze [i j _]
  (try-move i j :right))

(defn kamikaze-factory-old [dir]
  (fn [i j _] (try-move i j dir)))

(defn kamikaze-factory [dir]
  (reify BikerStrategy
    (init [bs]
      dir)
    (decide [bs i j last-dir]
      (try-move i j dir))))

(defn beginner [i j _]
  (or
    (try-move i j :right)
    (try-move i j :down)))

(defn stubborn [i j _]
  (some #(try-move i j %)
    (keys dirs)))

;; TODO : protocolisé le biker

(defprotocol BikerStrategy
  (init [bs] "Renvoie dir")
  (decide [bs i j last-dir] 
          "Choisit la prochaine direction"))

(def relative-dirs {:up [:left :right]
                    :down [:right :left]
                    :left [:down :up]
                    :right [:up :down]})

(defn avoider [i j last-dir]
  (or
    (try-move i j last-dir)
    (let [[left right] (shuffle
                         (relative-dirs last-dir))]
      (or
        (try-move i j left)
        (try-move i j right)))))

(def avoider2
  (reify BikerStrategy
    (init [bs]
      (rand-nth (keys dirs)))
    (decide [bs i j last-dir]
      (or
        (try-move i j last-dir)
        (let [[left right] (shuffle
                             (relative-dirs last-dir))]
          (or
            (try-move i j left)
            (try-move i j right)))))))

(extend-protocol BikerStrategy
  clojure.lang.Fn
  (init [f]
    (rand-nth (keys dirs)))
  (decide [f i j last-dir]
    (f i j last-dir))
  nil
  (init [f]
    (rand-nth (keys dirs)))
  (decide [f i j last-dir]
    nil))

(defrecord TransparentKamikaze [dir]
  BikerStrategy
    (init [bs]
          dir)
    (decide [bs i j last-dir]
      (try-move i j dir)))









