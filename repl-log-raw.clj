(.equals 1 1N)

(== 1 1N)

(= 1 1N)

(= #{1} #{1N})

(= #{1} #{(BigInteger. 1)})

nil

32

*ns*

(in-ns 'clojure.core)

(in-ns 'formation.core)

:map

map

'map

::map

(require '[clojure.core :as c])

:c/toto

::c/toto

::d/toto

(namespace :c/toto)

(class map)

#{:a :b :c}

(conj *1 :d)

(conj *1 :c)

[:a :b :c]

(conj *1 :d)

(conj *1 :c)

(doc map)

(find-doc #"patt?ern")

(apropos "patter")

(apropos "seq")

(source map)

(doc conj)

(conj '(1 2 3) 4)

(conj [1 2 3] 4)

(class '(1 2 3))

(def a [1 2 3])

a

(conj a 4)

a

[*1 *2]

map

#'map

(meta #'map)

(pprint *1)

(meta [1 2 3])

(meta ^{:foo :bar } [1 2 3])

(doc map)

(doc for)

(source for)

(conj nil 2)

(println #_"hello world"\n         "bonjour maître")

(println (comment "hello world")\n         "bonjour maître")

#"\\d"

(re-seq #"\\d+" "a123b435")

(defn sq [x] (* x x))

(sq 4)

(doc sq)

(defn sq "Met au carré l'argument" \n  [x] \n  (* x x))

(doc sq)

(fn [x] (* x x))

(fn [x] (* x x x))

(*1 4)

(*2 4)

(*3 4)

((fn [x] (* x x x)) 4)

(map (fn [x] (* x x x)) (range 10))

(map #(* % % %) (range 10))

(fn fact [n]\n  (if (pos? n) \n    (* n (fact (dec n)))\n    1))

(*1 6)

(fn fact [n]\n  (if (pos? n) \n    (* n (fact n))\n    1))

(*1 6)

(doc pos?)

(seq [])

(= (Boolean. false) false)

(if (Boolean. false) "vrai" "faux")

(if (Boolean/valueOF false) "vrai" "faux")

(if (Boolean/valueOf false) "vrai" "faux")

(doc some)

(some (fn [x]\n        (if (pos? x)\n          x\n          nil))\n      [-12 -134 54 67])

(some (fn [x]\n        (if (pos? x)\n          (+ 2 x)\n          nil))\n      [-12 -134 54 67])

(#{:a :b :c} :a)

(#{:a :b :c} :d)

(some (set "abc") "robert")

(seq "abc")

(some (fn [x]\n        (if (pos? x)\n          (+ 2 x)))\n      [-12 -134 54 67])

(let [a 1\n      b (inc a)\n      c (+ (* b 2) a)]\n  [a b c])

(let [[a b c] [1 2 3]]\n  [b a a c]\n  )

(let [[a b c] [1 2 3 4]]\n  [b a a c]\n  )

(let [[a b c] [1 2]]\n  [b a a c]\n  )

(let[[a b c][1 2 3 4]][b a a c])

(a#{})

(let [[a b c] [1 [2 3] 4]]\n  {:a a\n   :b b\n   :c c}\n  )

(let [[a [b1 b2] c] [1 [2 3] 4]]\n  {:a a\n   :b1 b1\n   :b2 b2\n   :c c}\n  )

(let [[a [b1 b2 :as b] c] [1 [2 3] 4]]\n  {:a a\n   :b1 b1\n   :b2 b2\n   :b b\n   :c c}\n  )

(let [[a [b1 b2 :as b] c :or {c 42}] [1 [2 3] 4]]\n  {:a a\n   :b1 b1\n   :b2 b2\n   :b b\n   :c c}\n  )

(let [[a & as] [1 [2 3] 4]]\n  {:a a\n   :as as}\n  )

(defn test-sigs\n  "Doc"\n  ([] 0)\n  ([x] 1)\n  ([x y] 2)\n  ([x y & ys] '2+))

(defn test-sigs\n  "Doc"\n  ([] 0)\n  ([x] 1)\n  ([x y] 2)\n  ([x y & ys] "2+"))

(test-sigs)

(test-sigs 3 4 5)

(doc test-sigs)

((fn [[a b] c]\n   {:a a\n    :b b\n    :c c})\n  [1 2]\n  3)

(let [{a :a b :b} {:a 1 :b 2}]\n  [a b])

(let [{a :a b :b c :c} {:a 1 :b 2}]\n  [a b])

(let [{a :a b :b c :c} {:a 1 :b 2}]\n  [a b c])

(let [{a :a b :b c :c} {:a 1 :b 2 :d 4}]\n  [a b c])

(let [{:keys [a b c d]} {:a 1 :b 2 :d 4}]\n  [a b c d])

(let [{:strs [a b c d]} {"a" 1 :b 2 :d 4}]\n  [a b c d])

(let [{:syms [a b c d]} {"a" 1 'b 2 :d 4}]\n  [a b c d])

(let [{:keys [a b c d] :or {c 42}} {:a 1 :b 2 :d 4}]\n  [a b c d])

(when true\n  (print "coucou")\n  42)

(if true\n  (print "coucou")\n  42)

(if true\n  (do\n    (print "coucou")\n    42\n    ))

(if true\n  (do\n    (print "coucou")\n    42))

(macroexpand-1 '(when true\n     (print "coucou")\n     42))

(fn fact\n  ([n] (fact n 1))\n  ([n acc]\n  (if (pos? n) \n    (fact (dec n) (* n acc))\n    acc)))

(*1 6)

(fn fact\n  ([n] (fact n 1))\n  ([n acc]\n  (if (pos? n) \n    (recur (dec n) (* n acc))\n    acc)))

(fn fact\n  ([n] (fact n 1))\n  ([n acc]\n  (if (pos? n) \n    (fact (dec n) (* n acc))\n    acc)))

(*1 1000)

(fn fact\n  ([n] (fact n 1N))\n  ([n acc]\n  (if (pos? n) \n    (fact (dec n) (* n acc))\n    acc)))

(*1 1000)

(*1 10000)

(defn fact\n  ([n] (fact n 1N))\n  ([n acc]\n  (if (pos? n) \n    (fact (dec n) (* n acc))\n    acc)))

(fact 10000)

(defn fact\n  ([n] (fact n 1N))\n  ([n acc]\n  (if (pos? n) \n    (recur (dec n) (* n acc))\n    acc)))

(fact 10000)

(time (fact 10000))

(fn fact [n]\n  (if (pos? n) \n    (* n (recur (dec n)))\n    1))

(defn fact [n] \n  (loop [n n acc 1]\n    (if (pos? n) \n      (recur (dec n) (* n acc))\n      acc)))

(time (fact 10000))

(defn fact [n] \n  (loop [n n acc 1N]\n    (if (pos? n) \n      (recur (dec n) (* n acc))\n      acc)))

(time (fact 10000))

(defn fact [n] \n  (loop [n n acc 1]\n    (if (pos? n) \n      (recur (dec n) (* n acc))\n      acc)))

(time (fact 10000N))

(apropos "unchecked")

(seq [1 2 3])

(class *1)

(class '(1 2 3))

(seq '(1 2 3))

(seq #{1 2 3})

(seq {1 2 3 4})

(conj {1 2 3 4} [5 6])

(first (seq [1 2 3]))

(rest (seq [1 2 3]))

(rest [1 2 3])

(source empty?)

(def s (map (fn [x] (println x) x) '(1 2 3 4)))

(first s)

(def s (map (fn [x] (println ">>" x) x) '(1 2 3 4)))

(first s)

(def r (rest s))

(def n (next s))

s

sequential?

(sequential? #{})

(sequential? [])

(doc sequential?)

(sequential? s)

(seq? [])

(set "abc")

(seq "abc")

(reduce conj #{} "abc")

(reduce + 0 [1 2 3 4])

(reduce conj #{} "abc")

(find-doc "iterator")

(apropos #"-seq$")

(take 5 (range 10))

(drop 1 (range 10))

(rest (range 10))

(def s (map (fn [x] (println ">>" x) x) '(1 2 3 4)))

(def d (drop 1 s))

(def r (rest s))

(reduce * (range 1 6))

(reduce * (range 1 7))

(defn fact [n]\n  (reduce * (range 1 (inc n))))

(fact 10000N)

(defn fact [n]\n  (reduce * (range 1N (inc n))))

(fact 10000N)

(reduce + nil)

(+)

(*)

(-)

(defn fact [n]\n  (reduce * (range 1N (inc n))))

(take-while #(< % 7) (range 100))

(+ 4 a 7)

(+ 4 5 7)

(def s2 (cons 1 s))

(def s3 (lazy-seq [1 2 3]))

(def s3 (lazy-seq \n          (println "!!!")\n          [1 2 3]))

(first s3)

(conj [1 2 3] 4)

(peek *1)

(first *2)

(conj '(1 2 3) 4)

(peek *1)

(first *2)

(class (cons 4 '(1 2 3)))

(class (conj '(1 2 3) 4))

(take 10 (range))

(take 10 (conj (range) 42))

\n (class (conj (range) 42))

(cons 4 [1 2 3])

(class (range))

(peek (range))

(peek (range 10))

(count (range 30))

(peek (range))

(count (range))

(assoc {1 2 3 4} 5 6)

(assoc {1 2 3 4} 5 6 7 8)

(assoc [1 2 3 4] 1 42)

(assoc [1 2 3 4] 4 42)

(assoc [1 2 3 4] 5 42)

(get [1 2 3 4] 2)

(get {1 2 3 4} 2)

(get {1 2 3 4} 1)

(get {:a 2 :b 4} :a)

(:a {:a 2 :b 4})

(get {:a 2 :b 4} :c 57)

(:c {:a 2 :b 4} 57)

({:a 2 :b 4} :c 57)

({1 2 3 4} :c 57)

({1 2 3 4} 1 57)

(get #{:a :b :c})

(get #{:a :b :c} :a)

(get #{:a :b :c} :d)

(get #{:a :b nil} nil)

(contains? #{:a :b nil} nil)

(get #{:a :b nil} nil "pas trouvé")

(get nil :foo)

(assoc nil 1 :a)

(peek nil)

(pop nil)

(pop [])

(peek [])

(find {:a 1 :b 2} :a)

(doc find)

(or (find {:a 1 :b 2} :a) [:d 4])

(or (find {:a 1 :b 2} :d) [:d 4])

(or (find {:a 1 :b 2} :f) [:d 4])

(macroexpand-1 '(or (find {:a 1 :b 2} :f) [:d 4]))

(pprint *1)

(find [1 2 3 4] 2)

(find (vec "abcde") 2)

(key *1)

(val *2)

(nth [1 2 3] 2)

(nth [1 2 3] 2 nil)

(nth [1 2 3] 4 "defaut")

(nth [1 2 3] 4)

(get [1 2 3] 4 "defaut")

(get [1 2 3] 4 )

(rseq [1 2 3])

clojure.lang.Reversible

(disj #{1 2 3} 2)

(dissoc {1 2 3 4} 3)

(dissoc {1 2 3 4} 6)

(dissoc [1 2 3 4] 3)

(defn rm [v n]\n  (into (subvec v n) (drop (inc n) v)))

(rm [1 2 3 4] 2)

(defn rm [v n]\n  (into (subvec v 0 n) (drop (inc n) v)))

(rm [1 2 3 4] 2)

(def a (java.util.ArrayList. (range 100000)))

(time (.remove a 0))

(def a (java.util.ArrayList. (range 100000)))

(time (.remove a 99999))

(def a (java.util.ArrayList. (range 100000)))

(time (.remove a 0))

(time (dotimes [_ 100] (.remove a 0)))

(time (dotimes [_ 100] (.remove a 10000)))

(time (dotimes [_ 100] (.remove a 100)))

(class (.remove a 100))

(count a)

(def a (java.util.ArrayList. (range 100000)))

(count a)

(.remove a 0)

(count a)

(.remove a 0)

(count a)

(def a (java.util.ArrayList. (range 100000)))

(time (dotimes [_ 100] (.remove a (int 10000))))

(count a)

(def a (java.util.ArrayList. (range 100000)))

(time (dotimes [_ 100] (.remove ^java.util.ArrayList a (int 10000))))

(count a)

(time (dotimes [_ 100] (.remove ^java.util.ArrayList a 10000)))

(count a)

(set! *warn-on-reflection* true)

(def a (java.util.ArrayList. (range 100000)))

(time (dotimes [_ 100] (.remove a 10000)))

(def ^java.util.ArrayList a (java.util.ArrayList. (range 100000)))

(time (dotimes [_ 100] (.remove a 10000)))

(time (dotimes [_ 1000] (.remove a 10000)))

(time (dotimes [_ 1000] (.remove a 0)))

(time (dotimes [_ 1000] (.remove a 10)))

(def ^java.util.ArrayList a (java.util.ArrayList. (range 1000000)))

(time (dotimes [_ 1000] (.remove a 900000)))

(time (dotimes [_ 1000] (.remove a 2)))

(sorted-map 1 2 3 4 5 6)

(sorted-map 1 2 5 6 3 4)

(sorted-map > 1 2 5 6 3 4)

(sorted-map-by > 1 2 5 6 3 4)

(ancestor (class #()))

(ancestors (class #()))

(sorted-map-by > 1 2 5 6 3 4)

(sorted-map 1 2 5 6 3 4)

(subseq *1 < 2 <= 5)

(subseq *2 > 2 <= 5)

(rseq *3)

(for [a (range 20)\r      :let [ha (/ a 2)]\r      b (range 2 (inc ha))\r      :when (zero? (rem a b))]\r  [a b])

(for [a (range 3) b (range 30 33)] [a b])

(for [a "abc" b (range 3)] [a b])

(for [a "abc" b (range 3) c "DEF"] [a b c])

(for [a "abc"\n      :let [_ (println ">>" a)] \n      b (range 3) \n      c "DEF"] [a b c])

(do (for [a "abc"\n      :let [_ (println ">>" a)] \n      b (range 3) \n      c "DEF"] [a b c]) \n  nil)

(first (for [a "abc"\n      :let [_ (println ">>" a)] \n      b (range 3) \n      c "DEF"] [a b c]))

(take 3 (for [a "abc"\n      :let [_ (println ">>" a)] \n      b (range 3) \n      c "DEF"] [a b c]))

(prn (lazy-seq nil))

(println (lazy-seq nil))

(str (lazy-seq nil))

(str (doall (lazy-seq nil)))

(do (doall (for [a "abc"\n      :let [_ (println ">>" a)] \n      b (range 3) \n      c "DEF"] [a b c])) \n  nil)

(do (dorun (for [a "abc"\n      :let [_ (println ">>" a)] \n      b (range 3) \n      c "DEF"] [a b c])) \n  nil)

 \n(dorun (for [a "abc"\n      :let [_ (println ">>" a)] \n      b (range 3) \n      c "DEF"] [a b c]))

 \n(doall (for [a "abc"\n      :let [_ (println ">>" a)] \n      b (range 3) \n      c "DEF"] [a b c]))

 \n(doseq [a "abc"\n      :let [_ (println ">>" a)] \n      b (range 3) \n      c "DEF"] [a b c])

 \n(doseq [a "abc"\n      :let [_ (println ">>" a)] \n      b (range 3) \n      c "DEF"] (prn [a b c]))

(def persons [{:fname "Adam" :lname "Smith"}\n              {:fname "John" :lname "Smith"}\n              {:fname "Joe" :lname "Black"}\n              {:fname "Sirius" :lname "Black"}\n              {:fname "Stephane" :lname "Tavera"}\n              {:fname "Stephane" :lname "Guillon"}]\n  )

(doc update-in)

(assoc-in {} [:a :b :c] 42)

(update-in *1 [:a :b :c] inc)

(update-in *1 [:a :b :d] inc)

(update-in *1 [:a :b :d] (fnil inc 0))

(reduce (fn [m {:keys [lname fname]}]\n          (assoc m lname fname)) \n        {} persons)

(reduce (fn [m {:keys [lname fname]}]\n          (if-let [others (get m lname)]\n            (assoc m lname (conj others fname))\n            (assoc m lname [fname]))) \n        {} persons)

(agg-by-lname persons)

(reduce (fn [m {:keys [lname fname]}]\n          (if-let [others (get m lname)]\n            (conj m [lname (conj others fname)])\n            (assoc m lname [fname]))) \n        {} persons)

(agg-by-lname persons)

(group-by :lname persons)

(keys {:a 1 :b 2})

(vals {:a 1 :b 2})

(zipmap *1 *2)

(let [g (group-by :lname persons)\n      ks (keys g)\n      vs (vals g)]\n  (zipmap ks (map :fname vs)))

(let [g (group-by :lname persons)\n      ks (keys g)\n      vs (vals g)]\n  (map :fname vs))

(let [g (group-by :lname persons)\n      ks (keys g)\n      vs (vals g)]\n   vs)

(let [g (group-by :lname persons)\n      ks (keys g)\n      vs (seq (vals g))]\n   vs)

(let [item [{:lname "Smith", :fname "Adam"} \n            {:lname "Smith", :fname "John"}]\n      ]\n  (map :fname item))

(map (fn [x]\n       x) \n  (group-by :lname persons))

(map (fn [[lname persons]]\n       [lname (map :fname persons)]) \n  (group-by :lname persons))

(into {}\n      (map (fn [[lname persons]]\n             [lname (map :fname persons)]) \n           (group-by :lname persons)))

(reduce conj {}\n      (map (fn [[lname persons]]\n             [lname (map :fname persons)]) \n           (group-by :lname persons)))

(agg-by-lname2 persons)

(reduce conj {}\n      (map (fn [[lname persons]]\n             [lname (vec (map :fname persons))]) \n           (group-by :lname persons)))

(for [[lname persons] \n        (group-by :lname persons)\n        person persons] \n  [lname (:fname person)])

(doc group-by)

(reduce-by :lname (fn [others person]\n                    (conj others (:fname person)))\n           [] persons)

(reduce-by :fname (fn [others person]\n                    (conj others (:lname person)))\n           [] persons)

(reduce-by identity \n           (fn [n _] (inc n))\n           0 [1 2 7 9 1 2 5 8 7 2])

(reduce-by identity \n           (fn [n _] (inc n))\n           0 "qu'allait-il donc faire dans cette galère")

(doc frequencies)

(frequencies "qu'allait-il donc faire dans cette galère")

(source frequencies)

(reduce-by identity \n           (fn [n _] (inc n))\n           0 "qu'allait-il donc faire dans cette galère")

(reduce-by :lname (fn [others person]\n                    (conj others (:fname person)))\n           [] persons)

(source into)

(defn mbind [maybe f]\n  (when maybe\n    (f (first maybe))))

(defn mbind [maybe f]\n  (when-let [[x] maybe]\n    (f x)))

(defn maybe [x] \n  [x])

(defn mdiv [a b]\n  (when-not (zero? b)\n    [(/ a b)]))

(mdiv 12 3)

(mdiv 12 0)

(mbind (maybe 12) #(mdiv % 2) #(mdiv % 3))

(mbind (maybe 12) #(mdiv % 2))

(mbind (mbind (maybe 12) #(mdiv % 2)) #(mdiv % 3))

(mbind (mbind (maybe 12) #(mdiv % 0)) #(mdiv % 3))

#{[1 1] [1 2] [1 3]}

(defn neighours [[i j]]\n  (for [di [-1 0 1]\n        dj (if (zero? di) [-1 1] [-1 0 1])]\n    [(+ di i) (+ dj j)]))

(defn neighbours [[i j]]\n  (for [di [-1 0 1]\n        dj (if (zero? di) [-1 1] [-1 0 1])]\n    [(+ di i) (+ dj j)]))

(neighbours [1 1])

(count *8)

(count *1)

(neighbours [0 0])

(mapcat neighbours #{[1 1] [1 2] [1 3]})

(frequencies *1)

(defn step [cells]\n  (for [[cell n] (frequencies \n                   (mapcat neighbours cells))\n        :when (or (= n 3) (and (= n 2) (cells cell)))]\n    cell))

(step #{[1 1] [1 2] [1 3]})

(step *1)

(defn step [cells]\n  (set\n    (for [[cell n] (frequencies \n                     (mapcat neighbours cells))\n          :when (or (= n 3) (and (= n 2) (cells cell)))]\n      cell)))

(step #{[1 1] [1 2] [1 3]})

(step *1)

(partition 3 (range 20))

(partition 3 1 (range 20))

(println "coucou")

(with-out-str\n  (println "coucou"))

(source with-out-str)

(def a (atom 0))

@a

(swap! a inc)

(swap! a (fn [x] (Thread/sleep 1000) (inc x)))

(compare-and-set! a 3 4)

@a

(compare-and-set! a 2 4)

@a

(doc io!)

(constantly 5)

(*1)

(*1 "rfdzayzd" 56)

(*2 "rfdzayzd" 56)

(find-doc "agent-err")

(doc atom)

(def a (atom 1 :validator pos?))

(swap! a inc)

(swap! a -)

(- 1)

@a

(doc add-watch)

(= (atom 2) (atom 2))

(add-watch a :k (fn [k r os ns]\n                  (println ">>" os ns)))

(swap! a + 10)

(def ag (agent 6))

(send! ag (fn [x]\n            (Thread/sleep 10000)\n            (inc x)))

(send ag (fn [x]\n            (Thread/sleep 10000)\n            (inc x)))

@ag

(await-for ag)

(+ 2 2)

(eval "(+ 2 2)")

(doc await-for)

(conj [] 1 2 3)

(cons 1 '(2 3))

(swap! (atom []) conj 2)

(def a (atom 1))

(swap! a list 'x)

(def a (atom 1))

(def a (atom 100))

(swap! a / 2)

(-> [] (conj 1) (conj 2))

(->> (range 100) (filter odd?) (take 10))

(->> (range 100) (filter odd?) (take 10) drop)

(->> (range 100) (filter odd?) (take 10) rest)

(-> "STR" java.io.StringReader.\n  java.io.ReaderInputStream.)

(doc release-pending-sends)

(delay\n  (println "évalué")\n  5)

@*1

@*2

(def f (future (Thread/sleep 10000) 6))

@f

(def p (promise))

(def f (future (println "chose promise, chose" @p)))

(deliver p "due")

@f

(def f (future (str "chose promise, chose " @p)))

(def p (promise))

(def f (future (str "chose promise, chose " @p)))

(deref f 1000)

(doc deref)

(deref f 1000 nil)

(deliver p "due")

(deref f 1000 nil)

(deliver p "remise")

@p

(doc deliver)

*agent*

(def ag (agent nil))

(send agent (fn [_]\n              (deliver "redue")))

(send ag (fn [_]\n              (deliver "redue")))

(agent-error ag)

(def ag (agent nil))

(send ag (fn [_]\n              (deliver p "redue")))

(agent-error ag)

@p

(defn foldr [f init coll]\n  (if-let [[x & xs] (seq coll)]\n    (f (delay (foldr f init xs))\n       x)\n    init))

(foldr (fn [dacc x]\n         (+ @dacc x)) (range 10))

(foldr (fn [dacc x]\n         (+ @dacc x)) \n       0 (range 10))

(foldr (fn [dacc x]\n         (if (< x 5)\n           (+ @dacc x)\n           @dacc)) \n       0 (range 10))

(in-ns 'formation.tron)

(doc repeatedly)

#(#())

(first arena)

(map (fn [x]\n       ({nil \\space\n         1 "X"} x)) \n     (first arena))

(map (fn [x]\n       ({nil \\space\n         1 "X"} x "pas trouvé")) \n     (first arena))

(map (fn [x]\n       ({nil \\space\n         1 "X"} @x "pas trouvé")) \n     (first arena))

(apply str (map (fn [x]\n                  ({nil \\space\n                    1 "X"} @x "pas trouvé")) \n                (first arena)))

(str-row (first arena))

(show)

(valid-pos? 30 4)

(valid-pos? 10 4)

(valid-pos? 0 4)

(get-in arena [-1 -1])

(future (kamikaze 1 5 5))

(kamikaze 1 5 5)

(pst)

(kamikaze 1 5 5)

(show)

(kamikaze 1 5 5)

(show)

(kamikaze 2 1 5)

(show)

(kamikaze 2 5 1)

(show)

(biker kamikaze 1 5 5)

(biker kamikaze 2 5 1)

(show)

(map #(%1 %2) [inc identity] [1 1])

(map #(%1 %2) [inc identity] [1 4])

(map + [1 0] [1 4])

(biker kamikaze 2 5 1)

(show)

(biker beginner 'T 0 0)

(show)

(first arena)

(biker beginner 1 0 0)

(first arena)

(biker beginner 1 2 2)

(show)

(valid-pos? 0 0)

(biker stubborn 'S 0 0)

(show)

(biker stubborn 'S 5 0)

(show)

(biker stubborn 'S 5 0)

(future (biker stubborn 'S 5 0))

(show)

(biker stubborn 'S 10 2)

(future (biker stubborn 'X 9 2))

(show)

(while true\n  (show)\n  (Thread/sleep 1000))

(doc some)

(source some)

(future (biker stubborn 'X 9 2))

(future (biker stubborn 'S 5 4))

(while true\n  (show)\n  (Thread/sleep 1000))

(future (biker stubborn 'X 9 2))

(future (biker stubborn 'S 5 4))

(while true\n  (show)\n  (Thread/sleep 1000))

(future (biker stubborn 'S 5 4))

(while true\n  (show)\n  (Thread/sleep 1000))

(biker stubborn 'S 5 4)

(show)

(biker avoider 'A 1 4)

(show)

(find-doc "random")

(rand-nth [1 2 3\n           ])

(future (biker avoider 'A 1 4))\n(future (biker avoider 'B 6 8))

(while true\n  (show)\n  (Thread/sleep 1000))

\n(future (biker avoider 'C 7 9))

(while true\n  (show)\n  (Thread/sleep 1000))

(in-ns 'formation.oo)

(add 2 2)

(add 2 2.0)

(add 2.0 2)

(add 2 2)

(add 2.0 2)

(def add nil)

(add 2 2)

(def add nil)

(add 2 2)

(def add nil)

(add 2 2)

(add 2 2.0)

(add 2.0 2)

(say {:race :dog})

(def say nil)

(say {:race :dog} "hello")

(in-ns 'formation.tron)

BikerStrategy

formation.tron.BikerStrategy

(init avoider2)

(.init avoider2)

(reify clojure.lang.IFn\n  (invoke [f x]\n          (str "hello x")))

(reify clojure.lang.IFn\n  (invoke [f x]\n          (str "hello " x)))

(*1 "world")

(ancestors (class (conj)))

(ancestors (class conj))

(.invoke conj [] 1)

(init stubborn)

(init conj)

(decide conj 5 5 :left)

(show)

(future (biker avoider 'A 1 4))\n(future (biker avoider2 'B 6 8))

(show)

(init nil)

(decide nil 2 3 :left)

(TransparentKamikaze. :left)

(:dir *1)

(assoc *2 :dir :right)

(def ltk (TransparentKamikaze. :left))

(def rtk (assoc ltk :dir :right))

(= ltk rtk)

ltk

rtl

rtk

(fing ltk :dir)

(find ltk :dir)

(ltk :dir)

(get ltk :dir)

(:dir ltk)

(show)

(future (biker ltk 'T 10 15))

(show)

(doc ->TransparentKamikaze)

(->TransparentKamikaze :left)

(map->TransparentKamikaze :left)

(map->TransparentKamikaze {:dir :right})

(map->TransparentKamikaze {})

(map->TransparentKamikaze {:dur :left})

(dissoc *1 :dir)

(.dir ltk)

(set! *warn-on-reflection* true)

(.dir ltk)

(.dir ^TransparentKamikaze ltk)

(deftype MonType [a b])

(ancestors (class *1))

(ancestors MonType)

(deftype MonType [a b])

(def mt (MonType. 1 2))

(set! (.a mt) 7)

(deftype MonType [^:volatile-mutable a b])

(def mt (MonType. 1 2))

(set! (.a mt) 7)

(deftype MonType [^:volatile-mutable a b]\n  java.util.concurrent.Callable\n  (call [this]\n        (set! a 7)))

(def mt (MonType. 1 2))

(.call mt)

(.a mt)

(defmacro idem [x] x)

(idem 42)

(mapcroexpand-1 '(idem 42))

(macroexpand-1 '(idem 42))

(macroexpand-1 '(when test expr))

(macroexpand-1 '(cond a b c d))

(macroexpand-1 (macroexpand-1 '(cond a b c d)))

(macroexpand '(cond a b c d))

(macroexpand-1 '(when-let a b c d))

(macroexpand-1 '(when-let [a b] c d))

(macroexpand '(when-let [a b] c d))

(apropos "macro")

macroexpand-all

clojure.walk/macroexpand-all

(macroexpand '(cond a b c d))

(clojure.walk/macroexpand-all '(cond a b c d))

(defmacro stutter [x]\n  (list 'do x x x))

(stutter (println "hello"))

(macroexpand-1 '(stutter (println "hello")))

(defmacro stutter [x]\n  `(do \n     ~x \n     ~x \n     ~x))

(stutter (println "hello"))

(macroexpand-1 '(stutter (println "hello")))

(defmacro stutter [x]\n  `(do \n     x \n     ~x \n     ~x))

(stutter (println "hello"))

(macroexpand-1 '(stutter (println "hello")))

(defmacro my-if-let [[binding expr] then else]\n  `(if ~expr\n     (let [~binding ~expr]\n       ~then)\n     ~else))

(macroexpand-1 '(my-if-let [x (+ 1 1)] 3 4))

(my-if-let [x (do (println "coucou")\n                (+ 1 1))]\n           3 4)

(defmacro my-if-let [[binding expr] then else]\n  `(let [tmp ~expr]\n     (if tmp\n       (let [~binding tmp]\n         ~then)\n       ~else)))

(my-if-let [x (do (println "coucou")\n                (+ 1 1))]\n           3 4)

(defmacro my-if-let [[binding expr] then else]\n  `(let [tmp# ~expr]\n     (if tmp#\n       (let [~binding tmp#]\n         ~then)\n       ~else)))

(my-if-let [x (do (println "coucou")\n                (+ 1 1))]\n           3 4)

(macroexpand-1 '(my-if-let [x (do (println "coucou")\n                (+ 1 1))]\n           3 4))

(defmacro my-if-let [[binding expr] then else]\n  `(let [tmp# ~expr]\n     (if tmp#\n       (let [~binding tmp#]\n         ~then)\n       ~else)))

(macroexpand-1 '(my-if-let [x (do (println "coucou")\n                (+ 1 1))]\n           3 4))

(defmacro my-if-let [[binding expr] then else]\n  (let [tmp (gensym 'tmp)]\n    `(let [~tmp ~expr]\n       (if ~tmp\n         (let [~binding ~tmp]\n           ~then)\n         ~else))))

(macroexpand-1 '(my-if-let [x (do (println "coucou")\n                (+ 1 1))]\n           3 4))

(defmacro my-if-let [[binding expr] then else]\n  `(let [tmp# ~expr]\n     (if tmp#\n       (let [~binding tmp#]\n         ~then)\n       ~else)))

(defmacro my-when-let [[binding expr] & exprs]\n  `(let [tmp# ~expr]\n     (if tmp#\n       (let [~binding tmp#]\n         ~exprs)\n       nil)))

(macroexpand-1 '(my-when-let [x (do (println "coucou")\n                (+ 1 1))]\n           3 4))

(defmacro my-when-let [[binding expr] & exprs]\n  `(let [tmp# ~expr]\n     (if tmp#\n       (let [~binding tmp#]\n         ~@exprs)\n       nil)))

(macroexpand-1 '(my-when-let [x (do (println "coucou")\n                (+ 1 1))]\n           3 4))

(macroexpand '(let [[a b] [0 1]]))

(pprint *1)

(macroexpand '(let [[a [b c]] [0 1]]))

(pprint *1)

(macroexpand '(let [[a [b c]] [0 1]]))

(pprint *1)

(defmacro test []\n  &form)

(macroexpand-1 '(test))

(defmacro test [& exprs]\n  exprs)

(macroexpand-1 '(test))

(defmacro test []\n  &env)

(defmacro test []\n  (keys &env))

(macroexpand-1 '(test))

(macroexpand-1 '(let [a 1] (test)))

(clojure.walk/macroexpand-all '(let [a 1] (test)))

(defmacro test []\n  `(println '~&env))

(defmacro test []\n  `(println '~(keys &env)))

(test)

(let [a 1] (test))

(defmacro test []\n  `(println '(keys &env)))

(let [a 1] (test))

(defmacro test []\n  `(println '(keys ~&env)))

(let [a 1] (test))

(defmacro test []\n  `(println '(keys ~{'a 1})))

(let [a 1] (test))

(defmacro test []\n  `(println (keys '~{'a 1})))

(let [a 1] (test))

(defmacro test []\n  `(println '~(keys &env) ~(vec (keys &env))))

(let [a 1] (test))

(let [a 1 b 3.14] (test))

(let [a 1 [b c] [0 3.14]] (test))

*ns*

(show)

(future (biker avoider 'A 1 4))\n(future (biker avoider2 'B 6 8))

(while true\n  (show)\n  (Thread/sleep 1000)\n  (println))

(show)

(defn app [] "Coucou")

(defn factory [app]\n  #(println (app)))

(factory app)

((factory app))

(def f (factory app))

(f)

(defn app [] "hello")

(f)

(def g (factory #'app))

(g)

(f)

(defn app [] "hallo !")

(g)

(def app "hello")

(defn factory [app]\n  #(println app))

(def g (factory #'app))

(g)

(defn factory [app]\n  #(println @app))

(def g (factory #'app))

(g)

(show)

(get-in arena [0 10])

(apropos "histo")

(ref-history-count *2)

(doc ref)

(ref-max-history (get-in arena [0 10]))

(doc ref-max-history)

(deftype MaDate [d m y]\n  )

(defn scaffold [iface]\n  (doseq [[iface methods] (->> iface .getMethods\n                            (map #(vector (.getName (.getDeclaringClass %))\n                                    (symbol (.getName %))\n                                    (count (.getParameterTypes %))))\n                            (group-by first))]\n    (println (str "  " iface))\n    (doseq [[_ name argcount] methods]\n      (println\n        (str "    "\n          (list name (into ['this] (take argcount (repeatedly\ngensym)))))))))

(scaffold clojure.lang.Associatve)

(scaffold clojure.lang.Associative)

(in-ns 'formation.advanced)

(MaDate. 2012 5 16)

(def d (MaDate. 2012 5 16))

(def e (assoc d :month 6))

(.m d)

(.m ^MaDate d)

(.m ^MaDate e)

(nth [1 2 3] 10)

(get [1 2 3] 10)

(get [1 2 3] 10 0)

(nth [1 2 3] 10 0)

(println "a" "b" "c")

(println "a" (comment "b") "c")

(println "a" #_"b" "c")

(println "a" "" "c")

'(println "a" #_"b" "c")

'#()

'#'a

'`(if ~a)

:a/b

(require '[clojure.core :as c])

:c/b

::c/b

'::c/b

'a/b

`a/b

'c/b

`c/b

:c/b

::c/b

::b

'`(if ~a)

(pprint *1)

'^:tag sym

(meta *1)

'^Object sym

(meta *1)

(meta '^Object sym)

(meta '^"Object" sym)

(meta '^:cool sym)

(meta '^:cool ^:private sym)

(meta '^:cool ^:private toto)

(meta '^:cool ^:private ^Object ^{"pi" 3.14} toto)

(meta '^"Object" sym)

'#(f %)

'@a

(macroexpand '(cond a b c d e f))

(macroexpand '(let [[a b] [0 1]]))

(pprint *1)

(macroexpand '(let [[a b] [3 4]]))

(pprint *1)

(defmacro oops []\n  `(let [a 42]))

(oops)

(add 2 2)

(add 2 2.0)

(add 2.0 2)

(add 2 2)

(add 2 2.0)

(add 2.0 2)

(add 2 2 3 4)

(add 2 2 3.0 4)

(add 2N)

(add 2M)

(ancestors Integer)

(defprotocol Proto)

(defprotocol Proto\n  (method [this]))

Proto

*ns*

formation.advanced

formation.advanced.Proto

(seq "abc")

(subs "abc" 1)

(subs "abc" 3)

(subs "abc" 4)

(car "abc")

(cdr "abc")

(at "abc" 2)

(subvec [1 2 3] 1)

(subvec [1 2 3] 4)

(car [1 2 3])

(cdr [1 2 3])

(at [1 2 3])

(at [1 2 3] 2)

#_#_ attention piege

#_ #_ attention piege

#_ #_attention piege

(at "abcds" 2)

(at (vec "abcds") 2)

(cdr {:a 1 :b 2})

(cdr :keyword)

(defprotocol TestFail\n  (fail [_]))

(fail :k)

(at [] 0)

(at [1] 0)

(doc extend)

(at [1] 0)

(source into)

(->> (range 10)\n  (map inc)\n  (map #(* 2 %)))

(->> (range 10)\n  (map (comp #(* 2 %) inc)))

(->> (range 10)\n  (map (comp #(* 2 %) inc))\n  (reduce +))

(->> (range 10)\n  (reduce (fn [acc x]\n            (+ acc ((comp #(* 2 %) inc) x))) 0))

(doc keep)

(meta #'keep)

(keep odd? (range 10))

(filter odd? (range 10))

(keep #(when (odd? %) (inc %)) (range 10))

(->> (range 10) (filter odd?) (map inc))

(macroexpand '(dosync blbl))

