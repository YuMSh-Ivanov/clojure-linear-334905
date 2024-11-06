(ns linear)

(defn v? [x]
  (and
    (vector? x)
    (every? number? x)))

(defn m? [x]
  (and
    (vector? x)
    (not (empty? x))
    (every? v? x)
    (apply = (map count x))))

(defn shape [x]
  (cond
    (number? x) ()
    (empty? x) (list 0)
    :else (cons (count x) (shape (first x)))))

(letfn [(comp-wise [oper type?]
          (fn [& more]
            {:pre  [(not (empty? more))
                    (every? type? more)
                    (apply = (map shape more))]
             :post [(type? %)
                    (= (shape %) (shape (first more)))]}
            (apply mapv oper more)))]
  (def v+ (comp-wise + v?))
  (def v- (comp-wise - v?))
  (def v* (comp-wise * v?))
  (def vd (comp-wise / v?))
  (def m+ (comp-wise v+ m?))
  (def m- (comp-wise v- m?))
  (def m* (comp-wise v* m?))
  (def md (comp-wise vd m?)))

(defn dot [& more]
  {:pre  [(every? v? more)
          (or (empty? more) (apply = (mapv shape more)))]
   :post [(number? %)]}
  (if (empty? more)
    0
    (apply + (apply mapv * more))))

(defn v*s [x & y]
  {:pre  [(v? x)
          (every? number? y)]
   :post [(v? %)
          (= (shape %) (shape x))]}
  (mapv (apply partial * y) x))


(defn m*s [x & y]
  {:pre  [(m? x)
          (every? number? y)]
   :post [(m? %)
          (= (shape %) (shape x))]}
  (mapv #(apply v*s % y) x))


(defn m*v [x y]
  {:pre  [(m? x)
          (v? y)
          (= (second (shape x)) (first (shape y)))]
   :post [(v? %)
          (= (first (shape x)) (first (shape %)))]}
  (mapv #(dot % y) x))

(defn transpose [x]
  {:pre  [(m? x)]
   :post [(or (empty? %) (and (m? %) (= (reverse (shape %)) (shape x))))]}
  (apply mapv vector x))

(letfn [(m*m2 [x y]
          (mapv #(m*v (transpose y) %) x))]
  (defn m*m [& more]
    {:pre  [(not (empty? more))
            (every? m? more)
            (= (map second (map shape (pop (vec more)))) (map first (map shape (rest more))))]
     :post [(m? %)
            (= (first (shape (first more))) (first (shape %)))
            (= (second (shape (last more))) (second (shape %)))]}
    (reduce m*m2 more)))
