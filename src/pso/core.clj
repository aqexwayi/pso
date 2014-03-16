(ns pso.core
  )

;;  particle : { position velocity }
;;  swarm : { [particles] best-particle }
;;  context : { [swarms] best-swarm parameters }

(def default-parameter
  {:dim 2
   :c1 2.0
   :c2 2.0
   :vmax 0.1
   :swarm-volume 10
   :swarm-count 100
   :fitness-f (constantly 0)
   :inertia 1.0
   :global-inc 0.9
   :particle-inc 0.9
   })

(defn gen-vector [dim]
  (vec (take dim (repeatedly #(rand)))))

(defn gen-particle [dim]
  {:position (gen-vector dim)
   :velocity (mapv #(* % 0.1) (gen-vector dim))})

(defn index-of-best-particle [ps fitness-f]
  (first (apply min-key 
                (fn [[idx p]]
                  (fitness-f (:position p)))
                (map-indexed vector ps))))

(defn index-of-best-swarm [ss fitness-f]
  (first (apply min-key 
                (fn [[idx s]]
                  (let [p (nth (:particles s) (:best s))]
                    (fitness-f (:position p))))
                (map-indexed vector ss))))

(defn gen-swarm [dim swarm-volume fitness-f]
  (let [ps (vec (take swarm-volume (repeatedly #(gen-particle dim))))]
    {:particles ps 
     :best (index-of-best-particle ps fitness-f)}))
        
(defn pso-init [parameters]
  (let [para (merge default-parameter parameters)
        dim (:dim para)
        fitness-f (:fitness-f para)
        swarm-count (:swarm-count para)
        swarm-volume (:swarm-volume para)
        ss (vec (take swarm-count 
                      (repeatedly #(gen-swarm dim swarm-volume fitness-f))))
        gbi (index-of-best-swarm ss fitness-f)]
    {:swarms ss
     :best gbi
     :parameter para
     }))

(defn limit-value [v l h]
  (cond
   (< v l) l
   (> v h) h
   :else v))

(defn limit-vector [v l h]
  (mapv #(limit-value % l h) v))

(defn update-particle [par pbest pgbest para]
  (let [p0 (:position par)
        pb (:position pbest)
        pg (:position pgbest)
        v (:velocity par)
        c0 (:inertia para)
        c1 (* (:particle-inc para) (rand))
        c2 (* (:global-inc para) (rand))
        nv (mapv +
                 (mapv #(* c0 %) v)
                 (mapv #(* c1 %) (mapv - pb p0))
                 (mapv #(* c2 %) (mapv - pg p0)))
        np (mapv + p0 nv)
        ]
    {:position (limit-vector np 0.0 1.0)
     :velocity (limit-vector nv 0.0 1.0)}))

(defn update-swarm [swarm pgbest para]
  (let [ps (:particles swarm)
        pb (nth ps (:best swarm))
        ps-new (mapv #(update-particle % pb pgbest para) ps)
        ]
    {:particles ps-new
     :best (index-of-best-particle ps-new (:fitness-f para))}))

(defn pso-step [{ss :swarms gbi :best para :parameter}]
  (let [s-best (nth ss gbi)
        pgbest (nth (:particles s-best) (:best s-best))
        ss-new (mapv #(update-swarm % pgbest para) ss)
        ]
    {:swarms ss-new
     :best (index-of-best-swarm ss-new (:fitness-f para))
     :parameter para}))

(defn pso-solve [parameters]
  (loop [ctx (pso-init parameters)
         count 100]
    (if (zero? count)
      (let [best-swarm (nth (:swarms ctx) (:best ctx))
            best-particle (nth (:particles best-swarm) (:best best-swarm))]
        best-particle)
      (recur (pso-step ctx) (dec count)))))

(defn f-test [[x y]]
  (+ (* (- x 0.5) (- x 0.5))
     (* (- y 0.6) (- y 0.6))))

(def para-test
  {:dim 2
   :swarm-volume 2
   :swarm-count 2
   :fitness-f f-test})


(def para-test2
  {:dim 2
   :swarm-volume 10
   :swarm-count 10
   :fitness-f f-test})

