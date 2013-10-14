(ns raycaster.core)

(def window-width 512)
(def window-height 384)

(def world-map [
  [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]
  [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
  [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
  [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
  [1 0 0 0 0 0 2 2 2 2 2 0 0 0 0 3 0 3 0 3 0 0 0 1]
  [1 0 0 0 0 0 2 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 1]
  [1 0 0 0 0 0 2 0 0 0 2 0 0 0 0 3 0 0 0 3 0 0 0 1]
  [1 0 0 0 0 0 2 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 1]
  [1 0 0 0 0 0 2 2 0 2 2 0 0 0 0 3 0 3 0 3 0 0 0 1]
  [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
  [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
  [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
  [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
  [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
  [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
  [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
  [1 4 4 4 4 4 4 4 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
  [1 4 0 4 0 0 0 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
  [1 4 0 0 0 0 5 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
  [1 4 0 4 0 0 0 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
  [1 4 0 4 4 4 4 4 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
  [1 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
  [1 4 4 4 4 4 4 4 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
  [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]
])


(def player {:pos [22 12]         ; x and y start position
             :dir [-1 0]          ; initial direction vector
             :plane [0.0 0.66]})  ; the 2d raycaster version of camera plane




(defn calc-side-dist [ray-dir ray-pos map-pos delta-dist]
  (if (< ray-dir 0)
      (-> map-pos (- ray-pos) (* delta-dist))
      (-> map-pos (+ 1.0) (- ray-pos) (* delta-dist))))


(defn get-deltas
  ; An explanation on the math behind this formula can be found here:
  ; http://gamedev.stackexchange.com/questions/45013/raycasting-tutorial-vector-math-question
  [x y]
  (letfn [(delta [a b]
                 (-> (/ (* a a) (* b b))
                     (+ 1)
                     (Math/sqrt)))]
    [(delta y x) (delta x y)]))


(defn camera-x
  ; camera-x is the x-coordinate on the camera plane that the current x-coordinate of the screen represents,
  ; done this way so that the right side of the screen will get coordinate 1, the center of the screen gets
  ; coordinate 0, and the left side of the screen gets coordinate -1. Out of this, the direction of the ray
  ; can be calculated as was explained earlier: as the sum of the direction vector, and a part of the plane
  ; vector. This has to be done both for the x and y coordinate of the vector (since adding two vectors is
  ; adding their x-coordinates, and adding their y-coordinates).
  [x]
  (-> x (/ window-width) (* 2) (- 1)))


(defn shoot-ray [x]
  (let [camera-x   (/ (* x 2) (dec window-width))
        ray-pos    (:pos player)
        ray-dir    (map #(* (+ %1 %2) camera-x) (:dir player) (:plane player))
        map-pos    (map int ray-pos)  ; which box of the map we're in
        delta-dist (apply map #(Math/sqrt (+ 1 (/ (* %1 %1) (* %2 %2)))) [(reverse ray-dir) ray-dir])
        step       (map #(if (< %1 0) -1 1) ray-dir)]
    (loop [side-dist (map (fn [v] (apply calc-side-dist v))
                        (map vector ray-dir ray-pos map-pos delta-dist))
           map-pos map-pos
           direction (apply < side-dist)
           axis (if direction first second)]
      (if (zero? ((world-map (second map-pos)) (first map-pos)))
        (recur (assoc side-dist axis (axis delta-dist))
               (update-in map-pos [axis] (axis step))
               direction
               axis)
        [map-pos axis]))))

(defn -main []
  (println (shoot-ray 50)))


(- window-width 1.0)


(def player {:pos [22.0 12.0]         ; x and y start position
             :dir [-1.0 0.0]          ; initial direction vector
             :plane [0.0 0.66]})

(defn valid-game? [board]
  (let [{:keys [x o] :or {x 0 o 0}} (frequencies board)]
    (< -2 (- x o) 2)))

(defn win-count [[a1 a2 a3 a4 a5 a6 a7 a8 a9]]
  (count (filter true? [(= a1 a2 a3)
                        (= a4 a5 a6)
                        (= a7 a8 a9)

                        (= a1 a4 a7)
                        (= a2 a5 a8)
                        (= a3 a6 a9)

                        (= a1 a5 a9)
                        (= a3 a5 a7)])))


(def possible
  (let [b [:x :o :.]]
     (for [a1 b a2 b a3 b
           a4 b a5 b a6 b
           a7 b a8 b a9 b] [a1 a2 a3 a4 a5 a6 a7 a8 a9])))

(def valid-games
  (filter valid-game? possible))

(count valid-games)

(let [a1 1 a2 1 a3 1
      a4 2 a5 2 a6 2
      a7 2 a8 2 a9 2]
  [(= a1 a2 a3)
   (= a4 a5 a6)
   (= a7 a8 a9)

   (= a1 a4 a7)
   (= a2 a5 a8)
   (= a3 a6 a9)

   (= a1 a5 a9)
   (= a3 a5 a7)])





possible

(count (distinct possible))

(apply + (first possible))


(for [i (range 3)]
  (for [j (range 3)]
    (for [k (range 3)]
      (for [l (range 3)]
        [i j k]))))
