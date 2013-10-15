(ns raycaster.core
  (:use quil.core))


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

(def map-size-x (count (get world-map 0)))
(def map-size-y (count world-map))

(def tile-size 20)

(def window-width (* map-size-x tile-size))
(def window-height (* map-size-y tile-size))


(def player {:pos [22.0 12.0]         ; x and y start position
             :dir [-1.0 0.0]           ; initial direction vector
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
                 (try
                   (-> (/ (* a a) (* b b))
                       (+ 1)
                       (Math/sqrt))
                   (catch Exception e
                     ; Division by zero, just return a large number
                     (Double/MAX_VALUE))))]
    [(delta y x) (delta x y)]))


(defn camera-x
  ; camera-x is the x-coordinate on the camera plane that the current x-coordinate of the screen represents,
  ; done this way so that the right side of the screen will get coordinate 1, the center of the screen gets
  ; coordinate 0, and the left side of the screen gets coordinate -1. Out of this, the direction of the ray
  ; can be calculated as was explained earlier: as the sum of the direction vector, and a part of the plane
  ; vector. This has to be done both for the x and y coordinate of the vector (since adding two vectors is
  ; adding their x-coordinates, and adding their y-coordinates).
  [x]
  (-> x (/ window-width) (* 2.0) (- 1.0)))


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


(defn setup []
  (smooth)
  (frame-rate 1)
  (background 0))

(defn to-screen-coord [coord]
  (* coord tile-size))


(defn draw []
  (let [[player-pos-x player-pos-y] (:pos player)
        [player-dir-x player-dir-y] (:dir player)
        [delta-x delta-y] (get-deltas player-dir-x player-dir-y)
        [map-x map-y] (map int (:pos player))
        side-dist-x (if (< 0 player-pos-x)
                        (* delta-x (- player-pos-x map-x))
                        (* delta-x (- (inc map-x) player-pos-x)))
        side-dist-y (if (< 0 player-pos-y)
                        (* delta-y (- player-pos-y map-y))
                        (* delta-y (- (inc map-y) player-pos-y)))
        ]
    (background 0)

    ; Draws the tiles
    (stroke 255)
    (stroke-weight 1)
    (doseq [x (range map-size-x)
            y (range map-size-y)]
      (let [tile (get (get world-map y) x)]
        (cond
         (= tile 0) (fill 0 0 0)
         (= tile 1) (fill 255 0 0)
         (= tile 2) (fill 0 255 0)
         (= tile 3) (fill 0 0 255)
         (= tile 4) (fill 255 255 0)
         (= tile 5) (fill 0 255 255))
        (rect (* x tile-size) (* y tile-size) tile-size tile-size)))

    ; Draws the player
    ;(fill 0 255 0)
    ;(stroke-weight 0)
    ;(let [[player-dir-x player-dir-y] (:dir player)]
    ;  (triangle player-pos-x player-pos-y
    ;            (+ player-pos-x (* player-dir-x 5) 5) (- player-pos-y (* player-dir-y 5))
    ;            (+ player-pos-x (* player-dir-x 5) -5) (- player-pos-y (* player-dir-y 5))
    ;  ))

    (stroke 255 100 0)
    (stroke-weight 1)
    (let [coords (map to-screen-coord [player-pos-x player-pos-y side-dist-x side-dist-y])]
      (println coords)
      (apply line coords))

    ;(let [
    ;      [delta-x delta-y] (get-deltas ray-dir-x ray-dir-y)
    ;      side-dist-x (if (< 0 ray-dir-x) (* delta-x (- )))]
    ;  (line player-pos-x player-pos-y
    ;        (+ player-pos-x (to-screen-coord delta-x))
    ;        (+ player-pos-y (to-screen-coord delta-y))))
    ))


(defsketch example
  :title "Raycaster"
  :setup setup
  :draw draw
  :size [window-width window-height])
