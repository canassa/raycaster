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


(defn setup []
  (smooth)                          ;;Turn on anti-aliasing
  (frame-rate 1)                    ;;Set framerate to 1 FPS
  (background 0))                 ;;Set the background colour to
                                    ;;  a nice shade of grey.
(defn draw []
  (background 0)

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
      (rect (* x tile-size) (* y tile-size) tile-size tile-size))))

(defsketch example
  :title "Raycaster"
  :setup setup
  :draw draw
  :size [window-width window-height])

;(defn -main []
;  (draw (get-buffer)))
