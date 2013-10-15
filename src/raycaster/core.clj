(ns raycaster.core
  (:user quil.core)
  (:import [javax.swing JFrame]))

(def window-width 480)
(def window-height 480)

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

; (get-deltas 1.0 0.1)


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


;(defn get-buffer []
;  (.getBufferStrategy
;    (doto (new JFrame "Clojure")
;      (.setVisible true)
;      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
;      (.setSize (java.awt.Dimension. window-width window-height))
;      (.createBufferStrategy 2))))  ; Double buffering


(defn draw [buffer]
  (let [graphics (.getDrawGraphics buffer)
        size-x (count (first world-map))
        size-y (count world-map)
        tile-size-x (/ window-width size-x)
        tile-size-y (/ window-width size-y)]

    ; Clear background
    (.setColor graphics java.awt.Color/BLACK)
    (.fillRect graphics 0 0 window-width window-height)

    ; Draws the 2d grid
    (.setColor graphics java.awt.Color/WHITE)
    (doseq [x (range size-x)
            y (range size-y)]

      (when (= (get world-map (get world-map y) x) 1)
            (.fillRect graphics (* x size-x) (* y size-y) tile-size-x tile-size-y))

      (println (* x size-x) (* y size-y))
      (.drawRect graphics (* x size-x) (* y size-y) tile-size-x tile-size-y))

    ;(.fillOval graphics 200 200 50 50)

    ; It is best to dispose() a Graphics object when done with it.
    (.dispose graphics))

  ; Shows the contents of the backbuffer on the screen.
  (.show buffer))


(defn setup []
  (smooth)                          ;;Turn on anti-aliasing
  (frame-rate 1)                    ;;Set framerate to 1 FPS
  (background 200))                 ;;Set the background colour to
                                    ;;  a nice shade of grey.
(defn draw []
  (stroke (random 255))             ;;Set the stroke colour to a random grey
  (stroke-weight (random 10))       ;;Set the stroke thickness randomly
  (fill (random 255))               ;;Set the fill colour to a random grey

  (let [diam (random 100)           ;;Set the diameter to a value between 0 and 100
        x    (random (width))       ;;Set the x coord randomly within the sketch
        y    (random (height))]     ;;Set the y coord randomly within the sketch
    (ellipse x y diam diam)))       ;;Draw a circle at x y with the correct diameter

(defsketch example                  ;;Define a new sketch named example
  :title "Oh so many grey circles"  ;;Set the title of the sketch
  :setup setup                      ;;Specify the setup fn
  :draw draw                        ;;Specify the draw fn
  :size [323 200])

;(defn -main []
;  (draw (get-buffer)))
