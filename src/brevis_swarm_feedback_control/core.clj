#_"This file is a Brevis contribution.                                                                                                                                       
                                                                                                                                                                                     
    brevis is free software: you can redistribute it and/or modify                                                                                                           
    it under the terms of the GNU General Public License as published by                                                                                                             
    the Free Software Foundation, either version 3 of the License, or                                                                                                                
    (at your option) any later version.                                                                                                                                              
                                                                                                                                                                                     
    brevis is distributed in the hope that it will be useful,                                                                                                                
    but WITHOUT ANY WARRANTY; without even the implied warranty of                                                                                                                   
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                                                                                                                    
    GNU General Public License for more details.                                                                                                                                     
                                                                                                                                                                                     
    You should have received a copy of the GNU General Public License                                                                                                                
    along with brevis.  If not, see <http://www.gnu.org/licenses/>.                                                                                                          
                                                                                                                                                                                     
Copyright 2012, 2014 Kyle Harrington"

(ns brevis-swarm-feedback-control.core
  (:gen-class)
  (:use [brevis.graphics.basic-3D]
        [brevis.physics collision core space utils]
        [brevis.shape box sphere cone]
        [brevis core osd vector random input globals display
         parameters math]
        [clojure.set])
  (:require [clojure.string :as string]))

; If you base research upon this simulation, please reference the following paper:
;
; Gold, J., A. Wang, and K. Harrington, (2014) "Feedback Control of Evolving Swarms". In Proceedings of Artificial Life XIV, pp. 884-891.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ## Globals

(reset! params {:num-birds 25
                :initial-bird-energy 1
                :delta-bird-energy 0.0025
                :delta-collision 0.00001
                :delta-consumption 0.00005
                :num-foods 25
                :initial-food-energy 1
                :delta-food-energy 0.001
                :max-acceleration 1.0
                :force-strength 0.1
                :force-increment 0.1
                :force-width 0.1
                :tile-radius 107
                :max-force 10
                :pid-mag 0.1
                :p-constant 1
                :i-constant 0.01
                :log-interval 25
                :dt 0.5
                :food-position-type :uniform
                :environment-type :uniform
                :final-environment-type :uniform
                :gui true
                :output-directory ""
                :variation-trigger 20000 ;; if this is a number, then this is a time threshold. otherwise it should be a predicate (some function testable for true/false)
                :terminate-trigger 40000 ;; if this is a number, then this is a time threshold. otherwise it should be a predicate (some function testable for true/false)
                :start-time (System/nanoTime)
                :screenshot-interval -1
                })

; Note: there was an issue with dt in the initial version of this simulation where the physics and energy dts were treated differently
                   
(def screenshot-num (atom 0))

;; Delta variables are multiplied by dt
#_(def num-birds (atom 50))
#_(def initial-bird-energy (atom 1))
#_(def delta-bird-energy (atom 0.25));; cost of living
#_(def delta-collision (atom 0.001))
#_(def delta-consumption (atom 0.005))
(def dead-birds (atom 0))
(def accumulated-energy (atom (float 0)))
(def triggered (atom false))

#_(def num-foods 25)
#_(def initial-food-energy (atom 5))
#_(def delta-food-energy (atom 0.1));; food regenerates

(def speed 25)
#_(def max-acceleration 1.0)

(def floor (atom nil))

;(def width (Math/pow num-birds 0.95))
(def width 400)
(def height width)



(def tile-w 8) 
(def tile-h 8)

#_(def force-strength 0.1)
#_(def force-increment 0.1);; granularity of control
#_(def force-width 0.1)

#_(def tile-radius 107)
#_(def pid-mag 0)
#_(def p-constant 1); proportional constant for PID
#_(def i-constant 0.01); integral constant for PID
(def max-windup 
  (if (not (or (= (:i-constant @params) 0) (= (:pid-mag @params) 0)))
  (/ 1 (* (:i-constant @params) (:pid-mag @params)))
  0));

#_(def log-interval 25); interval between saving logs to text file

#_(def food-position-type (atom :uniform)); :uniform, :gaussian

#_(def log-filename (atom (str "brevisHCISwarm_" (System/nanoTime) "_tile-radius_" (:tile-radius @params) "_pid-mag_" (:pid-mag @params) "_i-constant_" (:i-constant @params) ".txt")))

(defn log-filename
  []
  (string/replace 
    (str (:output-directory @params) 
         "brevisHCISwarm_" (:start-time @params) 
         "_tile-radius_" (:tile-radius @params) 
         "_pid-mag_" (:pid-mag @params) 
         "_i-constant_" (:i-constant @params) 
         "_num-foods_" (:num-foods @params) 
         "_deltabirdenergy_" (:delta-bird-energy @params)
         "_deltacollision_" (:delta-collision @params)
         "_deltaconsumption_" (:delta-consumption @params)
         ".txt")
    ":" ""))


(let [min-x (- width) #_(- (/ width 2))
      max-x width #_(/ width 2)
      min-z (- height) #_(- (/ height 2))
      max-z height #_(/ height 2)
      min-y -10
      max-y 200]
  (defn out-of-bounds
    "Test if a position is out of bounds."
    [p]
    (or (> (.x p) max-x)
        (< (.x p) min-x)
        (> (.z p) max-z)
        (< (.z p) min-z)
        (> (.y p) max-y)
        (< (.y p) min-y))))

(defn log-string
  "Log a string to the current log filename."
  [s]
  (spit (log-filename) s :append true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ## Foods

(def food-radius 5)

(defn food?
  "Is a thing food?"
  [thing]
  (= (get-type thing) :food))

(defn random-food-position
  "Return a random valid location for food."
  []
  (cond (= (:food-position-type @params) :uniform)
        (vec3 (- (rand width) (/ width 2) (- (/ food-radius 2))) 
              (+ 45 (rand 10))
              (- (rand height) (/ height 2) (- (/ food-radius 2))))
        (= (:food-position-type @params) :circle)
        (let [t (* 2 Math/PI (lrand (/ width 2)))
              u (+ (lrand (/ width 2)) (lrand (/ width 2)))
              r (if (> u (/ width 2)) (- width u) u)]
          (vec3 (* r (Math/cos t)) 
                (+ 45 (rand 10))
                (* r (Math/sin t))))))
    
(defn make-food
  "Make a food entity."
  [position]
  (move (assoc (make-real {:type :food
                    :color (vec4 0 1 1 1)
                    :shape (create-sphere food-radius)})
               :energy (:initial-food-energy @params))
        position))

(defn random-food
  "Make a food at a random position."
  []
  (make-food (random-food-position)))

(defn update-food
  "Update a food item."
  [food]
  (if (> (:energy food) (* (get-dt) (:delta-consumption @params)))
    (assoc (set-color food
                      (vec4 (* 1 (:energy food)) 0 1 1))
           :energy (+ (:energy food) (* (get-dt) (:delta-food-energy @params))))
    (move (assoc (set-color food
                            (vec4 (* 10 (:energy food)) 0 1 1))
                 :energy (:initial-food-energy @params))
          (random-food-position))))
(add-update-handler :food update-food)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tiles

(defn bird?
  "Is a thing a bird?"
  [thing]
  (= (get-type thing) :bird))

(defn tile?
  "Is a thing a tile?"
  [thing]
  (= (get-type thing) :tile)) 

(defn tile-color
   "Figure out the color of a tile based on its properties and the currently selected tile."
   ([tile]
     (tile-color tile (:current-tile @floor)))
   ([tile current-pos]
     (if (map? tile)
       (vec4 0
             (if (pos? (get tile :force)) (get tile :force) 0);; positive is green
             (if (neg? (get tile :force)) (- (get tile :force)) 0);; negative is blue              
             0.5); not using alpha
       (vec4 0
             0 
             0 0.5))))

(defn bird-set-point
  [food-count]
  (* food-count (/ (:num-birds @params) (:num-foods @params))))

(defn coerce
  [val min max]
  (if (> val min) (if (< val max) val max) min))

(defn update-tile
  "Update a tile."
  [tile]
  (if (not (= (:pid-mag @params) 0))
  (let [nbrs (get-neighbor-objects tile)
        nbr-tiles #_(filter tile? nbrs) (vals (:tiles @floor))
        nbr-birds (filter bird? nbrs)
         ;bird-count (count nbr-birds)
         bird-dists (map (fn [bird] (Math/abs (length (vec3 (.x (sub (get-position bird) (get-position tile))) 0 (.z (sub (get-position bird) (get-position tile))) ) ))) nbr-birds)
         bird-count (count (filter (fn [dist] (< dist (:tile-radius @params))) bird-dists))
        
        ;bird-ones (when-not (empty? nbr-birds) 
         ;          (doall (for [bird nbr-birds :while (< (Math/abs (length (sub (get-position bird) (get-position tile)))) tile-radius)]
         ;                      1)))
         ;bird-count (if (empty? nbr-birds) 0 (reduce + bird-ones))
         nbr-foods (filter food? nbrs)
         ;food-count (count nbr-foods)
         food-dists (map (fn [food] (Math/abs (length (vec3 (.x (sub (get-position food) (get-position tile))) 0 (.z (sub (get-position food) (get-position tile))) )))) nbr-foods)
         food-count (count (filter (fn [dist] (< dist (:tile-radius @params))) food-dists))
        ;food-ones (when-not (empty? nbr-foods) 
         ;          (doall (for [food nbr-foods :while (< (Math/abs (length (sub (get-position food) (get-position tile)))) tile-radius)]
         ;                      1)))
         ;food-count (if (empty? nbr-foods) 0 (reduce + food-ones))
         ts (:tiles @floor)]
    
        (swap! floor assoc :tiles (assoc ts (:tile-pos tile) (assoc (get ts (:tile-pos tile)) :food-count food-count)))
        (swap! floor assoc :tiles (assoc ts (:tile-pos tile) (assoc (get ts (:tile-pos tile)) :bird-count bird-count)))
        (swap! floor assoc :tiles (assoc ts (:tile-pos tile) (assoc (get ts (:tile-pos tile)) :error (- (bird-set-point (:food-count tile)) (:bird-count tile)))))
        (swap! floor assoc :tiles (assoc ts (:tile-pos tile) (assoc (get ts (:tile-pos tile)) :cumulative-error (coerce (+ (:cumulative-error tile) (* (get-dt) (:error tile))) (- 0 max-windup) max-windup))))
        (swap! floor assoc :tiles (assoc ts (:tile-pos tile) (assoc (get ts (:tile-pos tile)) :force (coerce (* (+ (* (:p-constant @params) (:error tile)) (* (:i-constant @params) (:cumulative-error tile))) (:pid-mag @params)) (- (:max-force @params)) (:max-force @params)))))
        ;(println (map (fn [bird] (length (sub (get-position bird) (get-position tile)))) nbr-birds))
        ))
  (set-color tile (tile-color tile))) 

(add-update-handler :tile update-tile)

(defn tile-up
  "Select the tile above."
  []
  (let [ct (:current-tile @floor)]
    (log-string (str "USER " ct " UP\n")) 
    (swap! floor assoc
           :current-tile [(first ct) (mod (dec (second ct)) tile-h)])))

(defn tile-down
  "Select the tile above."
  []
  (let [ct (:current-tile @floor)]
    (log-string (str "USER " ct " DOWN\n")) 
    (swap! floor assoc
           :current-tile [(first ct) (mod (inc (second ct)) tile-h)])))

(defn tile-left
  "Select the tile above."
  []
  (let [ct (:current-tile @floor)]
    (log-string (str "USER " ct " LEFT\n")) 
    (swap! floor assoc
           :current-tile [(mod (dec (first ct)) tile-w) (second ct)])))

(defn tile-right
  "Select the tile above."
  []
  (let [ct (:current-tile @floor)]
    (log-string (str "USER " ct " RIGHT\n")) 
    (swap! floor assoc
           :current-tile [(mod (inc (first ct)) tile-w) (second ct)])))

(defn tile-inc-force
  "Make a tile more attractive."
  []
  (let [ct (:current-tile @floor)
        ts (:tiles @floor)
        t (get ts ct)
        f (get t :force)]
    (log-string (str "USER " ct " INC " f "\n")) 
    (swap! floor assoc
           :tiles (assoc ts
                         ct (assoc t :force (min 1 (+ f (:force-increment @params))))))))

(defn tile-dec-force
  "Make a tile less attractive."
  []
  (let [ct (:current-tile @floor)
        ts (:tiles @floor)
        t (get ts ct)
        f (get t :force)]
    (log-string (str "USER " ct " DEC " f "\n")) 
    (swap! floor assoc
           :tiles (assoc ts
                         ct (assoc t :force (max -1 (- f (:force-increment @params))))))))

(defn tile-zero-force
  "Make a tile neutral."
  []
  (let [ct (:current-tile @floor)
        ts (:tiles @floor)
        t (get ts ct)
        f (get t :force)]
    (log-string (str "USER " ct " NEUTRAL\n")) 
    (swap! floor assoc
           :tiles (assoc ts
                         ct (assoc t :force 0)))))

(defn all-tiles-zero-force
  "Make a tile neutral."
  []
  (log-string "USER ALL-TILES-NEUTRAL\n")
  (let [ts (:tiles @floor)]
    (swap! floor assoc
           :tiles (zipmap (keys ts)
                          (map #(assoc % :force 0) (vals ts))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ## Birds



(defn random-bird-position
  "Returns a random valid bird position."
  []
  (vec3 (- (rand width) (/ width 2)) 
        (+ 59.5 (rand 10));; having this bigger than the neighbor radius will help with speed due to neighborhood computation
        (- (rand height) (/ height 2))))

(defn random-genome
  "Make a random bird genome."
  []
  {:neighborC (- (lrand 2) 1);; Neighbor coefficient for close behavior
   :neighborF (- (lrand 2) 1);; Neighbor coefficient for far behavior
   :neighborD (lrand 25);; Neighbor distance
   :foodC (- (lrand 2) 1);; Food coefficient for close behavior
   :foodF (- (lrand 2) 1);; Food coefficient for far behavior
   :foodD (lrand 25);; food distance
   })

(defn mutate-genome
  "Mutate a bird's genome."
  [genome]
  (let [mut-step 0.5]
    (into {}
          (for [[k v] genome]
            [k (* (+ 1 (lrand (* 2 mut-step)) (- mut-step)) v)]))))

(defn make-bird
  "Make a new bird with the specified program. At the specified location."
  [position]  
  (assoc (move (make-real {:type :bird
                           :color (vec4 (lrand) (lrand) (lrand) 1)
                           ;:color (vec4 1 0 0 1)
                           ;:shape (create-sphere)})
                           :shape (create-cone 2.2 1.5)})
               position)
         :birth-time (get-time)
         :energy (:initial-bird-energy @params)
         :genome (random-genome)))
  
(defn random-bird
  "Make a new random bird."
  []
  (make-bird (random-bird-position)))    

(defn bound-acceleration
  "Keeps the acceleration within a reasonable range."
  [v]  
  (if (> (length v) (:max-acceleration @params))
    (mul (div v (length v)) (:max-acceleration @params))
    v))

(defn fly
  "Change the acceleration of a bird."
  [bird]
  (let [nbrs (get-neighbor-objects bird)
        nbr-tiles #_(filter tile? nbrs) (vals (:tiles @floor))
        nbr-birds (filter bird? nbrs)
        nbr-foods (filter food? nbrs)
        closest-bird (when-not (empty? nbr-birds) (first nbr-birds))        
        closest-food (when-not (empty? nbr-foods) (first nbr-foods))        
        bird-pos (get-position bird)
        dclosest-bird (when closest-bird (sub (get-position closest-bird) bird-pos))
        dclosest-food (when closest-food (sub (get-position closest-food) bird-pos))
        tile-forces (doall (for [tile nbr-tiles]
                             (let [dtile (sub (get-position tile) bird-pos)
                                   dtile (vec3 (.x dtile) 0 (.z dtile));; we dont care about height difference 
                                   d (length dtile)
                                   r (+ 0.1 (Math/abs d))]
                               (mul (if (zero? d) dtile (div dtile d))
                                    (if (zero? r) 0
                                      (* (:force-strength @params) (get tile :force) (Math/pow 10 
                                                                                               (/ (:force-width @params) r))))))))
        tile-force (reduce add tile-forces)
        new-acceleration (add (if closest-bird
                                (if (< (length dclosest-bird) (:neighborD (:genome bird)))
                                  (mul dclosest-bird (:neighborC (:genome bird)))
                                  (mul dclosest-bird (:neighborF (:genome bird))))
                                (vec3 0 0 0))
                              (if closest-food
                                (if (< (length dclosest-food) (:foodD (:genome bird)))
                                  (mul dclosest-food (:foodC (:genome bird)))
                                  (mul dclosest-food (:foodF (:genome bird))))
                                (vec3 0 0 0))
                              tile-force)
        ;; speed independent of distance
        #_(add (if (and closest-bird (not (zero? (length dclosest-bird))))
                (if (< (length dclosest-bird) (:neighborD (:genome bird)))
                  (mul dclosest-bird (/ (:neighborC (:genome bird)) (length dclosest-bird)))
                  (mul dclosest-bird (/ (:neighborF (:genome bird)) (length dclosest-bird))))
                (vec3 0 0 0))
              (if (and closest-food (not (zero? (length dclosest-food))))
                (if (< (length dclosest-food) (:foodD (:genome bird)))
                  (mul dclosest-food (/ (:foodC (:genome bird)) (length dclosest-food)))
                  (mul dclosest-food (/ (:foodF (:genome bird)) (length dclosest-food))))
                (vec3 0 0 0)))]
    #_(println "Tile-force:" tile-force)
    (if (or (< (:energy bird) 0) (out-of-bounds bird-pos))
      (do (swap! dead-birds inc)
        (move (assoc bird
                     :birth-time (get-time)
                     :energy (:initial-bird-energy @params)
                     :genome (mutate-genome (:genome (lrand-nth (filter bird? (all-objects))))))
              (random-bird-position)))
      (assoc (set-acceleration
                bird
                #_(if (out-of-bounds (get-position bird))                      
                   (move bird (vec3 0 25 0))
                   bird)
                (bound-acceleration
                  new-acceleration
                  #_(add (mul (get-acceleration bird) 0.5)
                       (mul new-acceleration speed))))
              :energy (- (:energy bird) (* (get-dt) (:delta-bird-energy @params))))
      #_(assoc (set-acceleration
                bird
                #_(if (out-of-bounds (get-position bird))                      
                   (move bird (vec3 0 25 0))
                   bird)
                (bound-acceleration
                  new-acceleration
                  #_(add (mul (get-acceleration bird) 0.5)
                       (mul new-acceleration speed))))
              :energy (- (:energy bird) (* (get-dt) @delta-bird-energy))))))

(enable-kinematics-update :bird); This tells the simulator to move our objects
(add-update-handler :bird fly); This tells the simulator how to update these objects

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ## Global updates   

(let [log-counter (atom 0)]
  (add-global-update-handler 1 
                              (fn [] (let [objs (all-objects)
                                           foods (filter food? objs)
                                           birds (filter bird? objs)
                                           average-age (/ (apply + (map #(- (get-time) (get % :birth-time)) birds))
                                                          (count birds))
                                           avg-food-energy (/ (apply + (map :energy foods)) (count foods))
                                           avg-bird-energy (/ (apply + (map :energy birds)) (count birds))
                                           all-genetics (map #(list (:neighborC %)
                                                                    (:neighborF %)
                                                                    (:neighborD %)
                                                                    (:foodC %)
                                                                    (:foodF %)
                                                                    (:foodD %)) (map :genome birds))
                                           agg-genetics (apply concat (for [k (range 6)]
                                                                        (let [genes (map #(nth % k) all-genetics)]
                                                                          [(/ (apply + genes) (count birds))
                                                                           (std-dev genes)])))
                                           ;current-tile (:current-tile @floor)
                                           ;force-at-position (get (get (:tiles @floor) current-tile) :force) 
                                           ]
                                       (when (zero? (mod @log-counter (:log-interval @params)))
                                         (log-string (str "" (string/join "\t" 
                                                                          (concat [(get-time) avg-bird-energy
                                                                                   avg-food-energy
                                                                                   @dead-birds
                                                                                   @accumulated-energy
                                                                                   average-age]
                                                                                  agg-genetics
                                                                                  [(count foods)])) "\n")))
                                       (swap! log-counter inc))
                                
                                
                                       
                                       #_(.setContentPane (:frame @ui) panel)
                                       #_(add-mig-items (:panel @ui) items)
                                       #_(sc/repaint! (:panel @ui))
                                       #_(sc/repaint! (:frame @ui)))))

(add-global-update-handler 2
                           (fn [] (let [objs (all-objects)
                                        foods (filter food? objs)]                                    
                                    ;; terminate simulation if need be
                                    (when (and (number? (:terminate-trigger @params)) 
                                               (> (get-time) (:terminate-trigger @params)))
                                      (swap! *gui-state* assoc :close-requested true))
                                    ;; switch environment types if need be
                                    (cond (and (not @triggered) 
                                               (number? (:variation-trigger @params)) 
                                               (> (get-time) (:variation-trigger @params)))
                                          (do (reset! triggered true)
                                            (swap! params assoc :environment-type (:final-environment-type @params)))))))

; Take periodic screenshots
#_(add-global-update-handler 3
                            (fn [] 
                              (when (and (pos? (:screenshot-interval @params))
                                         (> (get-time) (* (:screenshot-interval @params) @screenshot-num)))
                                (screenshot (str "feedbackSwarmEvolve_t_" (get-time) ".png"))
                                (swap! screenshot-num inc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ## Collision handling
;;
;; Collision functions take [collider collidee] and return [collider collidee]
;;   Both can be modified; however, two independent collisions are actually computed [a b] and [b a]

(defn land
  "Collision between a bird and the floor."
  [bird floor]
  [(set-velocity (set-acceleration bird (vec3 0 0.5 0)) (vec3 0 0.5 0));; maybe move as well       
   floor])

(add-collision-handler :bird :floor land)

;; A bird eats
(add-collision-handler :bird :food 
                       (fn [bird food]
                         (swap! accumulated-energy #(+ % (:delta-consumption @params)))
                         [(assoc bird :energy (+ (:energy bird) (:delta-consumption @params)))]))

;; A food is eaten
(add-collision-handler :food :bird 
                       (fn [food bird]
                         [(assoc food
                                 :energy (- (:energy food) (:delta-consumption @params))) bird]))

(defn bump
 "Collision between two birds. This is called on [bird1 bird2] and [bird2 bird1] independently
so we only modify bird1."
 [bird1 bird2]  
 [(assoc bird1 :energy (- (:energy bird1) (:delta-collision @params)))
  bird2])
(add-collision-handler :bird :bird bump)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ## brevis control code

(defn make-tile-floor
  "Make a floor object."
  [w h]    
  (let [tile-w 8
        tile-h 8
        tile-scale-w (/ w tile-w)
        tile-scale-h (/ h tile-h)
        tile-positions (for [x (range tile-w) y (range tile-h)]
                         [x y])
        current-tile-pos [(int (Math/ceil (/ tile-w 2))) (int (Math/ceil (/ tile-h 2)))]
        tiles (apply hash-map
                     (apply concat 
                            (for [[x y] tile-positions]
                              (let [tile-pos #_(vec3 (* x tile-scale-w) 0 (* y tile-scale-h))
                                    (vec3 (* (- x (/ tile-w 2) -0.5) tile-scale-w) 0 (* (- y (/ tile-h 2) -0.5) tile-scale-h))]
                                #_(println (tile-color [x y] current-tile-pos) [x y] current-tile-pos tile-pos tile-scale-w tile-scale-h)
                                [[x y]
                                 (assoc (move (make-real {:color (tile-color [x y] current-tile-pos)
                                                          :shininess 80
                                                          :type :tile
                                                          :density 8050
                                                          :hasShadow false
                                                          :shape (create-box tile-scale-w 0.1 tile-scale-h)
                                                          #_(create-box (/ tile-scale-w 2) 0.1 (/ tile-scale-h 2))})
                                              tile-pos)
                                        :tile-pos [x y]
                                        :force 0
                                        :food-count 0
                                        :bird-count 0
                                        :error 0
                                        :cumulative-error 0)]))))]
    (doseq [[tile-id tile] tiles]
      (add-object tile))
    {:current-tile current-tile-pos
     :tiles tiles}))

(defn initialize-simulation
  "This is the function where you add everything to the world."
  []  
  (init-world)
  (init-view)
  (swap! brevis.globals/*gui-state* assoc :gui (:gui @params))
  
  (.setPosition (:camera @brevis.globals/*gui-state*) (vec3 0.0 -385 0))
  (.setRotation (:camera @brevis.globals/*gui-state*) (vec4 90 0 -90 0))
  
  #_(set-dt 0.01)
  (set-dt (:dt @params))
  (set-neighborhood-radius 200)
  (default-display-text)
  (move-light 1 [1 0 0 1])
  (reset! floor (make-tile-floor width height))
  #_(add-object @floor)
  (dotimes [_ (:num-foods @params)]
    (add-object (random-food)))
  (dotimes [_ (:num-birds @params)]
    (add-object (random-bird))))


(defn -main [& args]       
  (let [;; First put everything into a map                                                                                                                                                                                                                                                                                 
        argmap (apply hash-map
                      (mapcat #(vector (read-string (first %)) (second %) #_(read-string (second %)))
                              (partition 2 args)))
        ;; Then read-string on *some* args, but ignore others                                                                                                                                                                                                                                                              
        argmap (apply hash-map
                      (apply concat
                             (for [[k v] argmap]
                               [k (cond (= k :output-directory) v
                                        :else (read-string v))])))
        random-seed (if (:random-seed argmap)
                      (byte-array (map byte (read-string (:random-seed argmap)))) 
                      (generate-random-seed))
        arg-params (merge @params argmap)
        rng (make-RNG random-seed)]    
    (println argmap)
    (println arg-params)
    (reset! params arg-params)
    
    (log-string (str "random-seed = " (random-seed-to-string random-seed) "\n"))
    (doseq [[k v] @params]
      (log-string (str k "\t" v "\n")))

    (with-rng rng
      ((if (:gui @params) start-gui start-nogui) 
        initialize-simulation java-update-world))))

;; For autostart with Counterclockwise in Eclipse
(when (find-ns 'ccw.complete)
  (-main))
