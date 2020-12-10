(ns sn.main
  (:require [rum.core :as rum]
            [clojure.set :as set]))


(def SIZE 15)

(def initial-state {:snake (list (int (/ (* SIZE SIZE) 2)))
                    :dir   :up
                    :dead  false
                    :food  24})
(def STATE (atom initial-state))


(defn calc-move [snake dir]
  (let [head     (first snake)
        new-head (case dir
                   :left  (dec head)
                   :right (inc head)
                   :up    (- head SIZE)
                   :down  (+ head SIZE))]
    new-head))


(defn dying? [snake dir]
  (let [head (first snake)]
    (case dir
      :left  (zero? (mod head SIZE))
      :right (zero? (mod (inc head) SIZE))
      :up    (< head SIZE)
      :down  (> (+ head SIZE)
                (* SIZE SIZE)))))


(defn spawn-food [snake]
  (let [table (into #{} (range (* SIZE SIZE)))
        free-cells (set/difference table snake)]
    (rand-nth (vec free-cells))))


(defn next-tick [{:keys [snake dir] :as state}]
  (let [is-dead   (dying? snake dir)
        new-head  (calc-move snake dir)
        food?     (= new-head (:food state))
        new-snake (cons new-head (if food?
                                   snake
                                   (butlast snake)))]
    (cond-> state
      is-dead       (assoc :dead true)
      (not is-dead) (assoc :snake new-snake)
      food?         (assoc :food (spawn-food snake)))))


(def DIRS {37 :left
           38 :up
           39 :right
           40 :down})

(def OPPOSITE {:left  :right
               :right :left
               :up    :down
               :down  :up})

(defn change-dir [e]
  (when-let [dir (get DIRS (.-keyCode e))]
    (when-not (= dir (get OPPOSITE (:dir @STATE)))
      (swap! STATE assoc :dir dir))))


(rum/defc Root < rum/reactive []
  (let [{:keys [snake food] :as state}
        (rum/react STATE)

        snake-set (into #{} snake)]
    [:div
     [:h1 "SNAKE" (when (:dead state) " X_X")]
     [:table
      (for [x (range SIZE)]
        [:tr {}
         (for [y    (range SIZE)
               :let [id (+ y (* x SIZE))
                     snake? (contains? snake-set id)]]
           [:td {:style {:width            "30px"
                         :height           "30px"
                         :background-color (when snake?
                                             "black")}
                 :id    id}
            (cond
              (and (= id food)
                   (:dead state)) "🧠"
              (= id food)         "🍏"
              :else               " ")])])]

     [:button {:on-click #(reset! STATE initial-state)}
      "RESTART"]
     [:div {} (pr-str state)]
     [:hr]]))



(defonce INT (js/setInterval #(swap! STATE next-tick) 160))
(defonce KB (js/document.addEventListener "keydown" change-dir))


(defn ^:dev/after-load start! []
  (rum/mount (Root)
    (js/document.getElementById "root")))
