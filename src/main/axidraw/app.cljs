(ns axidraw.app
  (:require
    ["react-dom/client" :as rdom]
    ["react-dom/server" :as rdom.server]
    [axidraw.util :as u]
    [helix.core :refer [defnc $]]
    [helix.dom :as d]
    [helix.hooks :as hooks]
    [reagent.core :as reagent]
    [svg-clj.composites :as c]
    [svg-clj.elements :as el]
    [svg-clj.layout :as l]
    [svg-clj.parametric :as par]
    [svg-clj.path :as p]
    [svg-clj.transforms :as tf]))


(defn create-rule
  [r]
  (let [bits (map parse-double (vec r))]
    (zipmap [[0 0 0]
             [0 0 1]
             [0 1 0]
             [0 1 1]
             [1 0 0]
             [1 0 1]
             [1 1 0]
             [1 1 1]]
            bits)))


(defn step
  [rule state]
  (->> (conj (take 2 state) (last state))
       (conj (partition 3 1 state state))
       (map rule)))


(defn ca
  [init-state rule n]
  (take n (iterate (partial step rule) init-state)))


(defn one-hot
  [n]
  (if (odd? n)
    (concat (repeat (/ (dec n) 2) 0)
            [1]
            (repeat (/ (dec n) 2) 0))
    (concat (repeat (dec (/ n 2)) 0)
            [1]
            (repeat (/ n 2) 0))))


(defn create-rep
  [x3 r s i init-state]
  (let [res (ca (map parse-double (vec init-state))
                (create-rule r)
                i)]
    (->> res
         flatten
         (map (fn [v]
                (-> (el/rect 10 x3)
                    (tf/style {:fill (if (zero? v)
                                       "none"
                                       "black")
                               :stroke (if (zero? v)
                                         "none"
                                         "black")
                               :stroke-width (if (zero? v)
                                               "1px"
                                               "3px")})))))))


(defn rect-grid
  "Build a rectilinear grid with `nx`, `ny` points in x and y directions, with `x-spacing` and `y-spacing` between each point in the x and y directions respectively.
  Returned as a flat list of points from [0 0] in a 'Z' pattern to [(* nx x-spacing) (* ny y-spacing)]."
  [nx ny x-spacing y-spacing]
  (for [b (range ny)
        a (range nx)]
    [(* a x-spacing) (* b y-spacing)]))


(defn actual-svg
  [x1 x2 x3 xd yd r s i init-state]
  (->
    (l/distribute-on-pts
      (create-rep x1 r s i init-state)
      (rect-grid s i xd yd))
    (tf/translate [x2 x3])))


(defn border
  []
  (-> (el/rect u/A3_WIDTH u/A3_HEIGHT)
      (tf/style u/black-line-style)
      (tf/translate u/A3_CENTER)))


(defn my-svg
  [{:keys [x1 x2 x3 xd yd r s i init-state]}]
  (actual-svg x1 x2 x3 xd yd r s i init-state))


(defnc svg-web-view
  [data]
  (-> (my-svg data)
      (el/g (border))
      (c/svg u/DISPLAY_WIDTH u/DISPLAY_HEIGHT)
      (update 1 #(assoc % :viewBox (str "0 0 " u/A3_WIDTH " " u/A3_HEIGHT)))
      (reagent/as-element)))


(defnc controller
  [{:keys [state set-state vals]}]
  (d/div
    (for [v vals]
      (d/div
        {:style {:margin "20px 0px"}}
        (d/p
          {:style {:margin 0}}
          (str (name (:var v)) ": "
               (get state (:var v))))
        (d/input
          {:value (get state (:var v))
           :type "range"
           :min (:min v)
           :max (:max v)
           :step (:step v)
           :on-change #(set-state assoc (:var v) (int (.. % -target -value)))})))))


(defnc app []
  (let [[state set-state] (hooks/use-state {:filename "test.svg"
                                            :x1 1 :x2 29 :x3 46
                                            :xd 12.5 :yd 12.5
                                            :s 20 :i 28
                                            :rule "01011010"
                                            :init-state ""})]
    (d/div {:style {:display "flex"
                    :flex-direction "row"}}
           (d/div {:style {:display "flex"
                           :flex-direction "column"}}
                  (d/h1 "Hello world")
                  (d/input {:value (:filename state)
                            :on-change #(set-state assoc :filename (.. % -target -value))})
                  (d/button {:on-click #(-> (my-svg state)
                                            (c/svg u/A3_WIDTH u/A3_HEIGHT)
                                            (reagent/as-element)
                                            (rdom.server/renderToString)
                                            (u/download-svg (:filename state)))} "Download")
                  (d/p "rule:")
                  (d/input {:value (:rule state)
                            :on-change #(set-state assoc :rule (.. % -target -value))})
                  (d/p "state:")
                  (d/input {:value (:init-state state)
                            :on-change #(set-state assoc :init-state (.. % -target -value))})
                  ($ controller {:state state
                                 :set-state set-state
                                 :vals [{:var :x1
                                         :min 0
                                         :max 15
                                         :step 1}
                                        {:var :x2
                                         :min 0
                                         :max 500
                                         :step 1}
                                        {:var :x3
                                         :min 0
                                         :max 500
                                         :step 1}
                                        {:var :xd
                                         :min 0
                                         :max 50
                                         :step 1}
                                        {:var :yd
                                         :min 0
                                         :max 50
                                         :step 1}
                                        {:var :s
                                         :min 1
                                         :max 50
                                         :step 1}
                                        {:var :i
                                         :min 1
                                         :max 50
                                         :step 1}]}))
           (d/div {:style {:width "100%"
                           :display "flex"
                           :align-items "center"
                           :justify-content "center"}}
                  ($ svg-web-view {:x1 (:x1 state)
                                   :x2 (:x2 state)
                                   :x3 (:x3 state)
                                   :xd (:xd state)
                                   :yd (:yd state)
                                   :r (:rule state)
                                   :init-state (:init-state state)
                                   :s (:s state)
                                   :i (:i state)})))))


(defonce root (rdom/createRoot (js/document.getElementById "root")))


(defn init
  []
  (.render root ($ app)))
