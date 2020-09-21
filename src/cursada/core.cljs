(ns ^:figwheel-hooks cursada.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [cljs-http.client :as http]
   [cljs.core.async :refer [<!]]))

(defn combination [n [m & ms :as mms]]
  (cond
    (<= n 1)            (map vector mms)
    (<= n (count mms)) (lazy-seq (concat (map #(into [m] %)
                                              (combination (dec n) ms))
                                         (combination n ms)))))
(defn explode [[m & ms :as mms]]
  (if ms
    (let [tail (explode ms)]
      (for [a m
            t tail]
        (cons a t)))
    (map vector m)))

(defn toMinutes [s]
  (let [[h m] (clojure.string/split s #":")]
    (+ (* (js/parseInt h) 60)
       (js/parseInt m))))

(defn overlap-hours [mat1 mat2]
  (for [{dia1 :dia
         ini1 :inicio
         fin1 :fin} (:dias mat1)
        {dia2 :dia
         ini2 :inicio
         fin2 :fin} (:dias mat2)]
    (if (and (= dia1
                dia2)
             (or  (< ini1 ini2 fin1)
                  (< ini1 fin2 fin1)))
      (- (min (toMinutes fin1)
              (toMinutes fin2))
         (max (toMinutes ini1)
              (toMinutes ini2)))
      0)))

(defn overlap-total [ms]
  (->> ms
       (combination 2)
       (map (fn [[m1 m2]]
              (reduce + (overlap-hours m1 m2))))
       (reduce +)))

(defn schedule [n o materias]
  (->> (for [combo  (combination n materias)]
         (->> combo
              (map (fn [{:keys [materia comisiones]}]
                     (map #(assoc % :materia materia)
                          comisiones)))
              (explode)))
       (apply concat)
       (map (fn [combo]
              {:overlap (overlap-total combo)
               :combination combo}))
       (sort-by :overlap)
       (remove (comp #(> % o) :overlap))))

(defonce app-state (atom nil))

(when (nil? @app-state)
  (go (let [response (<! (http/get "/data.json"))]
        (reset! app-state {:form {:quantity 1
                                  :overlap 0}
                           :materias (:body response)}))))
(def day-index {"Lunes"  0
                "Martes" 1
                "Miércoles" 2
                "Jueves" 3
                "Viernes" 4
                "Sabado" 5})

(defn day-grid [combo]
  (let [dias (->> combo
                  (mapcat :dias)
                  (group-by :dia)
                  (sort-by (comp day-index first))
                  (map (fn [[k vs]]
                         [k (sort-by :inicio vs)]))                  
                  )]
    [:table
     [:tbody
      (for [[week-day dia] dias]
        [:tr [:td week-day]
         (for [{:keys [inicio fin]} dia]
           [:td inicio "-" fin])])]]))

(defn hello-world []
  (let [{:keys [materias]
         {:keys [quantity
                 overlap
                 select
                 selection]} :form} @app-state]
    [:div
     [:h1 "Cursada Compatible"]
     [:div
      [:div
       [:label {:for :overlap} "Ovelap"]
       [:input {:value overlap
                :type :number
                :default-value 0
                :on-change #(->> %
                                 .-target
                                 .-value
                                 (swap! app-state assoc-in [:form :overlap]))}]]
      [:div
       [:label {:for :cantidad}  "Cantidad a cursar"]
       [:input {:value quantity
                :type :number
                :default-value 1
                :on-change #(->> %
                                 .-target
                                 .-value
                                 (swap! app-state assoc-in [:form :quantity]))}]]
      [:div
       [:label {:for :materias } "Materias"]
       [:select {:name :materias
                 :on-change #(->> %
                                  .-target
                                  .-value
                                  (swap! app-state assoc-in [:form :select]))}
        (keep-indexed (fn [i {:keys [materia]}]
                        ^{:key materia} [:option {:value i} materia])
                      materias)]
       [:button {:on-click #(swap! app-state
                                   update-in [:form :selection] (fnil conj #{}) (materias select))} "+"]
       [:button {:on-click #(swap! app-state
                                   update-in [:form :selection] (fnil disj #{}) (materias select))} "-"]]
      [:div "Seleccionar entre:"
       (when (> quantity 0)
         (for [{combo-overlap :overlap
                combo :combination} (schedule quantity overlap selection)]
           ^{:key combo}
           [:div
            [:div [:span "overlap " combo-overlap]
             (for [{:keys [comision materia]} combo]
               ^{:key [materia comision]}
               [:div materia comision])]
            (day-grid combo)]))]]]))

(defn mount [el]
(rdom/render [hello-world] el))

(defn get-app-element []
(gdom/getElement "app"))

(defn mount-app-element []
(when-let [el (get-app-element)]
  (mount el)))

(mount-app-element)

(defn ^:after-load on-reload []
(mount-app-element))
