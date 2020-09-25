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
                  (< ini1 fin2 fin1)
                  (= ini1 ini2)
                  (= fin1 fin2)))
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
              (map (fn [{:keys [codigo materia comisiones]}]
                     (map #(assoc %
                                  :materia materia
                                  :codigo codigo)
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
  (go (let [response (<! (http/get "data.json"))]
        (reset! app-state {:form {:quantity 1
                                  :select 0
                                  :overlap 240}
                           :materias (:body response)}))))

(defn autocomplete [{:keys [on-select options]}]
  (reagent/with-let [value (atom {:index 0 :class :autocomplete-hidden})]
    (let [{:keys [input class]} @value

          fragments (->> (clojure.string/split input #"\s+")
                         (remove empty?)
                         (map #(re-pattern (str "(?i).*" % ".*")))
                         seq)

          matches   (fn [s]
                      (if fragments
                        (every? #(re-matches % s) fragments)
                        true))]

      [:div.autocomplete
       [:input
        {:value input
         :on-click #(swap! value assoc :class :autocomplete-options)
         :on-change #(->> %
                          .-target
                          .-value
                          (swap! value assoc :input))}]
       [:div {:class class}
        (->> options
             (keep-indexed (fn [i v]
                             [i v]))
             (filter (fn [[i v]] (matches v)))
             (map (fn [[i v]]
                    ^{:key i}
                    [:div.autocomplete-option {:on-click #(swap! value (fn [m]
                                                                         (and on-select (on-select {:index i :value v}))
                                                                         (assoc m
                                                                                :index i
                                                                                :class :autocomplete-hidden
                                                                                :input v)))}
                     v])))]])))

(def day-index {"Lunes"  0
                "Martes" 1
                "Miércoles" 2
                "Jueves" 3
                "Viernes" 4
                "Sabado" 5})

(defn make-tree [fs leaf data]
  (if (seq fs)
    (->> data
         (group-by (first fs))
         (map (fn [[k vs]]
                [k (make-tree (rest fs) leaf vs)]))
         (into {}))
    (leaf data)))
(defn sort-tree [fs tree]
  (if (seq fs)
    (->> tree
         (sort-by (comp (first fs) first))
         (map (fn [[k t]]
                [k (sort-tree (rest fs) t)])))
    tree))

(defn day-row [hours data]
  (loop [r []
         [i :as iis] hours]
    (let [item (get data i)]
      (cond
        (empty? iis) r
        (nil?  item) (recur (conj r [:td])
                            (rest iis))
        :else        (let [{:keys [duration comision materia]} item]
                       (recur (conj r [:td {:col-span duration} materia "-" comision])
                              (drop duration iis)))))))

(defn day-grid [quantity combo]
  (let [data (->> combo
                  (mapcat (fn [{:keys [materia comision codigo dias]}]
                            (map #(assoc %
                                         :comision comision
                                         :materia codigo )
                                 dias))))

        hours (->> data
                   (mapcat (fn [{:keys [inicio fin]}]
                             [inicio fin]))
                   (into #{})
                   (sort))

        duration (let [horas-index (->> hours
                                        (sort)
                                        (keep-indexed (fn [i v]
                                                        [v i]))
                                        (into {}))]
                   (fn [{:keys [fin inicio]}]
                     (-
                      (horas-index fin)
                      (horas-index inicio))))
        tree (->> data
                  (map #(assoc % :duration (duration %)))
                  (make-tree [:dia :materia :inicio] first)
                  (sort-by (comp day-index first)))]
    [:div 
     [:table
      [:thead
       (into [:tr [:td "Día"]] (map (fn [hour]
                                      ^{:key hour} [:td hour])
                                    hours))]
      (into [:tbody ]
            (for [[day materias] tree
                  :let [rowspan         (count materias)
                        [first & rest] (->> materias
                                            (map (comp (partial day-row hours) second)))]]
              (concat [(into ^{:key first } [:tr [:td {:row-span rowspan} day]] first)]
                      (map #(into ^{:key %} [:tr] %) rest))))]]))

(defn hello-world []
  (let [{:keys [materias]
         {:keys [quantity
                 overlap
                 select
                 selection]} :form} @app-state
        quitar  (fn [materia]
                  #(swap! app-state
                          update-in [:form :selection] (fnil disj #{}) materia))
        agregar (fn [materia]
                  #(swap! app-state
                          update-in [:form :selection] (fnil conj #{}) materia)) ]
    [:div
     [:h1 "Cursada Compatible"]
     [:div

      [:p
       "Se pueden superponer hasta "
       [:input {:value overlap
                :type :number
                :default-value 0
                :on-change #(->> %
                                 .-target
                                 .-value
                                 (swap! app-state assoc-in [:form :overlap]))}] " minutos."]
      [:p
       "De las seleccionadas quiero cursar una cantidad de "
       [:input {:value quantity
                :type :number
                :default-value 1
                :on-change #(->> %
                                 .-target
                                 .-value
                                 (swap! app-state assoc-in [:form :quantity]))}]
       " materias."]
      [:div
       [:label {:for :materias } "Materias:"]
       [autocomplete {:options (map :materia materias)
                      :on-select (fn [{:keys [index]}]
                                   ((agregar (materias index))))}]]
      [:ul
       (for [{:keys [codigo materia] :as select} selection]
         ^{:key codigo} [:li codigo "-" materia [:button {:on-click (quitar select)} "- Quitar"]])]
      [:div
       "Cursadas:"
       (when (> quantity 0)
         (for [{combo-overlap :overlap
                combo     :combination} (schedule quantity overlap selection)]
           ^{:key combo}
           [:div
            [:p
             [:span "Superposición (minutos) " combo-overlap]]
            (day-grid quantity combo)]))]]]))

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
