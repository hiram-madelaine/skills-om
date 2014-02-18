(ns skills-om.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [chan put! <!]]
            [clojure.set :as s]))

(enable-console-print!)


;;;;; App State ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def app-state (atom [{:id 0 :label "Clojure" :version "1.6" :level 4 :tier "middle" :cat "lang"}
                      {:id 1 :label "Spring" :version "3.0.0" :level 3 :tier "middle" :cat "frmk"}
                     {:id 2 :label "Java" :version "7" :level 4 :tier "middle" :cat "lang"}
                     {:id 3 :label "Oracle" :version "7" :level 5 :tier "data" :cat "server"}
                      ]))
#_(swap! app-state conj {:id 4 :label "Weblogic" :level 4 :version "10.3" :cat "server" :tier "middle"})



(def app-ref {:tier [{:code "middle" :label "Middleware"}
                     {:code "front" :label "Front"}
                     {:code "data" :label "Database"}]
              :cat [ {:code "lang" :label "Language"}
                     {:code "frmk" :label "Framework"}
                     {:code "server" :label "Serveur"}]})


;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn trace
  "Display the raw data."
  [app owner]
  (om/component
   (dom/p nil (str app))))
;;;;;; Skill input ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn make-select
  [c l k v data]
  (dom/div nil
           (dom/label nil l)
           (apply dom/select #js {:value v
                                       :onChange #(put! c [k (-> % .-target .-value)])}
                       (dom/option #js {:value ""} "")
                       (map (fn [{:keys [code label]}]
                              (dom/option #js {:value code} label)) data))))

(defn make-input
  [c l k v t]
  (dom/div nil
           (dom/label nil l)
           (dom/input #js {:type t
                           :value v
                           :onChange #(put! c [k (-> % .-target .-value)])})))

(defn skill-input
  [app owner]
  (reify
    om/IInitState
    (init-state [_]
                {:chan (chan)
                 :inputs   {:label ""
                            :level 0
                            :cat ""
                            :tier ""
                            :version ""}})
    om/IWillMount
    (will-mount [this]
                (let [chan (om/get-state owner :chan)]
                  (go
                   (loop []
                     (let [[k v] (<! chan)]
                       (om/set-state! owner [:inputs k] v))
                     (recur)))))
    om/IRenderState
    (render-state [_ {chan :chan  {:keys [label level cat tier version] :as new-skill} :inputs }]
                  (prn new-skill)
                  (dom/div nil
                           (make-input chan "label" :label label "text")
                           (make-input chan "Version" :version version "tex")
                           (make-input chan "Level" :level level "range")
                           (make-select chan "Tier" :tier tier (om/get-shared owner [:tier] ) )
                           (make-select chan "Category" :cat cat (om/get-shared owner [:cat]))
                           (dom/button #js {:onClick #(om/transact! app (fn [skills] (conj skills new-skill )))})))))


;;;;;; Skills Board ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn skill-view
  "Display a skill on a post-it line"
  [app owner]
  (reify
    om/IRender
    (render [_]
            (let [{:as skill :keys [label version level tier cat]} app]
              (dom/li nil (dom/p nil  (dom/span nil label)
                                 (dom/span nil version)))))))
(defn post-it-view
  "Display a post-it from skills by category"
  [[cat skills] owner]
  (reify
    om/IRender
    (render [_]
            (dom/article #js {:className (:cat cat)}
                         (dom/h2 nil (dom/p nil (:cat cat)))
                         (apply dom/ul nil (om/build-all skill-view skills {:key :id}))))))


(defn posts-view
  "Display all the post-its for a tier"
  [skills-by-cat owner]
  (reify
    om/IRender
    (render [_]
            (apply dom/div nil (om/build-all post-it-view skills-by-cat )))))

(defn tier-view
  "Display a tier user story"
  [[tier skills] owner]
  (reify
    om/IRender
    (render [_]
            (dom/section nil
                         (dom/h2 nil (dom/p nil (:tier tier)))
                         (om/build posts-view skills {:fn #(s/index % [:cat])})  ))))

(defn board-view
  "Display the whole board of skills from skills by tier"
 [app owner]
 (reify
   om/IRender
   (render [_]
           (apply dom/div nil (om/build-all tier-view app)))))

(defn app-view
  [app owner]
  (reify
    om/IRender
    (render [_]
           (dom/div nil
                    (om/build skill-input app)
                    (om/build board-view app {:fn #(s/index % [:tier])})) )))


(om/root
 app-view
 app-state
  {:target (. js/document (getElementById "app"))
   :shared app-ref})
