(ns skills-om.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [chan put! <!]]
            [clojure.set :as s]))

(enable-console-print!)


;;;;; App State ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def app-state (atom {:skills  [{:id 0 :label "Clojure" :version "1.6" :level 4 :tier "middle" :cat "lang"}
                                {:id 1 :label "Spring" :version "3.0.0" :level 3 :tier "middle" :cat "frmk"}
                                {:id 2 :label "Java" :version "7" :level 4 :tier "middle" :cat "lang"}
                                {:id 3 :label "Oracle" :version "7" :level 5 :tier "data" :cat "server"}
                                ]}))

(def app-ref {:en {:tier [{:code "middle" :label "Middleware"}
                          {:code "front" :label "Front"}
                          {:code "data" :label "Database"}]
                   :cat [ {:code "lang" :label "Language"}
                          {:code "frmk" :label "Framework"}
                          {:code "server" :label "Serveur"}]
                   :level [{:code 1 :label "Novice"}
                           {:code 2 :label "Advanced beginner"}
                           {:code 3 :label "Competent"}
                           {:code 4 :label "Proficient"}
                           {:code 5 :label "Expert"}]
                   :input {:level "Level"
                           :label "Name"
                           :cat "Category"
                           :add-skill "Add skill"
                           :del-skill "X"}}
              :fr  {:tier [{:code "middle" :label "Middleware"}
                          {:code "front" :label "Front"}
                          {:code "data" :label "Database"}]
                   :cat [ {:code "lang" :label "Langage"}
                          {:code "frmk" :label "Framework"}
                          {:code "server" :label "Serveur"}]
                   :level [{:code 1 :label "Novice"}
                           {:code 2 :label "Débutant"}
                           {:code 3 :label "Compétent"}
                           {:code 4 :label "Efficace"}
                           {:code 5 :label "Expert"}]
                    :input {:level "Niveau"
                            :label "Nom"
                            :cat "Catégorie"
                            :add-skill "Ajouter compétence"
                            :del-skill "X"}}})


;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn ->label
  "Translate labels or business data"
  ([ref-data m k]
   "Find label in the ref data"
   (->> k
        ref-data
        (some #(when ((comp  #{(k m)} :code) %) %))
        :label))
  ([ref-data ks]
   (get-in ref-data ks)))



(defn trace
  "Display the raw data."
  [app owner]
  (om/component
   (dom/p nil (str app))))

;;;;;; Skill input ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn e-value
  [e]
  (-> e .-target .-value))

(defn make-select
  [c l k v data]
  (dom/span nil
           (dom/label nil l)
           (apply dom/select #js {:value v
                                  :onChange #(put! c [k (e-value %)])}
                       (dom/option #js {:value ""} "")
                       (map (fn [{:keys [code label]}]
                              (dom/option #js {:value code} label)) data))))

(defn make-input
  ([c l k v t]
   (make-input c l k v t {}))
  ([c l k v t opts]
   (let [put-chan #(put! c [k (e-value %)])]
     (dom/span nil
                    (dom/label nil l)
                    (dom/input (clj->js (merge opts {:type t
                                                     :value v
                                                     :onChange put-chan})))))))


(defn skill-input
  [app owner]
  (reify
    om/IInitState
    (init-state [_]
                (let [init {:label "" :version "" :level 0 :cat "" :tier ""}]
                  {:chan (chan)
                   :inputs init
                   :init init
                   :coercers {:level int}}))
    om/IWillMount
    (will-mount [this]
                (let [{ :keys [coercers chan init] :as state} (om/get-state owner)]
                  (go
                   (loop []
                     (let [[k v] (<! chan)
                           coerce (get coercers k identity)]
                       (condp = k
                         :post (do
                                 (om/transact! app (fn [skills] (conj skills v)))
                                 (om/set-state! owner [:inputs] init))
                         (om/set-state! owner [:inputs k] (coerce v))))
                     (recur)))))
    om/IRenderState
    (render-state [_ {chan :chan  {:keys [label level cat tier version] :as new-skill} :inputs }]
                  (let [i18n (om/get-shared owner :i18n)]
                    (dom/fieldset #js {:className "form"}
                                  (make-input chan (i18n [:input :label])  :label label "text")
                                  (make-input chan "Version" :version version "tex")
                                  (make-input chan (i18n [:input :level]) :level level "range" {:min 1 :max 5})
                                  (make-select chan "Tier" :tier tier (i18n [:tier] ))
                                  (make-select chan (i18n [:input :cat]) :cat cat (i18n [:cat]))
                                  (dom/input #js {:type  "button"
                                                  :value (i18n [:input :add-skill])
                                                  :onClick #(put! chan [:post new-skill])}))))))


;;;;;; Skills Board ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn skill-view
  "Display a skill on a post-it line"
  [app owner]
  (om/component
   (let [{:as skill :keys [label version level tier cat]} app]
     (dom/li nil
             (dom/p nil
                    (dom/span nil label)
                    (dom/span nil version)
                    (dom/span nil ((om/get-shared owner :i18n) skill :level)))))))

(defn post-it-view
  "Display a post-it from skills by category"
  [[cat skills] owner]
  (om/component
   (dom/article #js {:className (:cat cat)}
                (dom/h2 nil (dom/p nil ((om/get-shared owner :i18n) cat :cat)))
                (apply dom/ul nil (om/build-all skill-view skills {:key :id})))))


(defn posts-view
  "Display all the post-its for a tier"
  [skills-by-cat owner]
  (om/component
   (apply dom/div nil (om/build-all post-it-view skills-by-cat ))))


(defn tier-view
  "Display a tier user story"
  [[tier skills] owner]
  (om/component
    (dom/section nil
                 (dom/h2 nil (dom/p nil ((om/get-shared owner :i18n) tier :tier)))
                 (om/build posts-view skills {:fn #(s/index % [:cat])}))))

(defn board-view
  "Display the whole board of skills from skills by tier"
  [skills-by-tier owner]
 (om/component
  (apply dom/div nil (om/build-all tier-view skills-by-tier))))


(defn app-view
  "Display the whole app"
  [app owner]
  (om/component
   (dom/div nil
            (om/build skill-input app)
            (om/build board-view app {:fn #(s/index % [:tier])}))))

(om/root
 app-view
 app-state
  {:target (. js/document (getElementById "app"))
   :path [:skills]
   :shared {:i18n (partial ->label (:fr app-ref))}})
