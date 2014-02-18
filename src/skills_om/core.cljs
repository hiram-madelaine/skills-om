(ns skills-om.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.set :as s]))

(enable-console-print!)

(def app-state (atom [{:id 0 :label "Clojure" :version "1.6" :level 4 :tier :middle :cat :lang}
                      {:id 1 :label "Spring" :version "3.0.0" :level 3 :tier :middle :cat :frmk}
                     {:id 2 :label "Java" :version "7" :level 4 :tier :middle :cat :lang}
                     {:id 3 :label "Oracle" :version "7" :level 5 :tier :data :cat :server}
                      ]))
(swap! app-state conj {:id 4 :label "Weblogic" :level 4 :version "10.3" :cat :server :tier :middle})

#_[
         {:tier :front :cat :lang :label "langage" :skills [ {:label "HTML" :level 4 :version 5}{:label "CSS" :level 3 :version 3}{:label "Javascript" :level 3}]}
         {:tier :front :cat :frmk :label "Framework" :skills [{:label "DWR" :level 3 :version 2}{:label "JQuery"}]}
         {:tier :middle :cat :lang :label "Language" :skills [{:label "Java" :level 5} {:label "Groovy" :level 3} {:label "Clojure" :level 2} {:label "Scala" :level 2}]}
         {:tier :middle :cat :frmk :label "Framework" :skills [{:label "Spring Core" :level 4}{:label "Hibernate" :level 4}{:label "Camel" :version "2.7" :level 4}]}
         {:tier :middle :cat :lib :label "Librairie" :skills [{:label "Google-Collections" :level 4}{:label "Joda" :level 4 :version "2.0"}{:label "POI" :version "3.7" :level 4}]}
         {:tier :middle :cat :server :label "Serveurs" :skills [{:label "Weblogic" :level 4 :version "10.3"} {:label "Tomcat" :version "6" :level 3}{:label "GlassFish" :version 2 :level 3}]}
         {:tier :data :cat :server :label "Base de donnÃ©es" :skills [ {:label "Oracle" :level 4 :version "10g"}{:label "PostgreSql" :level 3} {:label "mySQL" :level 2}{:label "MongoDB" :level 2}]}
         ]


(defn trace
  [app owner]
  (om/component
   (dom/p nil (str app))))


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
            (dom/article #js {:className (name (:cat cat))}
                         (dom/h2 nil (dom/p nil (name  (:cat cat))))
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
                         (dom/h2 nil (dom/p nil (name (:tier tier))))
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
           (dom/div nil (om/build board-view app {:fn #(s/index % [:tier])})) )))


(om/root
 app-view
 app-state
  {:target (. js/document (getElementById "app")) })
