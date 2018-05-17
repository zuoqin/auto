(ns avto.vin
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om :include-macros true]
            [om-tools.dom :as dom :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [secretary.core :as sec :refer-macros [defroute]]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [avto.core :as avto]
            [avto.settings :as settings]
            [om.dom :as omdom :include-macros true]
            [cljs-time.format :as tf]
            [cljs-time.core :as tc]
            [cljs-time.coerce :as te]
            [cljs-time.local :as tl]
            [clojure.string :as str]
            [ajax.core :refer [GET POST]]
            [om-bootstrap.input :as i]
            [om-bootstrap.button :as b]
            [om-bootstrap.panel :as p]
            [goog.string :as gstring]
            [goog.string.format]
            [chord.client :refer [ws-ch]]
            [cljs.core.async :refer [put! dropping-buffer chan take! <! >! timeout close!]]
  )
  (:import goog.History)
)

(def jquery (js* "$"))

(enable-console-print!)

(def ch (chan (dropping-buffer 2)))

(def pledges [{:value "WDD2210561A044990" :text "WDD2210561A044990"} {:value "XTA212140C2050415" :text "XTA212140C2050415"} {:value "XUFJA695JE3036223" :text "XUFJA695JE3036223"} {:value "XWB4A11EDCA049746" :text "XWB4A11EDCA049746"} {:value "Z94CU41DABR030030" :text "Z94CU41DABR030030"}])

(defn clearprices []
    (swap! avto/app-state assoc-in [:object :onemonthprice] 0.0)
    (swap! avto/app-state assoc-in [:object :onedayprice] 0.0)
    (swap! avto/app-state assoc-in [:object :twomonthprice] 0.0)
    (swap! avto/app-state assoc-in [:object :minprice] 0.0)
    (swap! avto/app-state assoc-in [:object :maxprice] 0.0)
)


(defn error-handler [{:keys [status status-text]}]
  (.log js/console (str "something bad happened: " status " " status-text))
  (swap! avto/app-state assoc-in [:state] 0)
)


(defn comp-groups
  [group1 group2]
  ;(.log js/console group1)
  ;(.log js/console group2)
  (if (> (compare (:name group1) (:name group2)) 0)
      false
      true
  )
)

(defn drop-nth [n coll]
   (keep-indexed #(if (not= %1 n) %2) coll))


(defn handle-chkbsend-change [e]
  (let [
      id (str/join (drop 9 (.. e -currentTarget -id)))
      groups (:groups (:device @avto/app-state))
      newgroups (if (= true (.. e -currentTarget -checked)) (conj groups id) (remove (fn [x] (if (= x id) true false)) groups))
    ]
    (.stopPropagation e)
    (.stopImmediatePropagation (.. e -nativeEvent) )
    (swap! avto/app-state assoc-in [:device :groups] newgroups)
  )
)


(defn handleChange [e]
  ;(.log js/console (.. e -nativeEvent -target)  )  
  ;(.log js/console (.. e -nativeEvent -target -step))
  (swap! avto/app-state assoc-in [:object (keyword (.. e -nativeEvent -target -id))] (if (= "" (.. e -nativeEvent -target -step)) (.. e -nativeEvent -target -value) (js/parseFloat (.. e -nativeEvent -target -value))))
)

(defn map-item [item]
  (let [
    ]
    ;
    {:value (get item "value") :text (get item "text")}
  )
)

(defn setobjectvals [value]
  (case value
    "Z94CU41DABR030030" (let []
    (swap! avto/app-state assoc-in [:object :bodyType] "8")
    (swap! avto/app-state assoc-in [:object :driveType] "2")
    (swap! avto/app-state assoc-in [:object :engineType] "1")
    (swap! avto/app-state assoc-in [:object :gearboxType] "2")
    (swap! avto/app-state assoc-in [:object :generation] "885")
    (swap! avto/app-state assoc-in [:object :horsepower] 100)
    (swap! avto/app-state assoc-in [:object :year] "2016")
    (swap! avto/app-state assoc-in [:object :region] "77")
    (swap! avto/app-state assoc-in [:object :run] 10000)
    (swap! avto/app-state assoc-in [:object :model] "Hyundai Solaris")
    (swap! avto/app-state assoc-in [:object :bank] "Публичное акционерное общество РОСБАНК")
    (swap! avto/app-state assoc-in [:object :person] "ЧИКУРОВА ВИОРИКА ИВАНОВНА 15.01.1969")
    (swap! avto/app-state assoc-in [:object :passport] "Паспорт гражданина Российской Федерации, 94 13 393916")
    )
    "XTA212140C2050415" (let []
    (swap! avto/app-state assoc-in [:object :bodyType] "1")
    (swap! avto/app-state assoc-in [:object :driveType] "3")
    (swap! avto/app-state assoc-in [:object :engineType] "1")
    (swap! avto/app-state assoc-in [:object :gearboxType] "1")
    (swap! avto/app-state assoc-in [:object :generation] "8585")
    (swap! avto/app-state assoc-in [:object :horsepower] 100)
    (swap! avto/app-state assoc-in [:object :year] "2017")
    (swap! avto/app-state assoc-in [:object :region] "77")
    (swap! avto/app-state assoc-in [:object :run] 10000)
    (swap! avto/app-state assoc-in [:object :model] "LADA ВАЗ-21214 030-20")
    (swap! avto/app-state assoc-in [:object :bank] "Публичное акционерное общество РОСБАНК")
    (swap! avto/app-state assoc-in [:object :person] "ИВАНОВ АЛЕКСЕЙ ВАСИЛЬЕВИЧ 01.05.1962")
    (swap! avto/app-state assoc-in [:object :passport] "Паспорт гражданина Российской Федерации, 25 07 931798")
    )


    "WDD2210561A044990" (let []
    (swap! avto/app-state assoc-in [:object :bodyType] "8")
    (swap! avto/app-state assoc-in [:object :driveType] "3")
    (swap! avto/app-state assoc-in [:object :engineType] "1")
    (swap! avto/app-state assoc-in [:object :gearboxType] "2")
    (swap! avto/app-state assoc-in [:object :generation] "1270")
    (swap! avto/app-state assoc-in [:object :horsepower] 100)
    (swap! avto/app-state assoc-in [:object :year] "2017")
    (swap! avto/app-state assoc-in [:object :region] "77")
    (swap! avto/app-state assoc-in [:object :run] 10000)
    (swap! avto/app-state assoc-in [:object :model] "Mercedes МЕРСЕДЕС BENZ S350")
    (swap! avto/app-state assoc-in [:object :bank] "Публичное акционерное общество РОСБАНК")
    (swap! avto/app-state assoc-in [:object :person] "Ильченко Алексей Игоревич 16.04.1986")
    (swap! avto/app-state assoc-in [:object :passport] "Паспорт гражданина Российской Федерации, 2012 561810")
    )

    "XWB4A11EDCA049746" (let []
    (swap! avto/app-state assoc-in [:object :bodyType] "16")
    (swap! avto/app-state assoc-in [:object :driveType] "2")
    (swap! avto/app-state assoc-in [:object :engineType] "1")
    (swap! avto/app-state assoc-in [:object :gearboxType] "2")
    (swap! avto/app-state assoc-in [:object :generation] "432")
    (swap! avto/app-state assoc-in [:object :horsepower] 100)
    (swap! avto/app-state assoc-in [:object :year] "2017")
    (swap! avto/app-state assoc-in [:object :region] "77")
    (swap! avto/app-state assoc-in [:object :run] 10000)
    (swap! avto/app-state assoc-in [:object :model] "Daewoo MATIZ")
    (swap! avto/app-state assoc-in [:object :bank] "Публичное акционерное общество РОСБАНК")
    (swap! avto/app-state assoc-in [:object :person] "Киселева Галина Александровна 07.09.1965")
    (swap! avto/app-state assoc-in [:object :passport] "Паспорт гражданина Российской Федерации, 8010 170843")
    )

    "XUFJA695JE3036223" (let []
    (swap! avto/app-state assoc-in [:object :bodyType] "8")
    (swap! avto/app-state assoc-in [:object :driveType] "2")
    (swap! avto/app-state assoc-in [:object :engineType] "1")
    (swap! avto/app-state assoc-in [:object :gearboxType] "2")
    (swap! avto/app-state assoc-in [:object :generation] "6711")
    (swap! avto/app-state assoc-in [:object :horsepower] 100)
    (swap! avto/app-state assoc-in [:object :year] "2016")
    (swap! avto/app-state assoc-in [:object :region] "77")
    (swap! avto/app-state assoc-in [:object :run] 10000)
    (swap! avto/app-state assoc-in [:object :model] "Chevrolet Cruze KL1J")
    (swap! avto/app-state assoc-in [:object :bank] "Публичное акционерное общество РОСБАНК")
    (swap! avto/app-state assoc-in [:object :person] "Гавринёва Екатерина Владимировна 29.11.1982")
    (swap! avto/app-state assoc-in [:object :passport] "Паспорт гражданина Российской Федерации, 6003 764500")
    )
  )
)


(defn map-model [model]
  (let [
    ]
    ;
    {:value (get model "value") :text (get model "text") :isManufacturing (get model "isManufacturing")}
  )
)

(defn  map-characteristic-item [item]
  (let [
     tr1 (.log js/console item)
    ]
    ;
    {:value (get item "value") :text (get item "text")}
  )
)


(defn map-characteristic [item]
  (let [
      bodytypes (map map-characteristic-item (get item "bodyTypes"))
      enginetypes (map map-characteristic-item (get item "engineTypes"))
      gearboxtypes (map map-characteristic-item (get item "gearboxTypes"))
      drivetypes (map map-characteristic-item (get item "driveTypes"))
    ]
    ;
    {:bodyTypes bodytypes :engineTypes enginetypes :driveTypes drivetypes :gearboxTypes gearboxtypes}
  )
)

(defn map-generation [item]
  (let [
    ]
    ;
    {:value (get item "value") :text (get item "text")}
  )
)

(defn OnGetCharacteristics [response]
  (let[
    
    characteristics (map-characteristic response)

    oldmodels (:models (first (filter (fn [x] (if (= (:value x) (js/parseInt (:manufacture (:object @avto/app-state)))) true false)) (:manufactures @avto/app-state))))

    oldyears (:years (first (filter (fn [x] (if (= (js/parseInt (:model (:object @avto/app-state))) (:value x)) true false)) oldmodels)))

    oldgenerations (:generations (first (filter (fn [x] (if (= (js/parseInt (:year (:object @avto/app-state)))  (:value x)) true false)) oldyears)))

    newgenerations (map (fn [x] (if (= (:value x) (js/parseInt (:generation (:object @avto/app-state)))) (assoc-in x [:characteristics] characteristics) x)) oldgenerations)

    newyears (map (fn [x] (if (= (:value x) (js/parseInt (:year (:object @avto/app-state))) ) (assoc-in x [:generations] newgenerations) x)) oldyears)

    newmodels (map (fn [x] (if (= (:value x) (js/parseInt (:model (:object @avto/app-state))))  (assoc-in x [:years] newyears) x)) oldmodels) 

    newmanufactures (map (fn [x] (if (= (:value x) (js/parseInt (:manufacture (:object @avto/app-state)))) (assoc-in x [:models] newmodels) x)) (:manufactures @avto/app-state) )
    ]
    ;(.log js/console generations)
    (swap! avto/app-state assoc-in [:manufactures] newmanufactures)
    (swap! avto/app-state assoc-in [:state] 0)
    (put! ch 60)
    ;;(.log js/console response)
  )
)


(defn OnGetGenerations [response]
  (let[
    generations (map map-generation response)
    tr1 (.log js/console (str "first generation: " (:text (first generations))))
    oldmodels (:models (first (filter (fn [x] (if (= (:value x) (js/parseInt (:manufacture (:object @avto/app-state)))) true false)) (:manufactures @avto/app-state))))

    oldyears (:years (first (filter (fn [x] (if (= (js/parseInt (:model (:object @avto/app-state))) (:value x)) true false)) oldmodels)))

    newyears (map (fn [x] (if (= (:value x) (js/parseInt (:year (:object @avto/app-state))) ) (assoc-in x [:generations] generations) x)) oldyears)

    newmodels (map (fn [x] (if (= (:value x) (js/parseInt (:model (:object @avto/app-state))))  (assoc-in x [:years] newyears) x)) oldmodels) 

    newmanufactures (map (fn [x] (if (= (:value x) (js/parseInt (:manufacture (:object @avto/app-state)))) (assoc-in x [:models] newmodels) x)) (:manufactures @avto/app-state) )
    ]
    ;(.log js/console generations)
    (swap! avto/app-state assoc-in [:manufactures] newmanufactures)
    (swap! avto/app-state assoc-in [:state] 0)
    (put! ch 59)
    ;;(.log js/console response)
  )
)

(defn OnGetYears [response]
  (let[
    from_year (get response "from")
    to_year  (get response "to")
    years (map (fn [x] {:value x :text (str x)}) (range from_year (min 2019 (+ to_year 1)))) 
    oldmodels (:models (first (filter (fn [x] (if (= (:value x) (js/parseInt (:manufacture (:object @avto/app-state)))) true false)) (:manufactures @avto/app-state))))

    newmodels (map (fn [x] (if (= (:value x) (js/parseInt (:model (:object @avto/app-state))))  (assoc-in x [:years] years) x)) oldmodels) 

    newmanufactures (map (fn [x] (if (= (:value x) (js/parseInt (:manufacture (:object @avto/app-state)))) (assoc-in x [:models] newmodels) x)) (:manufactures @avto/app-state) )
    ]
    (.log js/console years)
    (swap! avto/app-state assoc-in [:manufactures] newmanufactures)
    (swap! avto/app-state assoc-in [:state] 0)
    (put! ch 58)
    ;;(.log js/console response)
  )
)

(defn OnGetModels [response]
  (let[
    models (map map-model response)

    newmanufactures (map (fn [x] (if (= (:value x) (js/parseInt (:manufacture (:object @avto/app-state)))) (assoc-in x [:models] models) x)) (:manufactures @avto/app-state) )
    ]
    (.log js/console (str "received model1: " (:text (first models))))

    (swap! avto/app-state assoc-in [:manufactures] newmanufactures)
    (swap! avto/app-state assoc-in [:state] 0)
    (put! ch 57)
    ;;(.log js/console response)
  )
)


(defn requestYears []
  (let [
    ;status (js/parseInt (:statuses (:filter @app-state)))
    ;user (:user (:filter @app-state))
    ]
    (.log js/console "requesting yerars")
    (swap! avto/app-state assoc-in [:state] 1)
    (GET (str settings/apipath "taxonomy/manufactureYears?modelId=" (:model (:object @avto/app-state))) {
      :handler OnGetYears
      :error-handler error-handler
      :response-format :json
      :headers {(keyword "X-Requested-With")  "XMLHttpRequest"}
    })
  )
)

(defn requestCharacteristics []
  (let [

    ]
    (.log js/console "requesting characteristics")
    (swap! avto/app-state assoc-in [:state] 1)
    (GET (str settings/apipath "taxonomy/availableCharacteristics?generationId=" (:generation (:object @avto/app-state))) {
      :handler OnGetCharacteristics
      :error-handler error-handler
      :response-format :json
      :headers {(keyword "X-Requested-With")  "XMLHttpRequest"}
    })
  )
)

(defn requestGenerations []
  (let [
    ;status (js/parseInt (:statuses (:filter @app-state)))
    ;user (:user (:filter @app-state))
    ]
    (.log js/console "requesting generations")
    (swap! avto/app-state assoc-in [:state] 1)
    (GET (str settings/apipath "taxonomy/generations?modelId=" (:model (:object @avto/app-state)) "&manufactureYear=" (:year (:object @avto/app-state))) {
      :handler OnGetGenerations
      :error-handler error-handler
      :response-format :json
      :headers {(keyword "X-Requested-With")  "XMLHttpRequest"}
    })
  )
)

(defn requestModels []
  (let [
    ;status (js/parseInt (:statuses (:filter @app-state)))
    ;user (:user (:filter @app-state))
    ]
    (.log js/console "Trying to download models")
    (swap! avto/app-state assoc-in [:state] 1)
    (GET (str settings/apipath "taxonomy/models?makeId=" (:manufacture (:object @avto/app-state))) {
      :handler OnGetModels
      :error-handler error-handler
      :response-format :json
      :headers {(keyword "X-Requested-With")  "XMLHttpRequest"}
    })
  )
)


(defn getCharacteristics []
  (let [
    manufacture (first (filter (fn [x] (if (= (js/parseInt (:manufacture (:object @avto/app-state))) (:value x)) true false)) (:manufactures @avto/app-state)))

    model      (first (filter (fn [x] (if (= (js/parseInt (:model (:object @avto/app-state))) (:value x)) true false)) (:models manufacture)))
   
    year (first (filter (fn [x] (if (= (:year (:object @avto/app-state)) (:value x)) true false)) (:years model)))

    generation (first (filter (fn [x] (if (= (js/parseInt (:generation (:object @avto/app-state))) (:value x)) true false)) (:generations year)))
    ]
    (.log js/console (str "found model: " model))
    (if (= (count (:characteristics generation)) 0)
      (requestCharacteristics)
      (put! ch 60)
    )
  )
)

(defn getGenerations []
  (let [
    manufacture (first (filter (fn [x] (if (= (js/parseInt (:manufacture (:object @avto/app-state))) (:value x)) true false)) (:manufactures @avto/app-state)))

    model (first (filter (fn [x] (if (= (js/parseInt (:model (:object @avto/app-state))) (:value x)) true false)) (:models manufacture)))
   
    year (first (filter (fn [x] (:year (:object @avto/app-state)) (:value x) true false) (:years model)))
    ]
    (.log js/console (str "found model: " model))
    (if (= (count (:generations year)) 0)
      (requestGenerations)
      (put! ch 59)
    )
  )
)


(defn getYears []
  (let [
    manufacture (first (filter (fn [x] (if (= (js/parseInt (:manufacture (:object @avto/app-state))) (:value x)) true false)) (:manufactures @avto/app-state)))

    model (first (filter (fn [x] (if (= (js/parseInt (:model (:object @avto/app-state))) (:value x)) true false)) (:models manufacture)))
    ]
    (.log js/console (str "found model: " model))
    (if (= (count (:years model)) 0)
      (requestYears)
      (put! ch 58)
    )
  )
)


(defn getModels []
  (let [
    manufacture (first (filter (fn [x] (if (= (js/parseInt (:manufacture (:object @avto/app-state))) (:value x)) true false)) (:manufactures @avto/app-state)))
    ]
    (.log js/console manufacture)
    (if (= (count (:models manufacture)) 0)
      (requestModels)
      (put! ch 57)
    )
  )
)


(defn createCharacteristics []
  (let [
    select_bodyType (js/document.getElementById "bodyType")
    select_gearboxType (js/document.getElementById "gearboxType")
    select_engineType (js/document.getElementById "engineType")
    select_driveType (js/document.getElementById "driveType")
    models (:models (first (filter (fn [x] (if (= (:value x) (js/parseInt (:manufacture (:object @avto/app-state)))) true false)) (:manufactures @avto/app-state))))

    years (:years (first (filter (fn [x] (if (= (:value x) (js/parseInt (:model (:object @avto/app-state)))) true false)) models)))

    generations (:generations (first (filter (fn [x] (if (= (:value x) (js/parseInt (:year (:object @avto/app-state))) ) true false)) years)))

    characteristics (:characteristics (first (filter (fn [x] (if (= (:value x) (js/parseInt (:generation (:object @avto/app-state)))) true false)) generations)))
    ]
    (.log js/console "Updating bodytype")
    (doall (map (fn [x] (let [
      option (js/document.createElement "option")

      ]

      (set! (.-text option) (:text x))
      (set! (.-value option) (:value x))
      (.add select_bodyType option)
    )) (:bodyTypes characteristics)))

    (jquery
      (fn []
        (-> (jquery "#bodyType")
          (.selectpicker "refresh")
        )
      )
    )

    (jquery
      (fn []
        (-> (jquery "#bodyType")
          (.selectpicker "val" (:bodyType (:object @avto/app-state)))
        )
      )
    )

    (.log js/console "Updating gearboxType")
    (doall (map (fn [x] (let [
      option (js/document.createElement "option")

      ]

      (set! (.-text option) (:text x))
      (set! (.-value option) (:value x))
      (.add select_gearboxType option)
    )) (:gearboxTypes characteristics)))

    (jquery
      (fn []
        (-> (jquery "#gearboxType")
          (.selectpicker "refresh")
        )
      )
    )

    (jquery
      (fn []
        (-> (jquery "#gearboxType")
          (.selectpicker "val" (:gearboxType (:object @avto/app-state)))
        )
      )
    )



    (.log js/console "Updating engineType")
    (doall (map (fn [x] (let [
      option (js/document.createElement "option")

      ]

      (set! (.-text option) (:text x))
      (set! (.-value option) (:value x))
      (.add select_engineType option)
    )) (:engineTypes characteristics)))

    (jquery
      (fn []
        (-> (jquery "#engineType")
          (.selectpicker "refresh")
        )
      )
    )

    (jquery
      (fn []
        (-> (jquery "#engineType")
          (.selectpicker "val" (:engineType (:object @avto/app-state)))
        )
      )
    )

    (.log js/console "Updating drivetype")
    (doall (map (fn [x] (let [
      option (js/document.createElement "option")

      ]

      (set! (.-text option) (:text x))
      (set! (.-value option) (:value x))
      (.add select_driveType option)
    )) (:driveTypes characteristics)))

    (jquery
      (fn []
        (-> (jquery "#driveType")
          (.selectpicker "refresh")
        )
      )
    )

    (jquery
      (fn []
        (-> (jquery "#driveType")
          (.selectpicker "val" (:driveType (:object @avto/app-state)))
        )
      )
    )
  )
)


(defn createGenerations []
  (let [
    select (js/document.getElementById "generation")
    models (:models (first (filter (fn [x] (if (= (:value x) (js/parseInt (:manufacture (:object @avto/app-state)))) true false)) (:manufactures @avto/app-state))))

    years (:years (first (filter (fn [x] (if (= (:value x) (js/parseInt (:model (:object @avto/app-state)))) true false)) models)))

    generations (:generations (first (filter (fn [x] (if (= (:value x) (js/parseInt (:year (:object @avto/app-state)))) true false)) years)))
    ]
    (.log js/console "Updating generations")
    (doall (map (fn [x] (let [
      option (js/document.createElement "option")

      ]

      (set! (.-text option) (:text x))
      (set! (.-value option) (:value x))
      (.add select option)
    )) generations))



  (jquery
     (fn []
       (-> (jquery "#generation")
         (.prop "disabled" false)
       )
     )
   )

 

  (jquery
     (fn []
       (-> (jquery "#bodyType")
         (.prop "disabled" false)
       )
     )
   )


  (jquery
     (fn []
       (-> (jquery "#engineType")
         (.prop "disabled" false)
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#gearboxType")
         (.prop "disabled" false)
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#driveType")
         (.prop "disabled" false)
       )
     )
   )

    (jquery
      (fn []
        (-> (jquery "#generation")
          (.selectpicker "refresh")
        )
      )
    )


    (jquery
      (fn []
        (-> (jquery "#generation")
          (.selectpicker "val" (js/parseInt (:generation (:object @avto/app-state))) )
        )
      )
    )
  )
)

(defn createYears []
  (let [
    select (js/document.getElementById "year")
    models (:models (first (filter (fn [x] (if (= (:value x) (js/parseInt (:manufacture (:object @avto/app-state)))) true false)) (:manufactures @avto/app-state))))

    years (:years (first (filter (fn [x] (if (= (:value x) (js/parseInt (:model (:object @avto/app-state)))) true false)) models)))
    ]
    (.log js/console "Updating years")
    (doall (map (fn [x] (let [
      option (js/document.createElement "option")

      ]

      (set! (.-text option) (:text x))
      (set! (.-value option) (:value x))
      (.add select option)
    )) years))

  (jquery
     (fn []
       (-> (jquery "#year")
         (.prop "disabled" false)
       )
     )
   )


    (jquery
      (fn []
        (-> (jquery "#year")
          (.selectpicker "refresh")
        )
      )
    )

    (jquery
      (fn []
        (-> (jquery "#year")
          (.selectpicker "val" (js/parseInt (:year (:object @avto/app-state))) )
        )
      )
    )
  )
)


(defn createModels []
  (let [
    select (js/document.getElementById "model")
    models (:models (first (filter (fn [x] (if (= (:value x) (js/parseInt (:manufacture (:object @avto/app-state)))) true false)) (:manufactures @avto/app-state))))
    ]
    (.log js/console (str "Updating models for manufacture: " (:manufacture (:object @avto/app-state))) )
    (doall (map (fn [x] (let [
      option (js/document.createElement "option")

      ]

      (set! (.-text option) (:text x))
      (set! (.-value option) (:value x))
      (.add select option)
    )) models))


  (jquery
     (fn []
       (-> (jquery "#model" )
         (.prop "disabled" false)
       )
     )
   )
    (jquery
      (fn []
        (-> (jquery "#model")
          (.selectpicker "refresh")
        )
      )
    )

    (jquery
      (fn []
        (-> (jquery "#model")
          (.selectpicker "val" (js/parseInt (:model (:object @avto/app-state))))
        )
      )
    )
  )
)

(defn createRegions []
  (let [
    select (js/document.getElementById "region")
    
    ]
    (.log js/console "Updating regions drop down")
    (doall (map (fn [x] (let [
      option (js/document.createElement "option")

      ]

      (set! (.-text option) (:text x))
      (set! (.-value option) (:value x))
      (.add select option)
    )) (:regions @avto/app-state))) 

  (jquery
     (fn []
       (-> (jquery "#region")
         (.prop "disabled" false)
       )
     )
   )

    (jquery
      (fn []
        (-> (jquery "#region")
          (.selectpicker "refresh")
        )
      )
    )

    (jquery
      (fn []
        (-> (jquery "#region")
          (.selectpicker "val" (:region (:object @avto/app-state)))
        )
      )
    )
  )
)

(defn createManufactures []
  (let [
    select (js/document.getElementById "manufacture")
    
    ]
    (.log js/console "Updating manufactures drop down")
    (doall (map (fn [x] (let [
      option (js/document.createElement "option")

      ]

      (set! (.-text option) (:text x))
      (set! (.-value option) (:value x))
      (.add select option)
    )) (:manufactures @avto/app-state))) 

  (jquery
     (fn []
       (-> (jquery "#manufacture")
         (.prop "disabled" false)
       )
     )
   )

    (jquery
      (fn []
        (-> (jquery "#manufacture")
          (.selectpicker "refresh")
        )
      )
    )

    (jquery
      (fn []
        (-> (jquery "#manufacture")
          (.selectpicker "val" (js/parseInt (:manufacture (:object @avto/app-state))) )
        )
      )
    )
  )
)


(defn clearCharacteristics []
  (let [
    select_bodyType (js/document.getElementById "bodyType")
    select_gearboxType (js/document.getElementById "gearboxType")
    select_engineType (js/document.getElementById "engineType")
    select_driveType (js/document.getElementById "driveType")
    models (:models (first (filter (fn [x] (if (= (:value x) (js/parseInt (:manufacture (:object @avto/app-state)))) true false)) (:manufactures @avto/app-state))))
    years (:years (first (filter (fn [x] (if (= (:value x) (js/parseInt (:model (:object @avto/app-state)))) true false)) models)))

    generations (:generations (first (filter (fn [x] (if (= x (js/parseInt (:year (:object @avto/app-state)))) true false)) years)))
    characteristics (:characteristics (first (filter (fn [x] (if (= x (js/parseInt (:generation (:object @avto/app-state)))) true false)) generations)))
    ]
    (.log js/console "Removing existing characteristics")
    (clearprices)
    (doall (map (fn [x] (let [ 
      val (str "[value=" (:value x) "]")
      option  (-> (jquery "#bodyType")
         (.find val)) 
      ]
      (.log js/console (str "remove bodyType=" val))
      (.remove option)
      
      
      
      )) (:bodyTypes characteristics))
    )

    (doall (map (fn [x] (let [
      val (str "[value=" (:value x) "]")
      option  (-> (jquery "#engineType")
         (.find val))
      ]
      (.remove option)
      
      )) (:engineTypes characteristics))
    )

    (doall (map (fn [x] (let [
      val (str "[value=" (:value x) "]")
      option  (-> (jquery "#gearboxType")
         (.find val))
      ]
      (.remove option)
      
      )) (:gearboxTypes characteristics))
    )

    (doall (map (fn [x] (let [
      val (str "[value=" (:value x) "]")
      option  (-> (jquery "#driveType")
         (.find val))
      ]
      (.remove option)
      
      )) (:driveTypes characteristics))
    )

  )
)

(defn clearGenerations []
  (let [
    select (js/document.getElementById "generation")
    models (:models (first (filter (fn [x] (if (= (:value x) (js/parseInt (:manufacture (:object @avto/app-state)))) true false)) (:manufactures @avto/app-state))))
    years (:years (first (filter (fn [x] (if (= (:value x) (js/parseInt (:model (:object @avto/app-state)))) true false)) models)))

    generations (:generations (first (filter (fn [x] (if (= (:value x) (js/parseInt (:year (:object @avto/app-state)))) true false)) years)))
    ]
    (.log js/console (str "Removing existing generations: " (count generations))  )
    
    (clearprices)
    (clearCharacteristics)
    (doall (map (fn [x] (let [
      val (str "[value=" (:value x) "]")
      option  (-> (jquery "#generation")
         (.find val)) 
      ]
      (.log js/console (str "generation option=" val))
      (.remove option)
    )) generations))


  (jquery
     (fn []
       (-> (jquery "#generation")
         (.prop "disabled" true)
       )
     )
   )
  )

  (jquery
     (fn []
       (-> (jquery "#bodyType")
         (.prop "disabled" true)
       )
     )
   )


  (jquery
     (fn []
       (-> (jquery "#engineType")
         (.prop "disabled" true)
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#gearboxType")
         (.prop "disabled" true)
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#driveType")
         (.prop "disabled" true)
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#bodyType" )
         (.selectpicker "refresh")
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#gearboxType" )
         (.selectpicker "refresh")
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#driveType" )
         (.selectpicker "refresh")
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#engineType" )
         (.selectpicker "refresh")
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#generation" )
         (.selectpicker "refresh")
       )
     )
   )
)


(defn clearYears []
  (let [
    select (js/document.getElementById "model")
    models (:models (first (filter (fn [x] (if (= (:value x) (js/parseInt (:manufacture (:object @avto/app-state)))) true false)) (:manufactures @avto/app-state))))
    years (:years (first (filter (fn [x] (if (= (:value x) (js/parseInt (:model (:object @avto/app-state)))) true false)) models)))
    ]
    (.log js/console "Removing existing years")
    (clearprices)
    (clearGenerations)
    (clearCharacteristics)
    (doall (map (fn [x] (let [
      val (str "[value=" (:value x) "]")
      option  (-> (jquery "#year")
         (.find val))
      ]
      ;(.log js/console (str "option = " val))
      (.remove option)
      
  
  
    )) years))

  (jquery
     (fn []
       (-> (jquery "#year")
         (.prop "disabled" true)
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#generation")
         (.prop "disabled" true)
       )
     )
   )


  (jquery
     (fn []
       (-> (jquery "#bodyType")
         (.prop "disabled" true)
       )
     )
   )


  (jquery
     (fn []
       (-> (jquery "#engineType")
         (.prop "disabled" true)
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#gearboxType")
         (.prop "disabled" true)
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#driveType")
         (.prop "disabled" true)
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#year" )
         (.selectpicker "refresh")
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#generation" )
         (.selectpicker "refresh")
       )
     )
   )


  (jquery
     (fn []
       (-> (jquery "#bodyType" )
         (.selectpicker "refresh")
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#gearboxType" )
         (.selectpicker "refresh")
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#driveType" )
         (.selectpicker "refresh")
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#engineType" )
         (.selectpicker "refresh")
       )
     )
   )

  )
)



(defn clearModels []
  (let [
    select (js/document.getElementById "model")
    models (:models (first (filter (fn [x] (if (= (:value x) (js/parseInt (:manufacture (:object @avto/app-state)))) true false)) (:manufactures @avto/app-state))))
    ]
    (.log js/console "Removing existing models")
    (clearprices)
    (clearYears)
    (clearGenerations)
    (clearCharacteristics)
    (doall (map (fn [x] (let [
      ;tr1 (.log js/console (str "removing: " (:value x)))
      val (str "[value=" (:value x) "]")
      option  (-> (jquery "#model")
         (.find val)
       )  
      ]
      (.remove option)
      
    )) models))


  (jquery
     (fn []
       (-> (jquery "#model")
         (.prop "disabled" true)
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#year")
         (.prop "disabled" true)
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#generation")
         (.prop "disabled" true)
       )
     )
   )


  (jquery
     (fn []
       (-> (jquery "#bodyType")
         (.prop "disabled" true)
       )
     )
   )


  (jquery
     (fn []
       (-> (jquery "#engineType")
         (.prop "disabled" true)
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#gearboxType")
         (.prop "disabled" true)
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#driveType")
         (.prop "disabled" true)
       )
     )
   )
  (jquery
     (fn []
       (-> (jquery "#model" )
         (.selectpicker "refresh")
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#year" )
         (.selectpicker "refresh")
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#generation" )
         (.selectpicker "refresh")
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#bodyType" )
         (.selectpicker "refresh")
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#gearboxType" )
         (.selectpicker "refresh")
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#driveType" )
         (.selectpicker "refresh")
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#engineType" )
         (.selectpicker "refresh")
       )
     )
   )
  )
)

(defn onDropDownChange [id value]
  (let [
    ;value (if (= id "unit") )
    ]
    (.log js/console (str "manufacture: " (:manufacture (:object @avto/app-state))))
    (case id
      "pledge" (setobjectvals value)
      "model" (clearYears)
      "year" (clearGenerations)
      "generation" (clearCharacteristics)
      (.log js/console (str "DropDown change for id: " id))
    )
    
    (swap! avto/app-state assoc-in [:object (keyword id)] value)
    (case id
      "manufacture" (put! ch 11)
      "model" (put! ch 12)
      "year" (put! ch 13)
      "generation" (put! ch 14)
      (.log js/console (str "DropDown change for: " id))
    )
  )
  ;(.log js/console (str "id=" id "; value=" value))
)

(defn setDropDowns []
  (jquery
    (fn []
      (-> (jquery "#pledge" )
        (.selectpicker {})
      )
    )
  )
  (jquery
    (fn []
      (-> (jquery "#generation" )
        (.selectpicker {})
      )
    )
  )

  (jquery
    (fn []
      (-> (jquery "#driveType" )
        (.selectpicker {})
      )
    )
  )

  (jquery
    (fn []
      (-> (jquery "#gearboxType" )
        (.selectpicker {})
      )
    )
  )


  (jquery
    (fn []
      (-> (jquery "#engineType" )
        (.selectpicker {})
      )
    )
  )

  (jquery
    (fn []
      (-> (jquery "#bodyType" )
        (.selectpicker {})
      )
    )
  )


  (jquery
     (fn []
       (-> (jquery "#year")
         (.selectpicker {})
       )
     )
   )

  (jquery
     (fn []
       (-> (jquery "#manufacture")
         (.selectpicker {})
       )
     )
   )

   (jquery
     (fn []
       (-> (jquery "#model")
         (.selectpicker "val" (js/parseInt (:model (:object @avto/app-state))) )
         (.on "change"
           (fn [e]
             (onDropDownChange (.. e -target -id) (.. e -target -value))
               ;(.log js/console e)
           )
         )
       )
     )
   )

   (jquery
     (fn []
       (-> (jquery "#year")
         (.selectpicker "val" (js/parseInt (:year (:object @avto/app-state))) )
         (.on "change"
           (fn [e]
             (onDropDownChange (.. e -target -id) (.. e -target -value))
               ;(.log js/console e)
           )
         )
       )
     )
   )

   (jquery
     (fn []
       (-> (jquery "#pledge")
         (.selectpicker "val" (:pledge (:object @avto/app-state)))
         (.on "change"
           (fn [e]
             (onDropDownChange (.. e -target -id) (.. e -target -value))
               ;(.log js/console e)
           )
         )
       )
     )
   )

   (jquery
     (fn []
       (-> (jquery "#manufacture")
         (.selectpicker "val" (js/parseInt (:manufacture (:object @avto/app-state))))
         (.on "change"
           (fn [e]
             (onDropDownChange (.. e -target -id) (.. e -target -value))
               ;(.log js/console e)
           )
         )
       )
     )
   )

   (jquery
     (fn []
       (-> (jquery "#generation")
         (.selectpicker "val" (js/parseInt (:generation (:object @avto/app-state))))
         (.on "change"
           (fn [e]
             (onDropDownChange (.. e -target -id) (.. e -target -value))
               ;(.log js/console e)
           )
         )
       )
     )
   )


   (jquery
     (fn []
       (-> (jquery "#driveType")
         ;(.selectpicker "val" (:driveType (:object @avto/app-state)))
         (.on "change"
           (fn [e]
             (onDropDownChange (.. e -target -id) (.. e -target -value))
               ;(.log js/console e)
           )
         )
       )
     )
   )

   (jquery
     (fn []
       (-> (jquery "#gearboxType")
         ;(.selectpicker "val" (:gearboxType (:object @avto/app-state)))
         (.on "change"
           (fn [e]
             (onDropDownChange (.. e -target -id) (.. e -target -value))
               ;(.log js/console e)
           )
         )
       )
     )
   )

   (jquery
     (fn []
       (-> (jquery "#engineType")
         ;(.selectpicker "val" (:engineType (:object @avto/app-state)))
         (.on "change"
           (fn [e]
             (onDropDownChange (.. e -target -id) (.. e -target -value))
               ;(.log js/console e)
           )
         )
       )
     )
   )

   (jquery
     (fn []
       (-> (jquery "#bodyType")
         ;(.selectpicker "val" (:bodyType (:object @avto/app-state)))
         (.on "change"
           (fn [e]
             (onDropDownChange (.. e -target -id) (.. e -target -value))
               ;(.log js/console e)
           )
         )
       )
     )
   )
)

(defn setManufacturesDropDown []
  (createManufactures)
)

(defn setcontrols [value]
  (case value
    46 (setDropDowns)
    56 (setManufacturesDropDown)
    57 (createModels)
    58 (createYears)
    59 (createGenerations)
    60 (createCharacteristics)
    61 (createRegions)

    11 (getModels)
    12 (getYears)
    13 (getGenerations)
    14 (getCharacteristics)
    43 (go
         (<! (timeout 100))

       )

    44 (swap! avto/app-state assoc-in [:showmap] -1)
  )
)

(defn initqueue []
  (doseq [n (range 1000)]
    (go ;(while true)
      (take! ch(
        fn [v] (
           ;.log js/console v
           ;(setcalculatedfields) 
           setcontrols v
           
           ;(.log js/console v)  
          )
        )
      )
    )
  )
)


(initqueue)

(defn array-to-string [element]
  (let [
      newdata {:empname (get element "empname") } 
    ]
    (:empname newdata)
  )
)



(defn OnError [response]
  (let [     
      newdata { :error (get (:response response)  "error") }
    ]
    (.log js/console (str  response )) 
    
  )
  
  
)

(defn handleFromChange [e]
  ;;(.log js/console e  )  
  ;(.log js/console "The change ....")

)





(defn handle-change [e owner]
  ;(.log js/console e)
  (swap! avto/app-state assoc-in [:object (keyword (.. e -target -id))] 
    (.. e -target -value)
  )

  (.log js/console "jhghghghj")
)


(defn comp-analogs
  [analog1 analog2]
  (if (> (compare (:address analog1) (:address analog2)) 0)
      false
      true
  )
)

(defcomponent showanalogs-view [data owner]
  (render
    [_
      
    ]
    (let [
      ;tr1 (.log js/console data)
      ]
      (if (> (count (:analogs (:object @avto/app-state))) 0)
        (dom/div {:className "panel panel-primary"}
          (dom/div {:className "panel panel-heading" :style {:margin-bottom "0px"}}
            (dom/div {:className "row"} 
              (dom/div {:className "col-xs-3  col-xs-offset-1" :style {:text-align "center"}}
                "Адрес"
              )
              (dom/div {:className "col-xs-1"}
                "Тип дома"
              )
              (dom/div {:className "col-xs-1" :style {:text-align "center"}}
                "Общая площадь"
              )
              (dom/div {:className "col-xs-1" :style {:text-align "center"}}
                "Год постройки"
              )
              (dom/div {:className "col-xs-1" :style {:text-align "center"}}
                "Цена"
              )
            )
          )
          (dom/div {:className "panel panel-body" :style {:padding-top "0px"}}
            (map (fn [item]
              (let [ 
                ]
                (dom/div {:className "row tablerow" :style {:margin-right "0px" :margin-left "-16px"}}
                  (dom/div {:className "col-xs-3  col-xs-offset-1" :style {:text-align "left" :border "1px solid lightgrey" :padding-top "6px" :padding-bottom "6px"}}
                    (dom/h4 {:className "list-group-item-heading" :style {:font-weight "normal" :white-space "nowrap"}} (:address item))

                  )

                  (dom/div {:className "col-xs-1" :style {:text-align "left" :border "1px solid lightgrey" :padding-top "6px" :padding-bottom "6px"}}
                    (dom/h4 {:className "list-group-item-heading" :style {:font-weight "normal" :white-space "nowrap"}} (:housetype item))
                  )
                  (dom/div {:className "col-xs-1" :style {:text-align "right" :border "1px solid lightgrey" :padding-top "6px" :padding-bottom "6px"}}
                    (dom/h4 {:className "list-group-item-heading" :style {:font-weight "normal" :white-space "nowrap"}} (:totalarea item))
                  )
                  (dom/div {:className "col-xs-1" :style {:text-align "right" :border "1px solid lightgrey" :padding-top "6px" :padding-bottom "6px"}}
                    (dom/h4 {:className "list-group-item-heading" :style {:font-weight "normal" :white-space "nowrap"}} (:buildyear item))
                  )
                  (dom/div {:className "col-xs-1" :style {:text-align "right" :border "1px solid lightgrey" :padding-top "6px" :padding-bottom "6px"}}
                    (dom/h4 {:className "list-group-item-heading" :style {:font-weight "normal" :white-space "nowrap"}} (avto/split-thousands (gstring/format "%.0f" (:price item))))
                  )
                )
              )
              )(sort (comp comp-analogs) (:analogs (:object @avto/app-state)))
            )
          )
        )
        (dom/div )
      )
    )
  )
)


(defn map-manufacture [manufacture]
  (let [
    ]
    ;
    {:value (get manufacture "value") :text (get manufacture "text")}
  )
)

(defn returnPrice [price]
  (let [thePrice (js/parseFloat price)]
    (str (* (+ (/ (Math.log10 (/ (min (max (:horsepower (:object @avto/app-state)) 50.0) 270.0) 100.0)) 3.0) 1.0)  (- 1 (/ (Math.log10 (/ (min (max 10000 (:run (:object @avto/app-state))) 1000000 )  100000.0)) 3.0)) thePrice) )
  )
)


(defn OnGetData [response]
  (let[]
    (swap! avto/app-state assoc-in [:object :onemonthprice] (returnPrice (get response "oneMonthPrice")))
    (swap! avto/app-state assoc-in [:object :onedayprice] (returnPrice (get response "oneDayPrice")))
    (swap! avto/app-state assoc-in [:object :twomonthprice] (returnPrice (get response "twoMonthPrice")))
    (swap! avto/app-state assoc-in [:object :minprice] (returnPrice (get (get response "analoguePriceRange") "min")))
    (swap! avto/app-state assoc-in [:object :maxprice] (returnPrice (get (get response "analoguePriceRange") "max")))
    ;(swap! avto/app-state assoc-in [:object :analogs] (map map-analog (get response "analogs")))
    (swap! avto/app-state assoc-in [:state] 0)
    ;;(.log js/console response)
  )
)


(defn OnGetRegions [response]
  (let[]
    (swap! avto/app-state assoc-in [:regions] (map map-item response))
    (swap! avto/app-state assoc-in [:state] 0)


    
    (put! ch 61)
    ;;(.log js/console response)
  )
)

(defn OnGetManufactures [response]
  (let[]
    (swap! avto/app-state assoc-in [:manufactures] (map map-manufacture response))
    (swap! avto/app-state assoc-in [:state] 0)


    
    (put! ch 56)
    ;;(.log js/console response)
  )
)

(defn requestRegions []
  (let [
    ]
   
    (swap! avto/app-state assoc-in [:state] 1)
    (GET (str settings/apipath "address/capitals") {
      :handler OnGetRegions
      :error-handler error-handler
      :response-format :json
      :headers {(keyword "X-Requested-With")  "XMLHttpRequest"
      }
    })
  )
)

(defn requestManufactures []
  (let [
    ;status (js/parseInt (:statuses (:filter @app-state)))
    ;user (:user (:filter @app-state))
    ]
   
    (swap! avto/app-state assoc-in [:state] 1)
    (GET (str settings/apipath "taxonomy/makes") {
      :handler OnGetManufactures
      :error-handler error-handler
      :response-format :json
      :headers {(keyword "X-Requested-With")  "XMLHttpRequest"
      }
    })
  )
)

(defn getManufactures []
  (if (= (count (:manufactures @avto/app-state)) 0)
    (requestManufactures)
    (put! ch 56)
  )
)


(defn getRegions []
  (if (= (count (:regions @avto/app-state)) 0)
    (requestRegions)
    (put! ch 61)
  )
)


(defn getdata []
  (let [
    ;status (js/parseInt (:statuses (:filter @app-state)))
    ;user (:user (:filter @app-state))
    ]
    (swap! avto/app-state assoc-in [:state] 1)
    (POST (str settings/apipath "appraisal") {
      :handler OnGetData
      :error-handler error-handler
      :response-format :json
      :format :json
      :params {:bodyType (:bodyType (:object @avto/app-state)) :drive (:driveType (:object @avto/app-state)) :engineType (:engineType (:object @avto/app-state)) :gearbox (:gearboxType (:object @avto/app-state)) :generationId (js/parseInt (:generation (:object @avto/app-state))) :horsePower (:horsepower (:object @avto/app-state)) :manufactureYear (js/parseInt (:year (:object @avto/app-state))) :regionId (js/parseInt (:region (:object @avto/app-state))) :run (:run (:object @avto/app-state))}
    })
  )
)


(defn buildManufactures [data owner]
  (let []
    (.log js/console (count (:manufactures @avto/app-state)))
    (map
      (fn [item]
        (dom/option {:key (:value item)  :value (:value item)} (:text item))
      )
      (:manufactures @avto/app-state) 
    )
  )
)

(defn buildModels [data owner]
  (map
    (fn [item]
      (dom/option {:key (:value item)  :value (:value item)} (:text item))
    )
    (:models (first (filter (fn [x] (if (= (js/parseInt (:manufacture (:object @avto/app-state))) (:value x)) true false)) (:manufactures @avto/app-state))))
  )
)


(defn buildYears [data owner]
  (let [
    models (:models (first (filter (fn [x] (if (= (js/parseInt (:manufacture (:object @avto/app-state))) (:value x)) true false)) (:manufactures @avto/app-state))))

    years (:years (first (filter (fn [x] (if (= (js/parseInt (:model (:object @avto/app-state))) (:value x)) true false)) models)))
    ]
    (map
      (fn [item]
        (dom/option {:key (:value item)  :value (:value item)} (:text item))
      )
      years
    )
  )
)


(defn build_pledges [data owner]
  (let [
    ]
    (map
      (fn [item]
        (dom/option {:key (:value item)  :value (:value item)} (:text item))
      )
      pledges
    )
  )
)

(defn builddriveTypes [data owner]
  (let [
    models (:models (first (filter (fn [x] (if (= (js/parseInt (:manufacture (:object @avto/app-state))) (:value x)) true false)) (:manufactures @avto/app-state))))

    years (:years (first (filter (fn [x] (if (= (js/parseInt (:model (:object @avto/app-state))) (:value x)) true false)) models)))

    generations (:generations (first (filter (fn [x] (if (= (:value x) (:year (:object @avto/app-state))) true false)) years)))

   characteristics (:characteristics (first (filter (fn [x] (if (= (:value x) (:generation (:object @avto/app-state))) true false)) generations)))
    ]
    (map
      (fn [item]
        (dom/option {:key (:value item)  :value (:value item)} (:text item))
      )
      (:driveTypes characteristics)
    )
  )
)


(defn buildgearboxTypes [data owner]
  (let [
    models (:models (first (filter (fn [x] (if (= (js/parseInt (:manufacture (:object @avto/app-state))) (:value x)) true false)) (:manufactures @avto/app-state))))

    years (:years (first (filter (fn [x] (if (= (js/parseInt (:model (:object @avto/app-state))) (:value x)) true false)) models)))

    generations (:generations (first (filter (fn [x] (if (= (:value x) (:year (:object @avto/app-state))) true false)) years)))

   characteristics (:characteristics (first (filter (fn [x] (if (= (:value x) (:generation (:object @avto/app-state))) true false)) generations)))
    ]
    (map
      (fn [item]
        (dom/option {:key (:value item)  :value (:value item)} (:text item))
      )
      (:gearboxTypes characteristics)
    )
  )
)


(defn buildbodyTypes [data owner]
  (let [
    models (:models (first (filter (fn [x] (if (= (js/parseInt (:manufacture (:object @avto/app-state))) (:value x)) true false)) (:manufactures @avto/app-state))))

    years (:years (first (filter (fn [x] (if (= (js/parseInt (:model (:object @avto/app-state))) (:value x)) true false)) models)))

    generations (:generations (first (filter (fn [x] (if (= (:value x) (:year (:object @avto/app-state))) true false)) years)))

   characteristics (:characteristics (first (filter (fn [x] (if (= (:value x) (:generation (:object @avto/app-state))) true false)) generations)))
    ]
    (map
      (fn [item]
        (dom/option {:key (:value item)  :value (:value item)} (:text item))
      )
      (:bodyTypes characteristics)
    )
  )
)

(defn buildGenerations [data owner]
  (let [
    models (:models (first (filter (fn [x] (if (= (js/parseInt (:manufacture (:object @avto/app-state))) (:value x)) true false)) (:manufactures @avto/app-state))))

    years (:years (first (filter (fn [x] (if (= (js/parseInt (:model (:object @avto/app-state))) (:value x)) true false)) models)))

    generations (:generations (first (filter (fn [x] (if (= (:value x) (:year (:object @avto/app-state))) true false)) years)))
    ]
    (map
      (fn [item]
        (dom/option {:key (:value item)  :value (:value item)} (:text item))
      )
      generations
    )
  )
)



(defn onMount [data]
  (swap! avto/app-state assoc-in [:current] 
    "Object detail"
  )
  (set! (.-title js/document) "Оценка стоимости автомоюбиля")
  (put! ch 46)
  (swap! avto/app-state assoc-in [:view] 1)
)

(defcomponent devdetail-page-view [data owner]
  (did-mount [_]
    (let [

      ]
      (onMount data)
      (put! ch 44)
    )
  )



  (did-update [this prev-props prev-state]
    ;(.log js/console "Update happened") 

    ;(put! ch 46)
  )
  (render
    [_]
    (let [
      tr1 (.log js/console "in render")
      ]
      (dom/div {:style {:padding-top "10px"}}
        ;(om/build avto/website-view avto/app-state {})
        
        (dom/h3 {:style {:text-align "center"}}
          (dom/i {:className "fa fa-cube"})
          (str "Независимая оценка автомобиля")
        )
        
        (dom/div {:className "row" :style {:width "100%" :margin-left "20px"}}



          (dom/div {:className "row"}
              (dom/div {:className "col-xs-3 col-sm-2 col-xs-offset-0 col-sm-offset-3" :style {:padding-left "0px" :padding-right "0px" :text-align "right"}}
                (dom/h5 (str "Выберите номер залога:"))
              )
              (dom/div {:className "col-xs-6 col-sm-2" :style {:margin-left "10px" :margin-top "4px" :padding-right "0px" :padding-left "0px"}}
                (omdom/select #js {:id "pledge"
                                   :disabled false
                                   :className "selectpicker"
                                   :data-none-selected-text "Выберите"
                                   :data-width "100%"
                                   :data-style "btn-default"
                                   :data-show-subtext "false"
                                   :data-live-search "true"
                                   :onChange #(handle-change % owner)
                                   }                
                  (build_pledges data owner)
                )
              )
            )


            (if (> (count (:model (:object @data))))
              (dom/div {:className "row" :style {:padding-top "10px"}}
                       (dom/div {:className "panel panel-primary"}
                                (dom/div {:className "panel-heading"}
                                         (str "Описание транспортного средства: " (:model (:object @avto/app-state)))
                                         )
                                )
                       )
              
            )

            (if (> (count (:person (:object @data))))
              (dom/div {:className "row" :style {:padding-top "10px"}}
                       (dom/div {:className "panel panel-primary"}
                                (dom/div {:className "panel-heading"}
                                         (str "Владелец, дата рождения: " (:person (:object @avto/app-state)))
                                         )
                                )
                       )
              
            )

            (if (> (count (:passport (:object @data))))
              (dom/div {:className "row" :style { :padding-top "10px"}}
                       (dom/div {:className "panel panel-primary"}
                                (dom/div {:className "panel-heading"}
                                         (str "Паспрортные данные: " (:passport (:object @avto/app-state)))
                                         )
                                )
                       )
              
            )

            (if (> (count (:bank (:object @data))))
              (dom/div {:className "row" :style { :padding-top "10px"}}
                       (dom/div {:className "panel panel-primary"}
                                (dom/div {:className "panel-heading"}
                                         (str "Залогодержатель: " (:bank (:object @avto/app-state)))
                                         )
                                )
                       )
              
            )


            (if (> (count (:person (:object @data))))
              (dom/div {:className "row" :style {:padding-top "10px"}}
                       (dom/div {:className "panel panel-primary"}
                                (dom/div {:className "panel-heading"}
                                         (dom/a {:style {:color "white"} :href (str "http://5.189.157.176/" (:pledge (:object @avto/app-state)) ".pdf")} "Справка")
                                         )
                                )
                       )
              
            )

            (dom/div {:className "row" :style {:padding-top "10px" :display "block"}}
              (dom/div {:className "col-xs-3 col-sm-1 col-xs-offset-0 col-sm-offset-4"}

                (b/button {:className (if (= (:state @data) 0) "btn btn-primary" "btn btn-primary m-progress") :disabled? (if (and (> (count (:generation (:object @avto/app-state))) 0) (> (count (:pledge (:object @avto/app-state))) 0) (> (count (:year (:object @avto/app-state))) 0)) false true) :onClick (fn [e] (getdata))} "Получить стоимость: ")
               )

            )

            (if (> (:onemonthprice (:object @data)) 0.0)
              (dom/div {:style {:display (if (= (:state @data) 0) "block" "none")}}
                (dom/div {:className "row" :style {:display (if (= 0.0 (:data (:object @avto/app-state))) "none" "block") :padding-top "10px"}}
                  (dom/div {:className "panel panel-primary"}
                    (dom/div {:className "panel-heading"}
                      (str "Цена в течение месяца: " (avto/split-thousands (gstring/format "%.2f" (:onemonthprice (:object @avto/app-state)))))
                    )
                  )
                )
              )
            )

            (if (> (:twomonthprice (:object @data)) 0.0)
              (dom/div {:style {:display (if (= (:state @data) 0) "block" "none")}}
                (dom/div {:className "row" :style {:display (if (= 0.0 (:data (:object @avto/app-state))) "none" "block") :padding-top "10px"}}
                  (dom/div {:className "panel panel-primary"}
                    (dom/div {:className "panel-heading"}
                      (str "Цена в течение двух месяцев: " (avto/split-thousands (gstring/format "%.2f" (:twomonthprice (:object @avto/app-state)))))
                    )
                  )
                )
              )
            )

            (if (> (:onedayprice (:object @data)) 0.0)
              (dom/div {:style {:display (if (= (:state @data) 0) "block" "none")}}
                (dom/div {:className "row" :style {:display (if (= 0.0 (:data (:object @avto/app-state))) "none" "block") :padding-top "10px"}}
                  (dom/div {:className "panel panel-primary"}
                    (dom/div {:className "panel-heading"}
                      (str "Цена в течение дня: " (avto/split-thousands (gstring/format "%.2f" (:onedayprice (:object @avto/app-state)))))
                    )
                  )
                )
              )
            )

            (if (> (:minprice (:object @data)) 0.0)
              (dom/div {:style {:display (if (= (:state @data) 0) "block" "none")}}
                (dom/div {:className "row" :style {:display (if (= 0.0 (:data (:object @avto/app-state))) "none" "block") :padding-top "10px"}}
                  (dom/div {:className "panel panel-primary"}
                    (dom/div {:className "panel-heading"}
                      (str "Минимальная цена подобных авто " (avto/split-thousands (gstring/format "%.2f" (:minprice (:object @avto/app-state)))))
                    )
                  )
                )
              )
            )

            (if (> (:maxprice (:object @data)) 0.0)
              (dom/div {:style {:display (if (= (:state @data) 0) "block" "none")}}
                (dom/div {:className "row" :style {:display (if (= 0.0 (:data (:object @avto/app-state))) "none" "block") :padding-top "10px"}}
                  (dom/div {:className "panel panel-primary"}
                    (dom/div {:className "panel-heading"}
                      (str "Максимальная цена подобных авто " (avto/split-thousands (gstring/format "%.2f" (:maxprice (:object @avto/app-state)))))
                    )
                  )
                )
              )
            )
            ;; (dom/div
            ;;   (b/button {:className "btn btn-primary colbtn" :onClick (fn [e] (aset js/window "location" (str "#/groupstounit/" (:id (:device @data)))))} "Assign to groups")
            ;; )
            ;(om/build parentgroups-view data {})



        )
        (om/build showanalogs-view  data {})
      )


    )
  )
)





(sec/defroute devdetail-page "/vin" []
  (let[

    ]

    (swap! avto/app-state assoc-in [:view] 1)
    (om/root devdetail-page-view
             avto/app-state
             {:target (. js/document (getElementById "app"))})

  )
)


(defn vin []
  (-> js/document
      .-location
      (set! "#/vin"))
  (sec/dispatch! "/vin")

  ;;(aset js/window "location" "#/vin")
)

(vin)
