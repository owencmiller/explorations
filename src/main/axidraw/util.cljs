(ns axidraw.util
  (:require
    [goog.dom :as dom]))


(def A3_WIDTH 297)
(def A3_HEIGHT 420)
(def A3_RATIO (/ A3_HEIGHT A3_WIDTH))
(def A3_CENTER [(/ A3_WIDTH 2) (/ A3_HEIGHT 2)])
(def DISPLAY_WIDTH 600)
(def DISPLAY_HEIGHT (* DISPLAY_WIDTH A3_RATIO))


(def black-line-style
  {:stroke "black"
   :stroke-width "1px"
   :fill "none"})


(defn download-svg
  [svg-content filename]
  (let [el (dom/createDom "a" #js {:href (str "data:image/svg+xml;charset=utf-8," (js/encodeURI svg-content))
                                   :download filename
                                   :style "display:none;"})]
    (.appendChild js/document.body el)
    (.click el)
    (.removeChild js/document.body el)))
