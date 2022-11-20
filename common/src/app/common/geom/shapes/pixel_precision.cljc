;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.geom.shapes.pixel-precision
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes.points :as gpo]
   [app.common.geom.shapes.rect :as gpr]
   [app.common.geom.shapes.transforms :as gtr]
   [app.common.math :as mth]
   [app.common.pages.helpers :as cph]
   [app.common.types.modifiers :as ctm]))

(defn size-pixel-precision
  [modifiers {:keys [points transform transform-inverse] :as shape}]
  (let [origin        (gpo/origin points)
        curr-width    (gpo/width-points points)
        curr-height   (gpo/height-points points)

        path?            (cph/path-shape? shape)
        vertical-line?   (and path? (<= curr-width 0.01))
        horizontal-line? (and path? (<= curr-height 0.01))

        target-width  (if vertical-line? curr-width (max 1 (mth/round curr-width)))
        target-height (if horizontal-line? curr-height (max 1 (mth/round curr-height)))

        ratio-width  (/ target-width curr-width)
        ratio-height (/ target-height curr-height)
        scalev       (gpt/point ratio-width ratio-height)]
    (-> modifiers
        (ctm/resize scalev origin transform transform-inverse))))

(defn position-pixel-precision
  [modifiers {:keys [points]}]
  (let [bounds        (gpr/points->rect points)
        corner        (gpt/point bounds)
        target-corner (gpt/round corner)
        deltav        (gpt/to-vec corner target-corner)]

    (ctm/move modifiers deltav)))

(defn set-pixel-precision
  "Adjust modifiers so they adjust to the pixel grid"
  [modifiers shape]
  (let [move? (ctm/only-move? modifiers)]
    (cond-> modifiers
      (not move?)
      (size-pixel-precision shape)

      :always
      (position-pixel-precision shape))))

(defn adjust-pixel-precision
  [modif-tree objects]
  (let [update-modifiers
        (fn [modif-tree shape]
          (let [modifiers (dm/get-in modif-tree [(:id shape) :modifiers])]
            (if-not (ctm/has-geometry? modifiers)
              modif-tree
              (let [shape (gtr/transform-shape shape modifiers)]
                (-> modif-tree
                    (update-in [(:id shape) :modifiers] set-pixel-precision shape))))))]

    (->> (keys modif-tree)
         (map (d/getf objects))
         (reduce update-modifiers modif-tree))))
