;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.geom.shapes.constraints
  (:require
   [app.common.geom.point :as gpt]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.shapes.common :as gco]
   [app.common.geom.shapes.intersect :as gsi]
   [app.common.geom.shapes.points :as gpo]
   [app.common.geom.shapes.rect :as gpr]
   [app.common.geom.shapes.transforms :as gtr]
   [app.common.math :as mth]
   [app.common.types.modifiers :as ctm]
   [app.common.uuid :as uuid]))

;; Auxiliary methods to work in an specifica axis
(defn other-axis [axis]
  (if (= :x axis) :y :x))

(defn get-delta-start [axis rect tr-rect]
  (if (= :x axis)
    (- (:x1 tr-rect) (:x1 rect))
    (- (:y1 tr-rect) (:y1 rect))))

(defn get-delta-end [axis rect tr-rect]
  (if (= :x axis)
    (- (:x2 tr-rect) (:x2 rect))
    (- (:y2 tr-rect) (:y2 rect))))

(defn get-delta-size [axis rect tr-rect]
  (if (= :x axis)
    (- (:width tr-rect) (:width rect))
    (- (:height tr-rect) (:height rect))))

(defn get-delta-scale [axis rect tr-rect]
  (if (= :x axis)
    (/ (:width tr-rect) (:width rect))
    (/ (:height tr-rect) (:height rect))))

(defn get-delta-center [axis center tr-center]
  (if (= :x axis)
    (- (:x tr-center) (:x center))
    (- (:y tr-center) (:y center))))

(defn get-displacement
  ([axis delta]
   (get-displacement axis delta 0 0))

  ([axis delta init-x init-y]
   (if (= :x axis)
     (gpt/point (+ init-x delta) init-y)
     (gpt/point init-x (+ init-y delta)))))

(defn get-scale [axis scale]
  (if (= :x axis)
    (gpt/point scale 1)
    (gpt/point 1 scale)))

(defn get-size [axis rect]
  (if (= :x axis)
    (:width rect)
    (:height rect)))

(defn right-vector
  [child-points parent-points]
  (let [[p0 p1 p2 _] parent-points
        [_c0 c1 _ _] child-points
        dir-v (gpt/to-vec p0 p1)
        cp (gsi/line-line-intersect c1 (gpt/add c1 dir-v) p1 p2)]
    (gpt/to-vec c1 cp)))

(defn left-vector
  [child-points parent-points]

  (let [[p0 p1 _ p3] parent-points
        [_ _ _ c3] child-points
        dir-v (gpt/to-vec p0 p1)
        cp (gsi/line-line-intersect c3 (gpt/add c3 dir-v) p0 p3)]
    (gpt/to-vec c3 cp)))

(defn top-vector
  [child-points parent-points]

  (let [[p0 p1 _ p3] parent-points
        [c0 _ _ _] child-points
        dir-v (gpt/to-vec p0 p3)
        cp (gsi/line-line-intersect c0 (gpt/add c0 dir-v) p0 p1)]
    (gpt/to-vec c0 cp)))

(defn bottom-vector
  [child-points parent-points]

  (let [[p0 _ p2 p3] parent-points
        [_ _ c2 _] child-points
        dir-v (gpt/to-vec p0 p3)
        cp (gsi/line-line-intersect c2 (gpt/add c2 dir-v) p2 p3)]
    (gpt/to-vec c2 cp)))

(defn center-horizontal-vector
  [child-points parent-points]

  (let [[p0 p1 _ p3] parent-points
        [_ c1 _ _] child-points

        dir-v (gpt/to-vec p0 p1)

        p1c (gpt/add p0 (gpt/scale dir-v 0.5))
        p2c (gpt/add p3 (gpt/scale dir-v 0.5))

        cp (gsi/line-line-intersect c1 (gpt/add c1 dir-v) p1c p2c)]

    (gpt/to-vec c1 cp)))

(defn center-vertical-vector
  [child-points parent-points]
  (let [[p0 p1 p2 _] parent-points
        [_ c1 _ _] child-points

        dir-v (gpt/to-vec p1 p2)

        p3c (gpt/add p0 (gpt/scale dir-v 0.5))
        p2c (gpt/add p1 (gpt/scale dir-v 0.5))

        cp (gsi/line-line-intersect c1 (gpt/add c1 dir-v) p3c p2c)]

    (gpt/to-vec c1 cp)))

(defn start-vector
  [axis child-points parent-points]
  (let [pos-vector
        (cond (= :x axis) left-vector
              (= :y axis) top-vector)]
    (pos-vector child-points parent-points)))

(defn end-vector
  [axis child-points parent-points]
  (let [pos-vector
        (cond (= :x axis) right-vector
              (= :y axis) bottom-vector)]
    (pos-vector child-points parent-points)))

(defn center-vector
  [axis child-points parent-points]
  ((if (= :x axis) center-horizontal-vector center-vertical-vector) child-points parent-points))

(defn displacement
  [before-v after-v]
  (let [angl (gpt/angle-with-other before-v after-v)
        sign (if (mth/close? angl 180) -1 1)
        length (* sign (gpt/length before-v))]
    (if (mth/almost-zero? length)
      after-v
      (gpt/subtract after-v (gpt/scale (gpt/unit after-v) length)))))

(defn side-vector
  [axis [c0 c1 _ c3]]
  (if (= axis :x)
    (gpt/to-vec c0 c1)
    (gpt/to-vec c0 c3)))

(defn side-vector-resize
  [axis [c0 c1 _ c3] start-vector end-vector]
  (if (= axis :x)
    (gpt/to-vec (gpt/add c0 start-vector) (gpt/add c1 end-vector))
    (gpt/to-vec (gpt/add c0 start-vector) (gpt/add c3 end-vector))))

;; Constraint function definitions

(defmulti constraint-modifier (fn [type & _] type))

(defmethod constraint-modifier :start
  [_ axis child-points-before parent-points-before child-points-after parent-points-after]
  (let [start-before (start-vector axis child-points-before parent-points-before)
        start-after  (start-vector axis child-points-after parent-points-after)]
    (ctm/move-modifiers (displacement start-before start-after))))

(defmethod constraint-modifier :end
  [_ axis child-points-before parent-points-before child-points-after parent-points-after]
  (let [end-before  (end-vector axis child-points-before parent-points-before)
        end-after   (end-vector axis child-points-after parent-points-after)]
    (ctm/move-modifiers (displacement end-before end-after))))

(defmethod constraint-modifier :fixed
  [_ axis child-points-before parent-points-before child-points-after parent-points-after parent]
  (let [;; Same as constraint end
        end-before   (end-vector axis child-points-before parent-points-before)
        end-after    (end-vector axis child-points-after parent-points-after)
        start-before (start-vector axis child-points-before parent-points-before)
        start-after  (start-vector axis child-points-after parent-points-after)

        disp-end     (displacement end-before end-after)
        disp-start   (displacement start-before start-after)

        ;; We get the current axis side and grow it on both side by the end+start displacements
        before-vec   (side-vector axis child-points-after)
        after-vec    (side-vector-resize axis child-points-after disp-start disp-end)

        ;; after-vec will contain the side length of the grown side
        ;; we scale the shape by the diference and translate it by the start
        ;; displacement (so its left+top position is constant)
        scale        (/ (gpt/length after-vec) (gpt/length before-vec))

        resize-origin (first child-points-after)
        {:keys [transform transform-inverse]} parent]

    (-> (ctm/empty)
        (ctm/resize (get-scale axis scale) resize-origin transform transform-inverse)
        (ctm/move disp-start))))

(defmethod constraint-modifier :center
  [_ axis child-points-before parent-points-before child-points-after parent-points-after]
  (let [center-before  (center-vector axis child-points-before parent-points-before)
        center-after   (center-vector axis child-points-after parent-points-after)]
    (ctm/move-modifiers (displacement center-before center-after))))

(defmethod constraint-modifier :default [_ _ _ _ _]
  [])

(def const->type+axis
  {:left :start
   :top :start
   :right :end
   :bottom :end
   :leftright :fixed
   :topbottom :fixed
   :center :center
   :scale :scale})

(defn default-constraints-h
  [shape]
  (if (= (:parent-id shape) uuid/zero)
    nil
    (if (= (:parent-id shape) (:frame-id shape))
      :left
      :scale)))

(defn default-constraints-v
  [shape]
  (if (= (:parent-id shape) uuid/zero)
    nil
    (if (= (:parent-id shape) (:frame-id shape))
      :top
      :scale)))

#_(defn bounding-box-parent-transform
  "Returns a bounding box for the child in the same coordinate system
  as the parent.
  Returns a points array"
  [child parent]
  (-> child
      :points
      (gco/transform-points (:transform-inverse parent))
      (gpr/points->rect)
      (gpr/rect->points) ;; Restore to points so we can transform them
      (gco/transform-points (:transform parent))))

(defn normalize-modifiers
  "Before aplying constraints we need to remove the deformation caused by the resizing of the parent"
  [constraints-h constraints-v modifiers child {:keys [transform transform-inverse] :as parent} transformed-child-bounds transformed-parent-bounds]

  (let [child-bb-before (gpo/parent-coords-bounds (:points child) (:points parent))
        child-bb-after  (gpo/parent-coords-bounds transformed-child-bounds transformed-parent-bounds)
        
        scale-x (if (= :scale constraints-h)
                  1
                  (/ (gpo/width-points child-bb-before) (gpo/width-points child-bb-after)))

        scale-y (if (= :scale constraints-v)
                  1
                  (/ (gpo/height-points child-bb-before) (gpo/height-points child-bb-after)))

        tr1 (ctm/modifiers->transform modifiers)
        tr1-inv (gmt/inverse tr1)

        resize-vector (gpt/point scale-x scale-y)
        resize-transform (gmt/multiply tr1 transform)
        resize-transform-inverse (gmt/multiply transform-inverse tr1-inv)
        resize-origin (-> (gpo/origin transformed-child-bounds)
                          (gpt/transform tr1))

        _ (prn "?? " scale-x scale-y)
        ]

    (-> modifiers
        (ctm/resize
         resize-vector
         resize-origin
         resize-transform
         resize-transform-inverse))))

(defn calc-child-modifiers
  [parent child modifiers ignore-constraints parent-bounds transformed-parent-bounds]

  (let [modifiers (ctm/select-child-modifiers modifiers)

        constraints-h
        (if-not ignore-constraints
          (:constraints-h child (default-constraints-h child))
          :scale)

        constraints-v
        (if-not ignore-constraints
          (:constraints-v child (default-constraints-v child))
          :scale)]

    (if (and (= :scale constraints-h) (= :scale constraints-v))
      modifiers

      (let [child-bounds (:points child)
            modifiers (ctm/select-child-modifiers modifiers)
            transformed-child-bounds (gtr/transform-bounds child-bounds modifiers)
            modifiers (normalize-modifiers constraints-h constraints-v modifiers child parent transformed-child-bounds transformed-parent-bounds)
            transformed-child-bounds (gtr/transform-bounds child-bounds modifiers)

            parent-points-before parent-bounds #_(gpo/parent-coords-bounds parent-bounds parent-bounds)
            child-points-before  (gpo/parent-coords-bounds child-bounds parent-bounds)
            parent-points-after  transformed-parent-bounds #_(gpo/parent-coords-bounds transformed-parent-bounds transformed-parent-bounds)
            child-points-after   (gpo/parent-coords-bounds transformed-child-bounds transformed-parent-bounds)

            modifiers-h (constraint-modifier (constraints-h const->type+axis) :x
                                             child-points-before parent-points-before
                                             child-points-after parent-points-after
                                             parent)

            modifiers-v (constraint-modifier (constraints-v const->type+axis) :y
                                             child-points-before parent-points-before
                                             child-points-after parent-points-after
                                             parent)]

        (-> modifiers
            #_(ctm/add-modifiers modifiers-h)
            #_(ctm/add-modifiers modifiers-v))))))
