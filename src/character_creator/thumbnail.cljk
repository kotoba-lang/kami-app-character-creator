(ns character-creator.thumbnail
  "A cheap, asset-free 2D preview for the character picker grid — per
  ADR-2607031200 Phase 1, *not* the WGSL live-3D preview (that's Phase 2).
  Philosophically follows `kami-isekai-assets`'s `kami.isekai.chargen`
  ('composed primitives, not asset files'), but doesn't take a hard
  dependency on that repo — this is a handful of flat shapes, not worth a
  cross-repo coupling. Deliberately minimal: enough to tell characters
  apart in a grid, not a portrait renderer."
  )

(defn character-doc->sprite
  "`CharacterDoc` -> a vector of flat 2D primitives (`{:shape :circle|:rect
  :cx :cy ... :color}`, a 100x100 canvas, head-and-shoulders framing) for a
  quick front-view thumbnail."
  [{:character/keys [def palette]}]
  (let [{:keys [skin hair eye]} palette
        shoulder-w (+ 40.0 (* (get-in def [:body :shoulder-width] 0.4) 30.0))
        build (get-in def [:body :build] 0.3)]
    [{:shape :rect :cx 50 :cy 82 :w shoulder-w :h (+ 35.0 (* build 15.0)) :color skin}
     {:shape :circle :cx 50 :cy 35 :r 22 :color skin}
     {:shape :circle :cx 50 :cy 24 :r 24 :color hair}
     {:shape :circle :cx 43 :cy 36 :r 3 :color eye}
     {:shape :circle :cx 57 :cy 36 :r 3 :color eye}]))
