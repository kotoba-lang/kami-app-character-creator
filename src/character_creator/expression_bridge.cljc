(ns character-creator.expression-bridge
  "Bridge between VRM 1.0's 18 expression presets (`vrm.vrm-types/expression-
  preset-table`) and the 52-target ARKit blendshape vocabulary
  `character.blendshape` generates (`arkit-names`). Gap #2 identified in
  ADR-2607031200's context section: the two restored crates never wired
  these vocabularies together (no Rust original did either — `kami-vrm` and
  `kami-character` were independent crates). `preset->arkit-weights` is the
  one shared EDN table every generated character's expressions reference
  (per the ADR: authored once, not duplicated per `CharacterDoc`)."
  (:require [character.blendshape :as blendshape]
            [vrm.vrm-types :as vt]))

(def preset->arkit-weights
  "VRM 1.0 preset -> `[[arkit-name weight] ...]`. A standard, editable
  approximation (the same kind of mapping VRM-authoring tools like UniVRM
  ship) — not derived from any spec document, since none prescribes it."
  {:happy      [["mouthSmileLeft" 1.0] ["mouthSmileRight" 1.0]
                ["cheekSquintLeft" 0.3] ["cheekSquintRight" 0.3]]
   :angry      [["browDownLeft" 1.0] ["browDownRight" 1.0]
                ["noseSneerLeft" 0.5] ["noseSneerRight" 0.5]
                ["mouthFrownLeft" 0.3] ["mouthFrownRight" 0.3]]
   :sad        [["browInnerUp" 1.0]
                ["mouthFrownLeft" 0.6] ["mouthFrownRight" 0.6]
                ["eyeSquintLeft" 0.3] ["eyeSquintRight" 0.3]]
   :relaxed    [["eyeSquintLeft" 0.2] ["eyeSquintRight" 0.2]
                ["mouthSmileLeft" 0.2] ["mouthSmileRight" 0.2]]
   :surprised  [["eyeWideLeft" 1.0] ["eyeWideRight" 1.0]
                ["browInnerUp" 0.8] ["browOuterUpLeft" 0.8] ["browOuterUpRight" 0.8]
                ["jawOpen" 0.5]]
   :aa         [["jawOpen" 0.6] ["mouthFunnel" 0.1]]
   :ih         [["mouthStretchLeft" 0.3] ["mouthStretchRight" 0.3] ["jawOpen" 0.2]]
   :ou         [["mouthFunnel" 0.6] ["mouthPucker" 0.4]]
   :ee         [["mouthStretchLeft" 0.4] ["mouthStretchRight" 0.4]]
   :oh         [["mouthFunnel" 0.4] ["jawOpen" 0.3]]
   :blink      [["eyeBlinkLeft" 1.0] ["eyeBlinkRight" 1.0]]
   :blink-left [["eyeBlinkLeft" 1.0]]
   :blink-right [["eyeBlinkRight" 1.0]]
   :look-up    [["eyeLookUpLeft" 1.0] ["eyeLookUpRight" 1.0]]
   :look-down  [["eyeLookDownLeft" 1.0] ["eyeLookDownRight" 1.0]]
   :look-left  [["eyeLookOutLeft" 1.0] ["eyeLookInRight" 1.0]]
   :look-right [["eyeLookInLeft" 1.0] ["eyeLookOutRight" 1.0]]
   :neutral    []})

(def ^:private arkit-name->index
  (into {} (map-indexed (fn [i n] [n i])) blendshape/arkit-names))

(defn expression-list
  "Build the VRM `:expressions` vector — one `vrm.vrm-types/vrm-expression`
  per preset in `preset->arkit-weights`, each `:morph-target-binds` pointing
  at `head-mesh-idx`'s morph targets (index-aligned with
  `character.blendshape/arkit-names`, per how `character-creator.gltf-build/
  add-morph-targets` wrote them)."
  [head-mesh-idx]
  (mapv (fn [[preset weights]]
          (vt/vrm-expression
           {:name (vt/expression-preset->str preset)
            :preset preset
            :morph-target-binds
            (mapv (fn [[arkit-name w]]
                    (vt/morph-target-bind head-mesh-idx (arkit-name->index arkit-name) w))
                  weights)}))
        preset->arkit-weights))
