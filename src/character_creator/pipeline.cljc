(ns character-creator.pipeline
  "CharacterDoc -> real `.vrm` GLB bytes.

  ADR-2607031200 Phase 1 sketched this as `generate-character -> vrm.part ->
  vrm.compose -> vrm.humanoid -> vrm.export`. Reading the real `vrm` API
  changed that: `vrm.part/decompose` and `vrm.compose/compose` operate on
  *already-parsed* `VrmDocument`s (they split/merge parts of existing VRM
  files by node/mesh/accessor *index* — the 'swap hair between two VRoid
  avatars' use case). There is no from-scratch document builder in `vrm`,
  because no Rust original ever combined `kami-character`'s raw mesh output
  with `kami-vrm`'s document model. So this pipeline skips `vrm.part`/
  `vrm.compose` entirely and builds a `VrmDocument` directly via `vrm.gltf-
  types`/`vrm.vrm-types`'s own constructors (`character-creator.gltf-build`
  is the adapter) — same target shape, `vrm.export/export-glb` unchanged.

  Known Phase 1 limitation, inherited from `character.body` as restored
  (not introduced here): `generate-body`/`generate-clothing` build a 'neck +
  upper body' bust mesh, and the humanoid skeleton has 13 bones with no
  legs/lower-arms/hands — this generates a portrait-style avatar, not a
  full standing body. A full-body extension is separate follow-up work on
  `character.body` itself, not something this pipeline can add.

  Also Phase 1: exported meshes are unskinned (no `:skins`/JOINTS_0/
  WEIGHTS_0) — `character.body`'s mesh generators don't attach vertex
  weights, and computing them is GPU/pose-preview territory the ADR scoped
  to Phase 2. The humanoid bone *nodes* are still present and mapped via
  the VRMC_vrm `humanoid` extension, so the document is spec-valid; a
  viewer just can't pose-deform this mesh yet."
  (:require [character :as character]
            [character.material :as cmat]
            [vrm.gltf-types :as gt]
            [vrm.vrm-types :as vt]
            [vrm.export :as vexport]
            [character-creator.gltf-build :as gb]
            [character-creator.expression-bridge :as expr-bridge]))

(def ^:private material-ids
  "Every `:material` keyword `character/generate-character`'s parts
  actually use (head=:skin, 2x{eye-white,iris,pupil}, hair, body=:skin,
  clothing) — order fixes the glTF material array indices."
  [:skin :eye-white :iris :pupil :hair :clothing])

(defn character-doc->parts
  "`CharacterDoc` -> `character/generate-character`'s raw output
  (`{:parts :skeleton :blendshape-targets}`)."
  [{:character/keys [def]}]
  (character/generate-character def))

(defn parts->vrm-document
  "`{:parts :skeleton :blendshape-targets}` (+ the source `CharacterDef`,
  for material colours, + a display name) -> a `vrm.vrm-types/vrm-document`."
  [{:keys [parts skeleton blendshape-targets]} character-def character-name]
  (let [{:keys [skin eyes mouth hair clothing]} character-def
        material-maps (mapv #(cmat/for-part % skin eyes mouth hair clothing) material-ids)
        material-index (zipmap material-ids (range))
        parts (vec (remove #(empty? (:vertices %)) parts))
        [builder mesh-idxs]
        (reduce (fn [[b idxs] part]
                  (let [[b mi] (gb/add-part-mesh b part material-index)]
                    [b (if mi (conj idxs mi) idxs)]))
                [gb/empty-builder []]
                parts)
        head-mesh-idx (first mesh-idxs)
        builder (gb/add-morph-targets builder head-mesh-idx blendshape-targets)
        bone-nodes (gb/build-bone-nodes skeleton)
        n-bones (count bone-nodes)
        mesh-nodes (mapv (fn [mi] {:mesh mi}) mesh-idxs)
        all-nodes (into bone-nodes mesh-nodes)
        scene-node-indices (into [0] (map #(+ n-bones %) (range (count mesh-nodes))))
        human-bones (keep (fn [[i bone]]
                             (when-let [kw (vt/str->human-bone-name (:name bone))]
                               (vt/vrm-human-bone kw i)))
                           (map-indexed vector (:bones skeleton)))
        gltf (gt/gltf-document
              {:asset (gt/asset {:generator "kami-app-character-creator"})
               :scene 0
               :scenes [{:nodes scene-node-indices}]
               :nodes all-nodes
               :meshes (:meshes builder)
               :accessors (:accessors builder)
               :bufferViews (:buffer-views builder)
               :buffers [{:byteLength (count (:bin builder))}]
               :materials (mapv gb/material->gltf material-maps)
               :extensionsUsed ["VRMC_vrm"]})]
    (vt/vrm-document
     {:gltf gltf
      :bin (:bin builder)
      :version :v1-0
      :meta (vt/vrm-meta {:name character-name :authors ["kami-app-character-creator"]
                           :avatar-permission "everyone"})
      :humanoid (vt/vrm-humanoid human-bones)
      :expressions (expr-bridge/expression-list head-mesh-idx)})))

(defn character-doc->vrm-document
  "`CharacterDoc` -> `vrm.vrm-types/vrm-document` (the intermediate step —
  useful for Phase 2's render-IR path, which needs the document, not GLB
  bytes)."
  [{:character/keys [def name] :as doc}]
  (parts->vrm-document (character-doc->parts doc) def (or name "Character")))

(defn character-doc->vrm-bytes
  "`CharacterDoc` -> real `.vrm` GLB bytes (a byte-int vector, per `vrm.glb`
  convention)."
  [doc]
  (vexport/export-glb (character-doc->vrm-document doc)))
