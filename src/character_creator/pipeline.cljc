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

  Updated (/loop maturity pass): the 'body' part is no longer unskinned —
  `character.body/skin-body` attaches real `JOINTS_0`/`WEIGHTS_0` (inverse-
  distance auto-skinning to the rest-pose skeleton, see that namespace's
  docstring), and this pipeline now writes a real glTF `skins` entry
  (inverse-bind matrices from `character.body/bone-world-positions`) and
  references it from the body mesh's node. The 'neck + upper body' bust
  extent itself (no legs/lower-arms/hands) is still real and unchanged —
  full-body geometry extrusion is separate follow-up work on
  `character.body`, not something this pipeline can add.

  Updated again (/loop maturity pass, accessories): `:character/equip`/
  `:character/decals` (`character-creator.accessories`) resolve into extra
  `MeshPart`s here, positioned via the SAME `bone-world-positions` scheme
  every other part already uses (no second positioning system). Their
  materials are synthesized locally (`accessory-material`/`decal-material`)
  from each catalog entry's flat `:base-color`, extending the existing
  `character.material`-derived material array rather than replacing it."
  (:require [character :as character]
            [character.body :as cbody]
            [character.material :as cmat]
            [vrm.gltf-types :as gt]
            [vrm.vrm-types :as vt]
            [vrm.export :as vexport]
            [character-creator.gltf-build :as gb]
            [character-creator.expression-bridge :as expr-bridge]
            [character-creator.accessories :as acc]))

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

(defn- accessory-material
  [id]
  (let [{:keys [base-color]} (get acc/accessory-catalog id)]
    {:name (name id) :base-color base-color :metallic 0.1 :roughness 0.5 :emission [0.0 0.0 0.0]}))

(defn- decal-material
  [id]
  (let [{:keys [base-color]} (get acc/decal-catalog id)]
    {:name (name id) :base-color base-color :metallic 0.0 :roughness 0.8 :emission [0.0 0.0 0.0]}))

(defn parts->vrm-document
  "`{:parts :skeleton :blendshape-targets}` (+ the source `CharacterDef`,
  for material colours, + a display name, + `equip`/`decals` id vectors —
  `character-creator.accessories` catalog keys) -> a `vrm.vrm-types/vrm-
  document`."
  ([parts-map character-def character-name] (parts->vrm-document parts-map character-def character-name [] []))
  ([{:keys [parts skeleton blendshape-targets]} character-def character-name equip decals]
   (let [{:keys [skin eyes mouth hair clothing]} character-def
        bone-nodes (gb/build-bone-nodes skeleton)
        n-bones (count bone-nodes)
        bone-world-pos (cbody/bone-world-positions (:bones skeleton))
        bone-world-pos-by-name (zipmap (mapv :name (:bones skeleton)) bone-world-pos)
        accessory-parts (vec (keep #(acc/generate-accessory-part % bone-world-pos-by-name) equip))
        decal-parts (vec (keep #(acc/generate-decal-part % bone-world-pos-by-name) decals))
        extra-ids (distinct (into (mapv :material accessory-parts) (mapv :material decal-parts)))
        material-maps (into (mapv #(cmat/for-part % skin eyes mouth hair clothing) material-ids)
                             (mapv (fn [id] (if (contains? acc/accessory-catalog id)
                                              (accessory-material id)
                                              (decal-material id)))
                                   extra-ids))
        all-material-ids (into material-ids extra-ids)
        material-index (zipmap all-material-ids (range))
        parts (vec (remove #(empty? (:vertices %)) (into parts (into accessory-parts decal-parts))))
        [builder mesh-idxs mesh-names]
        (reduce (fn [[b idxs names] part]
                  (let [[b mi] (gb/add-part-mesh b part material-index)]
                    (if mi [b (conj idxs mi) (conj names (:name part))] [b idxs names])))
                [gb/empty-builder [] []]
                parts)
        head-mesh-idx (first mesh-idxs)
        builder (gb/add-morph-targets builder head-mesh-idx blendshape-targets)
        [builder skin-idx] (gb/add-skin builder (vec (range n-bones)) bone-world-pos)
        body-mesh-idx (some #(when (= "body" (second %)) (first %)) (map vector mesh-idxs mesh-names))
        mesh-nodes (mapv (fn [mi] (cond-> {:mesh mi} (= mi body-mesh-idx) (assoc :skin skin-idx)))
                          mesh-idxs)
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
               :skins (:skins builder)
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
      :expressions (expr-bridge/expression-list head-mesh-idx)}))))

(defn character-doc->vrm-document
  "`CharacterDoc` -> `vrm.vrm-types/vrm-document` (the intermediate step —
  useful for Phase 2's render-IR path, which needs the document, not GLB
  bytes). `:character/equip`/`:character/decals` (if present) resolve into
  extra accessory/decal meshes via `character-creator.accessories`."
  [{:character/keys [def name equip decals] :as doc}]
  (parts->vrm-document (character-doc->parts doc) def (or name "Character") (or equip []) (or decals [])))

(defn character-doc->vrm-bytes
  "`CharacterDoc` -> real `.vrm` GLB bytes (a byte-int vector, per `vrm.glb`
  convention)."
  [doc]
  (vexport/export-glb (character-doc->vrm-document doc)))
