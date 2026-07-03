(ns character-creator.gpu-adapter
  "CharacterDoc's in-memory `VrmDocument` (`character-creator.pipeline`) -> plain
  CPU-side vertex/morph arrays, ready for a WebGPU executor to upload. New for
  ADR-2607031200 Phase 2 — the missing link between the accessor *reader*
  (`vrm.convert/read-accessor-f32`, restored 1:1 from `kami-vrm`) and an actual
  GPU mesh renderer, which does not exist anywhere in `kotoba-lang` yet (neither
  `kami.webgpu.cljs`, whose `draw!` only renders instanced procedural primitives
  from `kami.webgpu.ir`, nor `kotoba.webgpu-rs.render-ir/parse-mesh`, whose
  `:meshes` entries are transform+material+skin *bindings* with a `:url` for a
  host loader to resolve — no such loader exists, and it carries no vertex data
  itself). This namespace + `kami.webgpu.mesh` (in `kotoba-lang/webgpu`) are that
  missing loader/renderer pair, scoped to what Phase 1's generated avatar
  actually has: a morphable, unskinned head mesh."
  (:require [vrm.convert :as conv]
            [character.blendshape :as blendshape]))

(defn- accessor->vec3s
  "Accessor index -> `[[x y z] ...]`."
  [vdoc acc-idx]
  (->> (conv/read-accessor-f32 vdoc acc-idx)
       (partition 3)
       (mapv vec)))

(defn mesh-geometry
  "`mesh-idx`'s sole primitive -> `{:positions :normals :indices :morph-target-
  names :morph-target-deltas}`. `:morph-target-deltas` is index-aligned with
  `:morph-target-names` (itself index-aligned with `character.blendshape/
  arkit-names`, per how `character-creator.gltf-build/add-morph-targets` wrote
  them) — each entry a `[[dx dy dz] ...]` vector, one delta per base vertex."
  [vdoc mesh-idx]
  (let [prim (get-in vdoc [:gltf :meshes mesh-idx :primitives 0])
        {:keys [POSITION NORMAL]} (:attributes prim)
        targets (:targets prim)]
    {:positions (accessor->vec3s vdoc POSITION)
     :normals (accessor->vec3s vdoc NORMAL)
     :indices (mapv int (conv/read-accessor-f32 vdoc (:indices prim)))
     :morph-target-names (vec (take (count targets) blendshape/arkit-names))
     :morph-target-deltas (mapv (fn [t] (accessor->vec3s vdoc (:POSITION t))) targets)}))

(defn head-mesh-geometry
  "Convenience: `character-creator.pipeline/character-doc->vrm-document`
  always builds the head mesh first (`pipeline.cljc`'s `head-mesh-idx (first
  mesh-idxs)`), so mesh index `0` is the morphable one."
  [vdoc]
  (mesh-geometry vdoc 0))

(defn preset-weight-vector
  "VRM expression preset keyword -> a full weight vector, index-aligned with
  `morph-target-names` (i.e. with `character.blendshape/arkit-names`), for
  driving `kami.webgpu.mesh`'s morph blend. Named targets not present on
  `morph-target-names` (shouldn't happen — the bridge table only names real
  ARKit targets) are silently ignored rather than throwing, since this is a
  display-layer convenience, not a data-integrity boundary."
  [preset->arkit-weights morph-target-names preset]
  (let [name->idx (into {} (map-indexed (fn [i n] [n i])) morph-target-names)
        base (vec (repeat (count morph-target-names) 0.0))]
    (reduce (fn [v [arkit-name w]]
              (if-let [i (name->idx arkit-name)]
                (assoc v i w)
                v))
            base
            (get preset->arkit-weights preset []))))
