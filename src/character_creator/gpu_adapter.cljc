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

(defn- accessor->vec2s
  "Accessor index -> `[[u v] ...]`."
  [vdoc acc-idx]
  (->> (conv/read-accessor-f32 vdoc acc-idx)
       (partition 2)
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

(defn mesh-geometry-by-name
  "Like `mesh-geometry`, but resolves the mesh by its `character/generate-
  character` part `:name` (`\"body\"`/`\"hair\"`/`\"clothing\"`/...) instead of
  a fixed index, since e.g. the `:bald` hair preset produces zero vertices —
  `character-creator.gltf-build/add-part-mesh` skips writing a mesh for it
  entirely (see its own \"empty-vertices guard\" docstring), which shifts
  every subsequent mesh's index. Returns `nil` if no mesh with that name
  exists (the bald-hair case, or any other zero-vertex part) rather than
  throwing — callers (interactive UIs cycling through presets) should treat
  a missing part as \"nothing to draw this frame\", not an error.
  `:joints`/`:weights` are only present if the primitive actually carries
  `JOINTS_0`/`WEIGHTS_0` (skinned parts, currently just \"body\").
  `:uvs` present if the primitive carries `TEXCOORD_0` — new for the /loop
  maturity pass' texture work (`character-creator's` own generated parts
  already write `TEXCOORD_0` via `gltf-build/add-part-mesh`, but nothing
  read it back out until now; a real parsed VRM like VRoid Studio output
  always carries it on painted meshes)."
  [vdoc mesh-name]
  (let [meshes (get-in vdoc [:gltf :meshes])
        mesh-idx (first (keep-indexed (fn [i m] (when (= mesh-name (:name m)) i)) meshes))]
    (when mesh-idx
      (let [prim (get-in vdoc [:gltf :meshes mesh-idx :primitives 0])
            {:keys [POSITION NORMAL JOINTS_0 WEIGHTS_0 TEXCOORD_0]} (:attributes prim)]
        (cond-> {:positions (accessor->vec3s vdoc POSITION)
                 :normals (accessor->vec3s vdoc NORMAL)
                 :indices (mapv int (conv/read-accessor-f32 vdoc (:indices prim)))
                 :material (:material prim)}
          JOINTS_0 (assoc :joints (->> (conv/read-accessor-f32 vdoc JOINTS_0) (partition 4) (mapv #(mapv int %))))
          WEIGHTS_0 (assoc :weights (->> (conv/read-accessor-f32 vdoc WEIGHTS_0) (partition 4) (mapv vec)))
          TEXCOORD_0 (assoc :uvs (accessor->vec2s vdoc TEXCOORD_0)))))))

(defn mesh-base-color-texture
  "`mesh-name`'s sole primitive's material -> `{:bytes :mime-type}` (see
  `vrm.convert/read-base-color-texture`), or `nil` if the mesh doesn't
  exist, has no material, or that material has no embedded baseColorTexture
  (e.g. every procedurally-generated character-creator part today — this
  is meaningful against a real parsed VRM like VRoid Studio output, which
  paints faces via a texture)."
  [vdoc mesh-name]
  (let [meshes (get-in vdoc [:gltf :meshes])
        mesh-idx (first (keep-indexed (fn [i m] (when (= mesh-name (:name m)) i)) meshes))]
    (when mesh-idx
      (let [prim (get-in vdoc [:gltf :meshes mesh-idx :primitives 0])]
        (when-let [mat-idx (:material prim)]
          (conv/read-base-color-texture vdoc mat-idx))))))

(defn body-mesh-geometry
  "`\"body\"`'s sole primitive -> `{:positions :normals :indices :joints
  :weights}` (`:joints`/`:weights` are per-vertex 4-tuples, glTF JOINTS_0/
  WEIGHTS_0 convention). Real now that `character.body/skin-body` attaches
  weights and `character-creator.gltf-build/add-part-mesh` writes them
  (/loop maturity pass, ADR-2607031200) — Phase 2 predates this and had no
  skinned real-avatar geometry to read. The body mesh always exists (unlike
  hair), so this throws (via destructuring `nil`) rather than returning `nil`
  if somehow absent — callers can rely on it."
  [vdoc]
  (mesh-geometry-by-name vdoc "body"))

(defn hair-mesh-geometry
  "`\"hair\"`'s sole primitive -> `{:positions :normals :indices}`, or `nil`
  for `:bald` (zero vertices, no mesh written — see `mesh-geometry-by-name`).
  New for the /loop maturity pass' interactive character-creator screen: the
  hair carousel changes this mesh, so it must actually be read+drawn for the
  preset change to be visible (Phase 2's demo never drew hair at all, only
  head+body)."
  [vdoc]
  (mesh-geometry-by-name vdoc "hair"))

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
