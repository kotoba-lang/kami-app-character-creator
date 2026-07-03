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

(defn- primitive->geometry
  "One glTF primitive map -> `{:positions :normals :indices :material :joints
  :weights :uvs}` (the shared per-primitive extraction `mesh-primitives-by-
  name`/`mesh-geometry-by-name` both build on)."
  [vdoc prim]
  (let [{:keys [POSITION NORMAL JOINTS_0 WEIGHTS_0 TEXCOORD_0]} (:attributes prim)]
    (cond-> {:positions (accessor->vec3s vdoc POSITION)
             :normals (accessor->vec3s vdoc NORMAL)
             :indices (mapv int (conv/read-accessor-f32 vdoc (:indices prim)))
             :material (:material prim)}
      JOINTS_0 (assoc :joints (->> (conv/read-accessor-f32 vdoc JOINTS_0) (partition 4) (mapv #(mapv int %))))
      WEIGHTS_0 (assoc :weights (->> (conv/read-accessor-f32 vdoc WEIGHTS_0) (partition 4) (mapv vec)))
      TEXCOORD_0 (assoc :uvs (accessor->vec2s vdoc TEXCOORD_0)))))

(defn mesh-primitives-by-index
  "Like `mesh-primitives-by-name`, but resolves the mesh by its raw index
  into `(:gltf :meshes)` instead of by `:name` — for an arbitrary UPLOADED
  VRM (character-creator.app's file-upload feature), where mesh names are
  whatever the original author's tool wrote and can't be relied on the way
  `character-creator`'s own generated `\"body\"`/`\"hair\"`/... names can.
  Returns `[]` if `mesh-idx` is out of range."
  [vdoc mesh-idx]
  (let [meshes (get-in vdoc [:gltf :meshes])]
    (if (or (nil? mesh-idx) (< mesh-idx 0) (>= mesh-idx (count meshes)))
      []
      (mapv #(primitive->geometry vdoc %) (get-in vdoc [:gltf :meshes mesh-idx :primitives])))))

(defn mesh-primitives-by-name
  "Like `mesh-geometry-by-name`, but returns a vector of geometry maps — ONE
  PER PRIMITIVE, not just the first. Real bug fix (/loop maturity pass,
  ADR-2607031200): every `mesh-*` reader in this namespace used to hardcode
  `:primitives 0`, silently dropping every other primitive. Every
  `character-creator`-generated part happens to be single-primitive
  (`gltf-build/add-part-mesh` always writes exactly one), so this was
  invisible against this app's own output — but a real parsed VRM (VRoid
  Studio's standard export shape) commonly gives a `head` mesh 2-3
  primitives, each with its OWN material (e.g. base skin / eye-white /
  face-decal texture), so reading only primitive 0 is why a real textured
  VRM's face rendered with no painted eyes/mouth even after this session's
  texture-pipeline work landed — confirmed against a real production VRM
  (local-only test asset, never committed; a hand-built synthetic multi-
  primitive fixture reproduces the same shape for this namespace's own
  tests). Returns `[]` if no mesh with that name exists (mirrors
  `mesh-geometry-by-name`'s `nil`-for-missing convention)."
  [vdoc mesh-name]
  (let [meshes (get-in vdoc [:gltf :meshes])
        mesh-idx (first (keep-indexed (fn [i m] (when (= mesh-name (:name m)) i)) meshes))]
    (mesh-primitives-by-index vdoc mesh-idx)))

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
  always carries it on painted meshes).

  Now a thin wrapper (/loop maturity pass) over `mesh-primitives-by-name`,
  returning only the FIRST primitive — backward-compatible for every
  existing caller, which (correctly, for character-creator's own single-
  primitive generated parts) never needed more than one. A real multi-
  primitive mesh (e.g. a parsed VRoid Studio VRM's head) needs
  `mesh-primitives-by-name` instead to see every primitive, not just this
  one."
  [vdoc mesh-name]
  (first (mesh-primitives-by-name vdoc mesh-name)))

(defn material-base-color-texture
  "`material-idx` -> `{:bytes :mime-type}` (see `vrm.convert/read-base-
  color-texture`), or `nil` if `material-idx` is `nil` or that material has
  no embedded baseColorTexture. Takes the index directly (not a mesh name)
  so a caller iterating `mesh-primitives-by-name`'s results can resolve
  each PRIMITIVE's own material — `mesh-base-color-texture`, below, only
  ever resolved a mesh's first-primitive material, the same primitive-0
  blind spot `mesh-geometry-by-name` had."
  [vdoc material-idx]
  (when material-idx
    (conv/read-base-color-texture vdoc material-idx)))

(defn mesh-base-color-texture
  "`mesh-name`'s FIRST primitive's material -> `{:bytes :mime-type}` (see
  `vrm.convert/read-base-color-texture`), or `nil` if the mesh doesn't
  exist, has no material, or that material has no embedded baseColorTexture
  (e.g. every procedurally-generated character-creator part today — this
  is meaningful against a real parsed VRM like VRoid Studio output, which
  paints faces via a texture). Kept for backward compatibility (every
  existing caller only ever dealt with single-primitive meshes); for a
  multi-primitive mesh, resolve each primitive's own texture via
  `mesh-primitives-by-name` + `material-base-color-texture` instead."
  [vdoc mesh-name]
  (material-base-color-texture vdoc (:material (mesh-geometry-by-name vdoc mesh-name))))

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
