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
  actually has: a morphable, unskinned head mesh.

  Node-hierarchy world-transform + skin-palette support (/loop maturity pass,
  spring-bone follow-up): added so an UPLOADED VRM's real skeleton (not the
  procedural character's synthetic bone list) can be posed — needed both to
  feed `vrm.spring/step`'s `node-world` argument and to compute the actual
  per-joint palette matrices `kami.webgpu.mesh/draw!` skins vertices with.
  Before this, the uploaded-VRM render path fed an all-identity joint palette
  (rest pose only, spring bones explicitly deferred at the time)."
  (:require [vrm.convert :as conv]
            [vrm.math :as m]
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

;; ─── node-hierarchy world transforms + skin palettes (spring-bone follow-up) ──

(defn- node-local-matrix
  "One glTF node's local TRS matrix. `translation`/`scale` default per the
  glTF spec (`[0 0 0]`/`[1 1 1]`); `rotation` defaults to identity UNLESS
  `override-rot` is given (a spring-bone override for this node this frame,
  in the same `[x y z w]` shape `vrm.spring/step`'s `overrides` returns)."
  [node override-rot]
  (m/mat4-from-scale-rotation-translation
   (or (:scale node) m/vec3-one)
   (or override-rot (:rotation node) [0.0 0.0 0.0 1.0])
   (or (:translation node) m/vec3-zero)))

(defn node-world-transforms
  "Every node's world matrix, walking the scene graph from `(:scenes
  (:scene gltf))`'s roots down through `:children` — `node-idx -> Mat4`
  (a plain map, not a vector, since a node's own index may exceed how many
  nodes are actually reachable from the active scene, though in practice
  every VRM node is scene-reachable). `overrides` (`{node-idx [x y z w]}`,
  the exact shape `vrm.spring/step` returns, reduced to a map) replaces a
  node's own rest rotation with a spring-bone (or any other pose system's)
  override for this frame — every non-overridden node still gets its own
  rest rotation, so calling this with `{}` gives the rest-pose world
  transforms (what the uploaded-VRM render path used, implicitly, before
  this pass — identity joint MATRICES happened to be wrong for a real
  skinned mesh even at rest, since a real skin's inverse-bind matrices are
  never identity; see `skin-joint-palette` below, which this enables)."
  [vdoc overrides]
  (let [gltf (:gltf vdoc)
        nodes (:nodes gltf)
        scene-idx (or (:scene gltf) 0)
        roots (get-in gltf [:scenes scene-idx :nodes] [])]
    (loop [queue (mapv (fn [i] [i m/mat4-identity]) roots)
           acc {}]
      (if (empty? queue)
        acc
        (let [[node-idx parent-world] (first queue)
              node (get nodes node-idx)
              local (node-local-matrix node (get overrides node-idx))
              world (m/mat4-mul parent-world local)
              children (mapv (fn [c] [c world]) (:children node))]
          (recur (into (vec (rest queue)) children) (assoc acc node-idx world)))))))

(defn skin-joint-palette
  "`skin-idx`'s joint palette — one 16-float column-major Mat4 (a plain
  Clojure vector, the exact shape `kami.webgpu.mesh/draw!`'s `joint-
  matrices` arg wants) per joint, index-aligned with `JOINTS_0`'s values
  (i.e. `(nth (skin-joint-palette ...) j)` is the matrix for local joint
  slot `j`, matching `skins[skin-idx].joints[j]`'s real node). Standard
  skinning formula: `palette[j] = world(joints[j]) * inverseBind[j]` — the
  inverse-bind matrix first moves a vertex from mesh-local space into
  joint `j`'s OWN bind-local space, then the joint's current world matrix
  moves it back out to world space at its current (possibly spring-posed)
  orientation. `node-world` is a `node-idx -> Mat4` map (typically
  `node-world-transforms`'s output)."
  [vdoc skin-idx node-world]
  (let [gltf (:gltf vdoc)
        skin (get-in gltf [:skins skin-idx])
        joints (:joints skin)
        inv-binds (when-let [acc (:inverseBindMatrices skin)]
                    (->> (conv/read-accessor-f32 vdoc acc) (partition 16) (mapv vec)))]
    (mapv (fn [i joint-node]
            (let [world (get node-world joint-node m/mat4-identity)
                  inv-bind (if inv-binds (nth inv-binds i) m/mat4-identity)]
              (m/mat4-mul world inv-bind)))
          (range (count joints))
          joints)))

(defn mesh-node
  "The scene-graph node that references `mesh-idx` (glTF nodes point AT a
  mesh, not the reverse), or `nil` if none does. A mesh's `:skin` (needed
  for `skin-joint-palette`) lives on this NODE, not on the mesh or any of
  its primitives — `primitive->geometry` above only surfaces per-vertex
  `JOINTS_0`/`WEIGHTS_0`, never the skin index itself, since a primitive
  has no way to know it without this lookup."
  [vdoc mesh-idx]
  (let [nodes (get-in vdoc [:gltf :nodes])]
    (first (filter #(= mesh-idx (:mesh %)) nodes))))
