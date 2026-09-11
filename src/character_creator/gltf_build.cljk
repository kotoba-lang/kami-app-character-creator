(ns character-creator.gltf-build
  "Minimal glTF 2.0 document/binary-buffer *writer* — the inverse of
  `vrm.convert`'s accessor *reader*. `kotoba-lang/vrm` (restored 1:1 from the
  deleted `kami-vrm` Rust crate, ADR-2607010930) ships `vrm.part`/`vrm.compose`
  for *swapping parts between existing parsed VRM files*, but nothing that
  builds a fresh glTF document from raw generated geometry
  (`character/generate-character`'s `MeshPart` vectors). This namespace is
  that adapter: plain `{:vertices [{:position :normal :uv}] :indices
  :material}` maps -> glTF accessors/bufferViews/buffer bytes + nodes, ready
  for `vrm.vrm-types/vrm-document` + `vrm.export/export-glb`.

  New code for ADR-2607031200 Phase 1 — not a port of anything, since no
  Rust original ever combined `kami-character`'s mesh generation with
  `kami-vrm`'s document model."
  (:require [vrm.glb :as glb]
            [vrm.gltf-types :as gt]))

;; ── float encode (mirrors vrm/test/vrm_test.cljc's local `f32->bytes`) ──

(defn- u16->le-bytes [n] [(bit-and n 0xFF) (bit-and (bit-shift-right n 8) 0xFF)])

(defn- f32->le-bytes [f]
  (glb/u32->le-bytes
   #?(:clj (Float/floatToIntBits (float f))
      :cljs (let [buf (js/ArrayBuffer. 4) view (js/DataView. buf)]
              (.setFloat32 view 0 f true)
              (bit-or (.getUint8 view 0)
                      (bit-shift-left (.getUint8 view 1) 8)
                      (bit-shift-left (.getUint8 view 2) 16)
                      (bit-shift-left (.getUint8 view 3) 24))))))

(def empty-builder {:bin [] :buffer-views [] :accessors [] :meshes []})

(defn- append-bin
  "Pad `bin` to a 4-byte boundary, append `bytes`, return `[bin byte-offset]`
  (offset is where `bytes` starts, i.e. after padding)."
  [bin bytes]
  (let [pad (mod (- 4 (mod (count bin) 4)) 4)
        bin (into bin (repeat pad 0))
        offset (count bin)]
    [(into bin bytes) offset]))

(defn- vec-min-max [vs n]
  (reduce (fn [[mins maxs] v]
            [(mapv min mins v) (mapv max maxs v)])
          [(vec (first vs)) (vec (first vs))]
          (rest vs)))

(defn write-vecn-accessor
  "Append `vs` (seq of n-component float vectors) as an f32 accessor of glTF
  `type` (\"VEC2\"/\"VEC3\"). `with-min-max?` computes component min/max
  (glTF requires this on POSITION accessors). Returns `[builder' acc-idx]`."
  [builder vs gltf-type with-min-max?]
  (let [n (count (first vs))
        bytes (vec (mapcat (fn [v] (mapcat f32->le-bytes v)) vs))
        [bin offset] (append-bin (:bin builder) bytes)
        bv-idx (count (:buffer-views builder))
        buffer-views (conj (:buffer-views builder)
                            {:buffer 0 :byteOffset offset :byteLength (count bytes)
                             :target gt/buffer-target-array-buffer})
        [mins maxs] (when with-min-max? (vec-min-max vs n))
        acc-idx (count (:accessors builder))
        accessor (cond-> {:bufferView bv-idx :componentType gt/component-type-float
                           :count (count vs) :type gltf-type}
                   with-min-max? (assoc :min mins :max maxs))
        accessors (conj (:accessors builder) accessor)]
    [(assoc builder :bin bin :buffer-views buffer-views :accessors accessors) acc-idx]))

(defn write-index-accessor
  "Append `indices` (seq of ints) as an UNSIGNED_INT SCALAR accessor.
  Returns `[builder' acc-idx]`."
  [builder indices]
  (let [bytes (vec (mapcat glb/u32->le-bytes indices))
        [bin offset] (append-bin (:bin builder) bytes)
        bv-idx (count (:buffer-views builder))
        buffer-views (conj (:buffer-views builder)
                            {:buffer 0 :byteOffset offset :byteLength (count bytes)
                             :target gt/buffer-target-element-array-buffer})
        acc-idx (count (:accessors builder))
        accessors (conj (:accessors builder)
                         {:bufferView bv-idx :componentType gt/component-type-unsigned-int
                          :count (count indices) :type "SCALAR"})]
    [(assoc builder :bin bin :buffer-views buffer-views :accessors accessors) acc-idx]))

(defn write-joints-accessor
  "Append `joint-idx-vecs` (seq of 4-int vectors) as an UNSIGNED_SHORT VEC4
  accessor (glTF `JOINTS_0` convention). Returns `[builder' acc-idx]`."
  [builder joint-idx-vecs]
  (let [bytes (vec (mapcat (fn [v] (mapcat u16->le-bytes v)) joint-idx-vecs))
        [bin offset] (append-bin (:bin builder) bytes)
        bv-idx (count (:buffer-views builder))
        buffer-views (conj (:buffer-views builder)
                            {:buffer 0 :byteOffset offset :byteLength (count bytes)
                             :target gt/buffer-target-array-buffer})
        acc-idx (count (:accessors builder))
        accessors (conj (:accessors builder)
                         {:bufferView bv-idx :componentType gt/component-type-unsigned-short
                          :count (count joint-idx-vecs) :type "VEC4"})]
    [(assoc builder :bin bin :buffer-views buffer-views :accessors accessors) acc-idx]))

(defn write-mat4-accessor
  "Append `mats` (seq of 16-float column-major vectors) as a FLOAT MAT4
  accessor (used for `inverseBindMatrices` — not a vertex attribute, so
  its bufferView carries no `:target`). Returns `[builder' acc-idx]`."
  [builder mats]
  (let [bytes (vec (mapcat (fn [m] (mapcat f32->le-bytes m)) mats))
        [bin offset] (append-bin (:bin builder) bytes)
        bv-idx (count (:buffer-views builder))
        buffer-views (conj (:buffer-views builder)
                            {:buffer 0 :byteOffset offset :byteLength (count bytes)})
        acc-idx (count (:accessors builder))
        accessors (conj (:accessors builder)
                         {:bufferView bv-idx :componentType gt/component-type-float
                          :count (count mats) :type "MAT4"})]
    [(assoc builder :bin bin :buffer-views buffer-views :accessors accessors) acc-idx]))

(defn add-part-mesh
  "Add a `character/generate-character` MeshPart (`{:name :vertices
  [{:position :normal :uv}] :indices :material}`) as a one-primitive glTF
  mesh. `material-index-fn` resolves `:material` (a keyword) to a glTF
  material array index — a plain map works (maps are `IFn` of their keys).
  If vertices carry `:joint-indices`/`:joint-weights` (`character.body/
  skin-body`'s output), also writes `JOINTS_0`/`WEIGHTS_0` accessors so the
  primitive is a real skinned mesh. Returns `[builder' mesh-idx]`, or
  `[builder nil]` if the part has no vertices (e.g. the `:bald` hair preset)."
  [builder {:keys [name vertices indices material]} material-index-fn]
  (if (empty? vertices)
    [builder nil]
    (let [positions (mapv :position vertices)
          normals (mapv :normal vertices)
          uvs (mapv :uv vertices)
          [builder pos-acc] (write-vecn-accessor builder positions "VEC3" true)
          [builder norm-acc] (write-vecn-accessor builder normals "VEC3" false)
          [builder uv-acc] (write-vecn-accessor builder uvs "VEC2" false)
          [builder idx-acc] (write-index-accessor builder indices)
          skinned? (contains? (first vertices) :joint-indices)
          [builder joints-acc weights-acc]
          (if skinned?
            (let [[b ja] (write-joints-accessor builder (mapv :joint-indices vertices))
                  [b wa] (write-vecn-accessor b (mapv :joint-weights vertices) "VEC4" false)]
              [b ja wa])
            [builder nil nil])
          attrs (cond-> {:POSITION pos-acc :NORMAL norm-acc :TEXCOORD_0 uv-acc}
                  skinned? (assoc :JOINTS_0 joints-acc :WEIGHTS_0 weights-acc))
          mesh {:name name
                :primitives [{:attributes attrs
                              :indices idx-acc
                              :material (material-index-fn material)}]}
          mesh-idx (count (:meshes builder))]
      [(update builder :meshes conj mesh) mesh-idx])))

(defn add-skin
  "Add a glTF `skins` entry: `joint-node-idxs` (indices into the gltf `nodes`
  array, index-aligned with the skeleton's bones — as produced by
  `build-bone-nodes`) + inverse-bind matrices computed from `bone-world-pos`
  (`character.body/bone-world-positions`' output). Translation-only (matches
  `generate-humanoid-skeleton`'s identity-rotation rest pose) — a bone's
  inverse bind matrix is just a translation by its negated world position.
  Returns `[builder' skin-idx]`."
  [builder joint-node-idxs bone-world-pos]
  (let [mat4 (fn [[x y z]] [1.0 0.0 0.0 0.0, 0.0 1.0 0.0 0.0, 0.0 0.0 1.0 0.0, x y z 1.0])
        ibms (mapv (fn [[x y z]] (mat4 [(- x) (- y) (- z)])) bone-world-pos)
        [builder ibm-acc] (write-mat4-accessor builder ibms)
        skin {:joints joint-node-idxs :inverseBindMatrices ibm-acc}
        skin-idx (count (:skins builder))]
    [(update builder :skins (fnil conj []) skin) skin-idx]))

(defn add-morph-targets
  "Attach `blendshape-targets` (`character.blendshape/generate-arkit-targets`'
  output — `[{:name :deltas} ...]`, index-aligned with `arkit-names`) as
  glTF morph targets on `mesh-idx`'s (sole) primitive. Each target is a
  POSITION-only delta accessor (no NORMAL/TEXCOORD — optional per spec).
  Returns `builder'`."
  [builder mesh-idx blendshape-targets]
  (let [[builder acc-idxs]
        (reduce (fn [[b accs] {:keys [deltas]}]
                  (let [[b acc] (write-vecn-accessor b deltas "VEC3" false)]
                    [b (conj accs acc)]))
                [builder []]
                blendshape-targets)]
    (assoc-in builder [:meshes mesh-idx :primitives 0 :targets]
              (mapv (fn [acc] {:POSITION acc}) acc-idxs))))

(defn material->gltf
  "Convert a `character.material/for-part` map (`{:name :base-color
  :metallic :roughness :emission ...}`) to a glTF `material` map."
  [{:keys [name base-color metallic roughness emission]}]
  {:name name
   :pbrMetallicRoughness {:baseColorFactor base-color :metallicFactor metallic :roughnessFactor roughness}
   :emissiveFactor (or emission [0.0 0.0 0.0])})

(defn build-bone-nodes
  "Convert a `character.body/generate-humanoid-skeleton`-shaped `{:bones
  [{:name :parent :local-position :local-rotation :local-scale} ...]}` (bone
  names are strings, `:parent` an index into `:bones` or nil) into glTF
  nodes (TRS + `:children`, index-for-index with `:bones`)."
  [{:keys [bones]}]
  (let [base (mapv (fn [{:keys [name local-position local-rotation local-scale]}]
                      {:name name :translation local-position
                       :rotation local-rotation :scale local-scale :children []})
                    bones)]
    (reduce (fn [nodes [i {:keys [parent]}]]
              (if parent
                (update-in nodes [parent :children] conj i)
                nodes))
            base
            (map-indexed vector bones))))
