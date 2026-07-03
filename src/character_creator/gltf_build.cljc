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

(defn add-part-mesh
  "Add a `character/generate-character` MeshPart (`{:name :vertices
  [{:position :normal :uv}] :indices :material}`) as a one-primitive glTF
  mesh. `material-index-fn` resolves `:material` (a keyword) to a glTF
  material array index — a plain map works (maps are `IFn` of their keys).
  Returns `[builder' mesh-idx]`, or `[builder nil]` if the part has no
  vertices (e.g. the `:bald` hair preset)."
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
          mesh {:name name
                :primitives [{:attributes {:POSITION pos-acc :NORMAL norm-acc :TEXCOORD_0 uv-acc}
                              :indices idx-acc
                              :material (material-index-fn material)}]}
          mesh-idx (count (:meshes builder))]
      [(update builder :meshes conj mesh) mesh-idx])))

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
