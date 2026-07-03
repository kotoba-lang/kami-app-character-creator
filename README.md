# kotoba-lang/kami-app-character-creator

Phase 1 of **ADR-2607031200** (`com-junkawasaki/root`,
`90-docs/adr/2607031200-kotoba-lang-vrm-character-creator.md`): an
EDN-native, user-facing VRM character creator. This repo covers Phase 1
only — "CharacterDoc スキーマ + 生成パイプライン配線 + 表情ブリッジEDN +
2Dサムネイルプレビュー + ローカル保存/.vrmエクスポート" — zero new
engine/GPU work, pure composition of the already-restored
`kotoba-lang/{vrm,character}` CLJC (ADR-2607010930).

## Pipeline

```
CharacterDoc (EDN)
  -> character/generate-character   (kotoba-lang/character)
  -> character-creator.gltf-build   (NEW: raw MeshPart -> glTF accessors/nodes)
  -> vrm.vrm-types/vrm-document     (kotoba-lang/vrm constructors)
  -> vrm.export/export-glb          (kotoba-lang/vrm)
  -> real .vrm GLB bytes
```

## Deviations from the ADR's sketch (found while implementing, not fabricated around)

- **`vrm.part`/`vrm.compose` are not used.** Reading the real API showed
  they operate on *already-parsed* `VrmDocument`s — splitting/merging parts
  of *existing* VRM files by node/mesh/accessor index (the "swap hair
  between two VRoid avatars" feature). There is no from-scratch document
  builder in `vrm`, because no Rust original ever combined
  `kami-character`'s raw mesh output with `kami-vrm`'s document model. This
  repo's `character-creator.gltf-build` is that missing adapter, built
  directly on `vrm.gltf-types`/`vrm.vrm-types`'s own constructors — same
  target shape (`vrm.export/export-glb` is unchanged), different path to
  it.
- **VRM 1.0 has 18 expression presets, not 17** (`vrm.vrm-types/expression-
  preset-table` includes `:neutral`; the ADR undercounted). No design
  impact, just a corrected number throughout code/tests.
- **`:character/palette` is a merge-in alias, not a parallel colour
  system.** The ADR sketched it as a sibling of `:character/def`;
  `character.params/CharacterDef` already has `:skin :tone` / `:hair
  :color` / `:eyes :iris-color`, so keeping both would be two sources of
  truth for the same three colours. `character-creator.doc/boot-config`
  merges palette into those nested fields instead.
- **The generated avatar is a bust (head + upper body), not a full
  standing character**, and is **unskinned** (no `:skins`/JOINTS_0/
  WEIGHTS_0) — both inherited from `character.body` as restored
  (`generate-body` docstring: "neck + upper body"; the humanoid skeleton
  has 13 bones, no legs/hands) and from the fact that no mesh generator in
  `character` attaches vertex weights. The exported `.vrm` is spec-valid
  (humanoid bone *nodes* are present and mapped via `VRMC_vrm`), but a
  viewer can't pose-deform it yet. Extending `character.body` to a full
  rigged body is separate follow-up work, not something this pipeline can
  add by itself.
- **The VRM-preset ⇄ ARKit-blendshape bridge
  (`character-creator.expression-bridge/preset->arkit-weights`) is new,
  hand-authored EDN**, not ported from anything — no Rust original wired
  these two vocabularies together. It's a standard, editable approximation
  (the same kind of mapping VRM-authoring tools like UniVRM ship).

## Namespaces

| File | Purpose |
|---|---|
| `character-creator.doc` | `CharacterDoc` schema, `default-character-doc`, `boot-config` (mirrors `kami-app-car-sim`'s `boot-config` pattern) |
| `character-creator.gltf-build` | glTF accessor/bufferView/buffer *writer* (the missing inverse of `vrm.convert`'s reader) + bone-node builder |
| `character-creator.expression-bridge` | VRM preset ⇄ ARKit blendshape EDN table + `vrm-expression` builder |
| `character-creator.pipeline` | `CharacterDoc -> VrmDocument -> .vrm bytes` |
| `character-creator.thumbnail` | Cheap 2D EDN sprite preview for a picker grid (not the Phase 2 live-3D preview) |
| `character-creator.persistence` | `:cljs`-only `localStorage` save/load + `.edn`/`.vrm` file download |

## Test

```bash
clojure -M:test
```

11 tests / 81 assertions, including a full round-trip: `CharacterDoc ->
character-doc->vrm-bytes -> vrm/parse-vrm` re-reads the exported `.vrm`
correctly (GLB magic header, humanoid bones, all 18 expressions).

## Not in Phase 1 (see ADR-2607031200)

- Live 3D preview (Phase 2 — needs a WGSL skinning+morph shader in
  `kotoba-lang/webgpu`).
- kotobase.net save/publish (Phase 3 — blocked on `kotoba-client` growing a
  write path; `persistence.cljc` ships local-only for now).
- Slider/ColorSwatch/Carousel UI widgets (Phase 4 — `kami-ui-sdk` has none
  yet).
