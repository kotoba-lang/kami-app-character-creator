(ns character-creator.doc
  "`CharacterDoc` — the app's own EDN document, per ADR-2607031200. Wraps
  `character.params/CharacterDef` (the sliders) unchanged rather than
  inventing a parallel schema.

  Deviation from the ADR sketch: the ADR proposed a separate `:character/
  palette` colour system alongside `CharacterDef`'s own `:skin :tone` /
  `:hair :color` / `:eyes :iris-color` fields. Keeping both would be two
  sources of truth for the same three colours, so `:character/palette` here
  is a *convenience alias* `boot-config` merges into those nested
  `CharacterDef` fields, not a parallel state slot on the resolved doc.

  `:character/equip` (the ADR's own name for this field) and `:character/
  decals` (new, not in the original ADR sketch) are now real — vectors of
  `character-creator.accessories/accessory-catalog` / `decal-catalog` id
  keywords, resolved into extra mesh parts by `character-creator.pipeline`.
  Empty vectors by default (no accessories/decals on a fresh character)."
  (:require [character.params :as params]))

(def default-palette
  {:skin [0.94 0.87 0.82] :hair [0.92 0.85 0.70] :eye [0.45 0.65 0.85]})

(defn- apply-palette
  [character-def {:keys [skin hair eye]}]
  (cond-> character-def
    skin (assoc-in [:skin :tone] skin)
    hair (assoc-in [:hair :color] hair)
    eye (assoc-in [:eyes :iris-color] eye)))

(defn default-character-doc
  "A fresh `CharacterDoc` from `character.params/default-character-def`,
  unmodified."
  []
  {:character/id (str (gensym "char-"))
   :character/name "New Character"
   :character/def (params/default-character-def)
   :character/palette default-palette
   :character/equip []
   :character/decals []})

(defn boot-config
  "`{:id :name :palette :hair-preset :clothing-preset :equip :decals
  :overrides}` -> a resolved `CharacterDoc`. Mirrors `kami-app-car-sim`'s
  `scene.cljc/boot-config` (id + paint-hex + map -> one pure config map)
  pattern.

  `:overrides` is a `[[path value] ...]` seq of extra `assoc-in`s into the
  `CharacterDef` (e.g. `[[[:body :height] 1.1]]`) for params this fn doesn't
  name directly — `character.params` ships one base preset today (no named
  base-character library like the car-sim garage's 6 vehicles), so `:id` is
  presently just an opaque passthrough, not a lookup key; a base-preset
  library is future work, not fabricated here."
  [{:keys [id name palette hair-preset clothing-preset equip decals overrides]
    :or {palette default-palette equip [] decals []}}]
  (let [base (apply-palette (params/default-character-def) palette)
        base (cond-> base
               hair-preset (assoc-in [:hair :preset] hair-preset)
               clothing-preset (assoc-in [:clothing :preset] clothing-preset))
        def (reduce (fn [d [path v]] (assoc-in d path v)) base overrides)]
    {:character/id (or id (str (gensym "char-")))
     :character/name (or name "New Character")
     :character/def def
     :character/palette palette
     :character/equip equip
     :character/decals decals}))
