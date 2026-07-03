(ns character-creator.persistence
  "Browser-local persistence + file export for a `CharacterDoc`. Per
  ADR-2607031200 Phase 1/3: `kotoba-lang/kotoba-client` (the kotobase.net
  client) is read-only today — no signed-publish path exists yet, so
  kotobase.net save is an explicit Phase 3 follow-up, not built here.
  Phase 1 ships local `localStorage` save/load + downloadable `.edn`/`.vrm`
  file export, which covers real usage without inventing a save backend
  that isn't there.

  Browser-only (`:cljs`) by nature; `:clj` throws a clear error rather than
  silently no-op'ing, so a JVM caller finds out immediately if it's wired
  in wrong."
  (:require [vrm.glb :as glb]))

(def ^:private storage-key-prefix "kami-character-creator/")

(defn- not-browser! [fn-name]
  (throw (ex-info (str fn-name " is browser-only (:cljs)")
                   {:character-creator/error :not-browser})))

(defn save-local!
  "Save `doc` to `localStorage` under its `:character/id`."
  [doc]
  #?(:cljs (.setItem js/localStorage (str storage-key-prefix (:character/id doc)) (pr-str doc))
     :clj (not-browser! "save-local!")))

(defn load-local
  "Load a previously-saved `CharacterDoc` by id, or `nil`."
  [id]
  #?(:cljs (some-> (.getItem js/localStorage (str storage-key-prefix id)) read-string)
     :clj (not-browser! "load-local")))

(defn list-local-ids
  "All `:character/id`s currently saved in `localStorage`."
  []
  #?(:cljs (let [n (.-length js/localStorage)]
             (vec (keep (fn [i]
                          (let [k (.key js/localStorage i)]
                            (when (clojure.string/starts-with? k storage-key-prefix)
                              (subs k (count storage-key-prefix)))))
                        (range n))))
     :clj (not-browser! "list-local-ids")))

(defn- download-blob!
  "Trigger a browser file-save-as for `bytes` (a byte-int vector) via a
  throwaway `<a download>` + `Blob`/`ObjectURL`."
  [bytes mime filename]
  #?(:cljs
     (let [arr (js/Uint8Array. (clj->js (vec bytes)))
           blob (js/Blob. #js [arr] #js {:type mime})
           url (.createObjectURL js/URL blob)
           a (.createElement js/document "a")]
       (set! (.-href a) url)
       (set! (.-download a) filename)
       (.click a)
       (.revokeObjectURL js/URL url))
     :clj (not-browser! "download-blob!")))

(defn download-edn!
  "Download `doc` as a `.edn` file (the CharacterDoc, editable/re-loadable)."
  [doc]
  (download-blob! (glb/string->byte-seq (pr-str doc)) "application/edn"
                   (str (:character/name doc) ".edn")))

(defn download-vrm!
  "Download already-exported `vrm-bytes` (from `character-creator.pipeline/
  character-doc->vrm-bytes`) as a real `.vrm` file."
  [doc vrm-bytes]
  (download-blob! vrm-bytes "model/gltf-binary" (str (:character/name doc) ".vrm")))
