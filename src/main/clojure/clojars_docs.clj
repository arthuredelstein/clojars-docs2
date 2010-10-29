(ns clojars-docs.core
 (:use [clojure.contrib.duck-streams :only (slurp*)]
   [hiccup.core])
 (:import (java.util.zip ZipFile)
   (java.io File))
 (:require (clojure [zip :as zip])))

;        [net.cgrand.enlive-html :only (html-resource)])

;;; In this section, we extract source files and make html.

(def *root* "/projects/clojars/copy-of-clojars/")

(def *text-file-suffixes*
  (for [suf ["txt" "clj" "java" "xml"]]
    (str "." suf)))

(defn src-to-html [clj-str]
  (str "<html><body><pre>\n"
    (escape-html clj-str)
    "</pre></body></html>\n"))

(defn write-text-to-html [root text]
  (let [path (str root "/" (:name text) ".html")]
    (-> (File. path) .getParent (File.) .mkdirs)
    (spit path (src-to-html (:text text)))
    (println "...wrote" path)))

(defn get-entries-in-jar [jarfile]
  (enumeration-seq (.entries jarfile)))

(defn text-entry? [entry]
  (let [name (.. entry getName toLowerCase)]
    (some #(.endsWith name %) *text-file-suffixes*)))

(defn entry-to-text [entry jarfile]
  {:text (slurp* (.getInputStream jarfile entry))
   :name (.getName entry)})

(defn get-texts-from-jar [path]
  (let [jarfile (ZipFile. path)]
    (for [entry (get-entries-in-jar jarfile) :when (text-entry? entry)]
      (entry-to-text entry jarfile))))

(defn process-jar
  "Extract the source code from a jar file and
write it into html files in the _srcs directory
inside root."
  [jarpath root]
  (let [localroot (str jarpath "_srcs")
        srcs (str root "/_srcs")
        htmlpath (.replace localroot root srcs)]
    (println "Processing" jarpath "... ")
    (try
      (doseq [text (get-texts-from-jar jarpath)]
        (write-text-to-html htmlpath text))
      (catch Exception e (println "... failed.")))))

(defn process-tree [root]
  (let [root (.getAbsolutePath (File. root))
        files (file-seq (File. root))]
    (doseq [file files :when (.. file getName toLowerCase (endsWith ".jar"))]
      (-> file .getAbsolutePath (process-jar root)))))

;;; Functions for creation of an index


(defn tails [x]
  (loop [xi x tails []]
    (if xi
      (recur (next xi) (conj tails xi))
      tails)))

(defn pairs [x]
  (for [xi (tails x)]
    [(first xi) (second xi)]))

(defn get-files
  "Get the absolute path of all files inside root."
  [root]
  (map #(.getAbsolutePath %) (file-seq (File. root))))

(defn full-path [path]
  (.getAbsolutePath (File. path)))

(defn dir?
  [f] (.isDirectory (File. f)))

(defn dir-of
  "Returns the path if it is a directory;
otherwise returns the file's parent."
  [path]
  (let [f (File. path)]
    (if (.exists f)
      (if (.isDirectory f)
        path
        (.getParent f))
      (throw (java.io.FileNotFoundException.)))))

(defn file-parent
  "Get the parent directory."
  [path]
  (.getParent (File. path)))

(defn file-name
  "Get the file name from a path."
  [path]
  (.getName (File. path)))

(defn file-children
  [path]
  (if dir? path)
  (.listFiles (File. path)))

(defn relative-path
  "Returns the relative path from base to path (both strings)"
  [base path]
  (let [
        base (.split (dir-of base) "/")
        path (.split (full-path path) "/")
        [bs ps]
        (loop [b base p path]
          (if (= (first b) (first p))
            (recur (rest b) (rest p))
            [b p]))]
    (apply str (concat
                 (repeat (count bs) "../")
                 (interpose \/ ps)))))

(defn get-jar-srcs-dir
  "Get the clojar.jar_srcs directory
enclosing the file at path."
  [path]
  (loop [f1 path]
    (if-not (.endsWith f1 ".jar_srcs")
      (recur (file-parent f1))
      f1)))

(defn compose-lein-dep
  "Create a lein dependencies argument that corresponds to
the clojar referred to by the clojar.jar_srcs directory."
  [path]
  (if path
    (let [jars-srcs-path (get-jar-srcs-dir path)
          x3 (file-parent jars-srcs-path)
          x2 (file-parent x3)
          x1 (file-parent x2)
          version (file-name x3)
          artifact (file-name x2)
          group (.replace (second (.split x1 "_srcs/")) "/" ".")
          name (if (= group artifact) artifact (str group "/" artifact))]
      (str "[" name " \"" version "\"]"))))

(defn select-html-files
  "Filter out non-html files."
  [fs]
  (filter #(.endsWith % ".html") fs))

(defn select-jar-src-dirs
  [fs]
  (filter #(.endsWith % "jar_srcs") fs))

(defn match-lein-dep-with-files
  [fs]
  (for [[f1 f2] (pairs fs)]
    (if (not= f1 f2)
      [(compose-lein-dep f1) f1])))

(defn map-dep-to-jar
  [fs]
  (into {} (match-lein-dep-with-files fs)))

(defn get-jar_src-dir [x root]
  (relative-path (str root "/_srcs") x))

(defn file-tree [root]
  (loop [tree (zip/seq-zip (quote root))]
    (let [tmp
          (zip/make-node tree (seq (.listFiles (zip/node tree))))]
      (if (not (zip/end? tmp))
        (recur (zip/next tmp))
        (zip/root tmp)))))
;(let [children (.listFiles (File. root))]
;  (if children
;    (recur (map #(zip/append-child tree (.getAbsolutePath %)) children))
;    tree))))

(defstruct artifact
  :lein-dep :jar-srcs :files)


(defn get-files-in-jars
  [fs]
  (loop [ftail fs data {}]
    (let [f (first ftail)
          j (get-jar-srcs-dir f)
          data2 (merge-with concat data {j [f]})]
      (if (next ftail)
        (recur (next ftail) data2)
        data2))))

(defn make-artifact-list-from-maps
  [depmap jarmap]
  (for [dep (sort (keys depmap))]
    (let [jar (depmap dep)
          longfiles (jarmap jar)
          files (for [file longfiles]
                  {:short (relative-path jar file) :rel (relative-path *root* file)})]
      (struct-map artifact :lein-dep dep :jar-srcs jar :files files))))

(defn make-artifact-collection
  []
  (let [allfiles (get-files *root*)
        depmap (map-dep-to-jar (select-jar-src-dirs allfiles))
        jarmap (get-files-in-jars (select-html-files allfiles))]
    (make-artifact-list-from-maps depmap jarmap)))


(defn make-srcs-index [artifacts]
  (let
    [tablerows
     (for [a artifacts]
       [:tr [:td {:valign "top"} (:lein-dep a)]
        [:td {:valign "top"}
         (for [f (:files a)] [:span [:a {:href (:rel f)} (:short f)] [:br]])]])
     table [:table tablerows]
     doc [:html [:body table]]]
    (spit (str *root* "/sources.html") (html doc))))


(defn make-srcs-index2 [artifacts]
  (let
    [tablerows
     (for [a artifacts]
       [:tr [:td {:valign "top"}
             [:a {:href (relative-path *root* (:jar-srcs a))} (:lein-dep a)]]])
     table [:table tablerows]
     doc [:html [:body table]]]
    (spit (str *root* "/sources2.html") (html doc))))