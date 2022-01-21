(do 123
  filter
  let
  1 2
  34)

(import ./parse-symbols :as ps :fresh true)
(import freja/state)
(import freja/new_gap_buffer :as gb)
(import freja/introspection :as i)

(setdyn :pretty-format "%P")

(defn gb
  []
  (-> (get-in state/editor-state
              [:stack])
      last
      (get-in [1 :editor :gb])))

(defn node-under-caret
  []
  (def code
    (-> (gb)
        gb/content))

  (print code)

  (do
    (def lines
      (with-dyns [:parse-state @{}]
        (seq [l :in (string/split "\n" code)]
          (->> (peg/match ps/token-peg (string l "\n"))
               (filter (fn [x] (not (nil? x)))))))))

  (def hiccup (ps/code->hiccup code))

  (ps/print-hiccup hiccup)

  (def l (gb/line-number (gb) ((gb) :caret)))
  (def c (gb/column! (gb) ((gb) :caret)))

  (print "found path to line " l ", column " c)
  (pp (ps/find-tree-index hiccup l c))

  (do comment
    (def path (ps/find-tree-index hiccup l c))

    (let [p (ps/get-in-hiccup hiccup (array/slice path 0 -2))
          c (ps/get-in-hiccup hiccup path)]
      (pp c)
      c)))

(comment
  (i/jump-to-def (symbol (last (node-under-caret))))

  (i/jump-to-def 'gb))

(do
  filter
  let
  1 2
  34)

(import freja/default-hotkeys :as dh)

(put dh/gb-binds :left-alt
     @{:key-down
       (fn [props]
         (do
           (def hc (ps/code->hiccup (gb/content props)))
           (put props :styling (map (fn [[_ {:start-i start-i
                                             :stop-i stop-i}]]
                                      [start-i stop-i {:background [0.6 0.2 0.2 0.8]}])
                                    (filter (fn [[_ _ sym]]
                                              state/user-env
                                              (get-in state/user-env [(symbol sym) :source-map]))
                                            (ps/nodes-between hc 0 999))))
           (put-in props [:binds :press]
                   (fn [[l p]]
                     (print "pos: " p)
                     (pp (last (last (ps/find-node hc p))))
                     (i/jump-to-def (symbol (last (last (ps/find-node hc p))))
                                    :env state/user-env)))
           (put props :changed true)))

       :key-release
       (fn [props]

         (do
           (put-in props [:binds :press] nil)
           (put props :styling @[])
           (put props :changed true)))})

(comment
  (def hc (ps/code->hiccup))
  (pp (ps/nodes-between hc 0 4))

  (put (gb) :styling (map (fn [[_ {:start-i start-i
                                   :stop-i stop-i}]]
                            [start-i stop-i {:background :red}])
                          (ps/nodes-between hc 0 4)))
  (put (gb) :changed true)

  (pp ((gb) :styling)))
