(setdyn :pretty-format "%P")

(def code
  ``
(+ 1 2
   (+ 3 4))
``)

(defn add-start-stop
  [[k v]]
  [k
   ~(/ (* ($) ,v ($))
       ,(fn [start content stop]
          [k content start stop]))])

(def token-peg
  ~{:tuple-start '"("
    :tuple-stop '")"
    :symbol '(some (if-not :d (if-not :s 1)))
    :number ':d+
    :string-delim `"`
    :part-of-string (cmt '(if-not :string-delim 1)
                         (fn [s]))
    :main (any (* :s*
                  (+ :string-delim
                     :tuple-start
                     :tuple-stop
                     :symbol
                     :number
                     1)))})

(def token-peg
  (from-pairs
    (seq [kv :pairs token-peg]
      (if (get {:tuple-start 1
                :tuple-stop 1
                :symbol 1
                :number 1}
               (first kv))
        (add-start-stop kv)
        kv))))

(pp token-peg)

(comment
  (peg/match token-peg code)
  #=>
  # kind, token, start, stop
  @[(:tuple-start "(" 0 1)
    (:symbol "+" 1 2)
    (:number "1" 3 4)
    (:number "2" 5 6)
    (:tuple-start "(" 10 11)
    (:symbol "+" 11 12)
    (:number "3" 13 14)
    (:number "4" 15 16)
    (:tuple-stop ")" 16 17)
    (:tuple-stop ")" 17 18)])

(defn data->obj
  [line-n kind value start stop]
  # uncomment this for enlive style
  # (will break examples further down though)
  #{:value value
  # :kind kind
  # :start [line-n start]
  # :stop [line-n stop]}
  @[kind {:start [line-n start]
          :stop [line-n stop]}
    value])

(defn parse
  ```
Parses line-by-line, returns state which
has
:delimiter-stack
information of depth of delimiters,
:stack
current array one is pushing values to
e.g. a tuple, but if the code parsed looks like:
``
1 2 3
``
Then the  is the "root node".
:line
current line-number
```
  [state line]
  (pp line)
  (def {:delimiter-stack delim-stack
        :stack stack
        :line line-n} state)
  (loop [t :in line
         :let [[kind value start stop] t]]
    (case kind
      :tuple-start (do
                     (array/push stack @[])
                     (array/push delim-stack (data->obj line-n ;t)))

      :tuple-stop (if (= (get (last delim-stack) 2)
                         "(")
                    (let [ld (last delim-stack)]
                      (array/pop delim-stack)
                      (def ls (last stack))
                      (array/pop stack)
                      (array/push (last stack) @[:tuple
                                                 {:start-delim ld
                                                  :start (get-in ld [1 :start])
                                                  :stop [line-n stop]
                                                  :stop-delim (data->obj line-n ;t)}
                                                 ;ls]))
                    (error "no match"))

      # else
      (array/push (last stack) (data->obj line-n ;t)))

    (put state :column stop))

  (update state :line inc))

(defn code->ast
  [c]
  (def lines (seq [l :in (string/split "\n" c)]
               (peg/match token-peg l)))

  (def state @{:line 0
               :delimiter-stack @[]
               :stack @[@[]]})

  (parse state (first lines))
  (parse state (lines 1)))

(def res (code->ast code))

(print "full parsed ast")
(print (string/format "%P" (res :stack)))

(defn spaces
  "Used for indentation"
  [n]
  (string/repeat " " n))

(defn print-hiccup
  "Prints hiccup style ASTs.
Just to more easily visualize."
  [hc &keys {:indent indent
             :rowbreak-end rb}]
  (default indent 0)

  (if (string? hc)
    (prin " " hc)
    (do
      (def tag (first hc))
      (def props (hc 1))
      (def children (drop 2 hc))
      (def nof (length children))

      (prin (spaces indent))
      (prin "@[:" tag " ")

      # can be modifed to print whole props
      (prin (string/format ``
{:start %p :stop %p}
`` (props :start) (props :stop)))
      # lets add a newline before children
      (when (or (= :tuple tag)
                (= :root tag))
        (print))
      (loop [i :range [0 nof]
             :let [h (in children i)]]
        (print-hiccup h
                      :indent (+ indent 1)
                      # don't rowbreak at end
                      # if final child
                      :rowbreak-end
                      (unless
                        (= i (dec nof))
                        0)))

      (prin "]")
      (when rb (print)))))

# extract the ast, slap it into a :root node
(def hiccup
  (let [tree (first (res :stack))]
    @[:root
      # bogus numbers, should be taken from
      # first and last children instead
      {:start [0 0]
       :stop [99999999999 9999999999]}
      ;tree]))

(defn find-tree-index
  [hc line col &opt pos res]
  (if (string? hc)
    res
    (do
      (def tag (first hc))
      (def props (hc 1))
      (def children (drop 2 hc))
      (def nof (length children))

      (default res @[])

      (let [{:start start :stop stop} props
            [start-l start-c] start
            [stop-l stop-c] stop]
        (when (and (<= start-l line)
                   (<= start-c col)
                   (>= stop-l line)
                   (> stop-c col))
          (unless (nil? pos) # this is the root
            (array/push res pos))
          (unless (empty? children)
            (loop [i :range [0 nof]
                   :let [c (in children i)]]
              (find-tree-index c line col i res)))))
      res)))


(defn pop-expr
  ``
Given hiccup-style AST and a line and a column
(e.g., where the caret currently is), find
the path to the innermost node wrapping
that line and column.
``
  [hiccup l c]
  (print "found path to line " l ", column " c)
  (pp (find-tree-index hiccup l c))
  (def [pi ci] (find-tree-index hiccup l c))

  (let [p (get hiccup (+ 2 pi))
        c (get p (+ 2 ci))
        spat (array/pop c)]
    (array/push p spat)))


(print)
(print "---------------------------------------------")
(print)
(print "before popping")
(print)

(print-hiccup
  hiccup
  :rowbreak-end true)

(print)

(pop-expr hiccup 1 3)
(pop-expr hiccup 1 3)

(print)
(print "after popping")

(print-hiccup
  hiccup
  :rowbreak-end true)

(comment
  # old stuff
  # old expected result, when I thoughtw of using enlive
  (def expected-result
    [{:kind :tuple
      :start [0 0]
      :stop [1 11]
      :children
      [{:kind :symbol
        :value '+
        :start [0 1]
        :stop [0 2]}
       {:kind :number
        :value 1
        :start [0 3]
        :stop [0 4]}
       {:kind :number
        :value 2
        :start [0 5]
        :stop [0 6]}
       {:kind :tuple
        :start [1 3]
        :stop [1 10]
        :children
        [{:kind :symbol
          :value '+
          :start [1 4]
          :stop [1 5]}
         {:kind :number
          :value 3
          :start [1 6]
          :stop [1 7]}
         {:kind :number
          :value 4
          :start [1 8]
          :stop [1 9]}]}]}])
  #
  #
  # prints enlive
  (defn print-enlive
    [e &keys {:indent i}]
    (default i 0)
    (if (= :tuple (e :kind))
      (do
        (prin (string ;(map (fn [_] " ") (range 0 i)))
              "(")
        (when (e :children)
          (let [f (first (e :children))
                rest (drop 1 (e :children))]
            (print-enlive f :indent 0)
            (map |(print-enlive $ :indent (+ i 1)) rest)))

        (prin (string ;(map (fn [_] " ") (range 0 i)))
              ")"))
      (do
        (prin (e :value) " ")

        (when (e :children)
          (map |(print-enlive $ :indent (+ i 2)) (e :children)))))))
