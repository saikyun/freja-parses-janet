(import freja/evaling)
(import freja/state)

(setdyn :pretty-format "%P")

(defn add-start-stop
  [k start content stop]
  [k content start stop])

(defn add-start-stop-peg
  [[k v]]
  [k
   ~(/ (* ($) ,v ($))
       ,(partial add-start-stop k))])

(def token-peg
  ~{:tuple-starts (set "([{")
    :tuple-start '(* (opt "@") :tuple-starts)
    :tuple-stop-set (set ")]}")
    :tuple-stop ':tuple-stop-set
    :symfirst (if-not (+ :s
                         :tuple-stop-set
                         :d)
                1)
    :symrest (if-not (+ :s
                        :tuple-stop-set)
               1)
    :symbol '(* :symfirst (any :symrest))
    :number ':d+

    :longstring-delim
    (cmt (* ($) '(some "`") ($))
         ,(fn longstring-delim
            [start delim stop]
            (let [ps (dyn :parse-state)]
              (cond (and (ps :in-string?)
                         (ps :longstring))
                (when (= delim (ps :longstring))
                  (do (put ps :in-string? false)
                    (put ps :longstring nil)
                    [:string-stop delim start stop]))

                (not (ps :in-string?))
                (do (put ps :in-string? true)
                  (put ps :longstring delim)
                  [:string-start delim start stop])))))

    :longbuffer-start
    (cmt (* ($) "@" '(some "`") ($))
         ,(fn longbuffer-start
            [start delim stop]
            (let [ps (dyn :parse-state)]
              (when
                (not (ps :in-string?))
                (do (put ps :in-string? true)
                  (put ps :longstring delim)
                  [:string-start (string "@" delim) start stop])))))

    :buffer-start
    (cmt (* ($) "@" '`"` ($))
         ,(fn longbuffer-start
            [start delim stop]
            (let [ps (dyn :parse-state)]
              (when (not (ps :in-string?))
                (put ps :in-string? true)
                [:string-start (string "@" delim) start stop]))))

    :string-delim (/ (* ($) '`"` ($))
                     ,(fn string-delim
                        [start delim stop]
                        (def ps (dyn :parse-state))

                        (update ps :in-string? not)

                        (if (ps :in-string?)
                          [:string-start delim start stop]
                          [:string-stop delim start stop])))

    :longstring (cmt (* ($) '(+ (some "`")
                                (some (if-not "`" 1))) ($))
                     ,(fn longstring
                        [start s stop]
                        (let [ps (dyn :parse-state)]
                          (cond
                            (= s (ps :longstring))
                            nil

                            (ps :longstring)
                            [:string s start stop]))))

    :string (cmt (* ($) '(some (if-not `"` 1)) ($))
                 ,(fn string
                    [start s stop]
                    (let [ps (dyn :parse-state)]
                      (cond
                        (ps :longstring)
                        nil

                        (ps :in-string?)
                        [:string s start stop]))))

    :ws ':s+

    :main (any (+ :longstring
                  :longstring-delim
                  :longbuffer-start
                  :string
                  :buffer-start
                  :string-delim
                  :tuple-start
                  :tuple-stop
                  :symbol
                  :number
                  :ws))})

(def token-peg
  (from-pairs
    (seq [kv :pairs token-peg]
      (if (get {:tuple-start 1
                :mut-tuple-start 1
                :tuple-stop 1
                :symbol 1
                :number 1
                :ws 1}
               (first kv))
        (add-start-stop-peg kv)
        kv))))

(defn new-parse-state
  []
  @{:line 0
    :delimiter-stack @[]
    :stack @[@[]]
    :abs-pos 0})

(comment
  (with-dyns [:parse-state (new-parse-state)]
    (->> (peg/match token-peg code)
         (filter truthy?)
         pp)))

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
  [line-n abs-pos kind value start stop]
  # uncomment this for enlive style
  # (will break examples further down though)
  #{:value value
  # :kind kind
  # :start [line-n start]
  # :stop [line-n stop]}
  @[kind @{:start [line-n start]
           :stop [line-n stop]
           :start-i (+ abs-pos start)
           :stop-i (+ abs-pos stop)}
    value])

(defn cleanup-longstring
  "Trims single newline in front and back."
  [s]
  (string/slice s
                (if (string/has-prefix? "\n" s)
                  1
                  0)
                (if (string/has-suffix? "\n" s)
                  (dec (length s))
                  (length s))))

(def matching-start
  {")" "("
   "]" "["
   "}" "{"})

(def matching-start-mut
  {")" "@("
   "]" "@["
   "}" "@{"})

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
  #(pp line)
  (def {:delimiter-stack delim-stack
        :stack stack
        :line line-n
        :abs-pos abs-pos} state)
  (loop [t :in line
         :let [[kind value start stop] t]]
    (case kind
      :tuple-start (do
                     (array/push stack @[])
                     (array/push delim-stack (data->obj line-n abs-pos ;t)))

      :tuple-stop
      (let [d (get (last delim-stack) 2)
            _ (when (nil? d)
                (errorf "one too many ending delimiters: %P"
                        (data->obj line-n abs-pos ;t)))
            mut (string/has-prefix? "@" d)]
        (if (= d
               (if mut
                 (matching-start-mut value)
                 (matching-start value)))
          (let [ld (last delim-stack)]
            (array/pop delim-stack)
            (def ls (last stack))
            (array/pop stack)
            (array/push
              (last stack)
              @[(case d
                  "(" :tuple
                  "[" :tuple
                  "{" :struct
                  "@(" :array
                  "@[" :array
                  "@{" :table)
                (let [start (get-in ld [1 :start])]
                  @{:start-delim ld
                    :start-i (get-in ld [1 :start-i])
                    :stop-i (+ abs-pos stop)
                    :start start
                    :stop [line-n stop]
                    :stop-delim (data->obj line-n abs-pos ;t)})
                ;ls]))
          (errorf "no match for %p start: %d, stop: %d"
                  d
                  start
                  stop)))

      :string-start (do
                      (array/push stack @[])
                      (array/push delim-stack (data->obj line-n abs-pos ;t)))

      :string-stop
      (let [d (get (last delim-stack) 2)
            buffer (string/has-prefix? "@" d)]
        (if (peg/match ~(+ `"`
                           ,value)
                       (if buffer
                         (slice d 1)
                         d))
          (let [ld (last delim-stack)]
            (array/pop delim-stack)
            (def ls (last stack))
            (array/pop stack)

            (def longstring (unless (= `"` value) value))

            (def content (string ;(mapcat |(drop 2 $) ls)))
            (def content (if-not longstring
                           content
                           (cleanup-longstring content)))

            (array/push
              (last stack)
              @[:string
                (let [start (get-in ld [1 :start])]
                  @{:start-delim ld
                    :longstring longstring
                    :buffer buffer
                    :start-i (+ abs-pos (in start 1))
                    :stop-i (+ abs-pos stop)
                    :start start
                    :stop [line-n stop]
                    :stop-delim (data->obj line-n abs-pos ;t)})
                content]))
          (error "no match")))

      # else
      (array/push (last stack) (data->obj line-n abs-pos ;t)))

    (put state :column stop))

  (update state :line inc)
  #(update state :abs-pos + 6(last line) 1)
  )

(defn code->ast
  [c]
  (def parsed-lines
    (with-dyns [:parse-state @{}]
      (seq [l :in (string/split "\n" c)]
        #(print "l: " l)
        (->> (peg/match token-peg (string l "\n"))
             (filter |(not (nil? $)))
             (|[l $])))))

  (def state (new-parse-state))
  
  (loop [[l pl] :in parsed-lines]
    (parse state pl)
    (update state :abs-pos + (length l) 1))

  state)


(defn spaces
  "Used for indentation"
  [n]
  (string/repeat " " n))

(defn print-hiccup
  "Prints hiccup style ASTs.
Just to more easily visualize."
  [hc &keys {:indent indent
             :rowbreak-end rb
             :show-ws show-ws
             :show-absolute show-i}]
  (default indent 0)
  (default show-ws false)

  (cond
    (string? hc)
    (prin (string/format " %p" hc))

    (and (not show-ws)
         (= :ws (first hc)))
    nil

    (do
      (def tag (first hc))
      (def props (hc 1))
      (def children (drop 2 hc))
      (def nof (length children))

      (prin (spaces indent))
      (prin "@[:" tag " ")

      # can be modifed to print whole props
      (prin (string/format ``
{:start %p :stop %p
`` (props :start) (props :stop)))
      (when show-i
        (prin (string/format ``
 :start-i %p :stop-i %p
`` (props :start-i) (props :stop-i))))
      (when (props :buffer)
        (prin " :buffer " (props :buffer)))
      (prin "}")
      # lets add a newline before children
      (when (or (= :tuple tag)
                (= :array tag)
                (= :struct tag)
                (= :table tag)
                (= :root tag))
        (print))
      (loop [i :range [0 nof]
             :let [h (in children i)]]
        (print-hiccup h
                      :show-ws show-ws
                      :show-absolute show-i
                      :indent (+ indent 1)
                      # don't rowbreak at end
                      # if final child
                      :rowbreak-end
                      (unless
                        (= i (dec nof))
                        0)))

      (prin "]")
      (when (or (= :root tag)
                rb)
        (print)))))

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
                   (or (> stop-l line)
                       (> stop-c col)))
          (unless (nil? pos) # this is the root
            (array/push res pos))
          (unless (empty? children)
            (loop [i :range [0 nof]
                   :let [c (in children i)]]
              (find-tree-index c line col i res)))))
      res)))

(defn find-node
  [hc i &opt pos res]
  (if (string? hc)
    res
    (do
      (def tag (first hc))
      (def props (hc 1))
      (def children (drop 2 hc))
      (def nof (length children))

      (default res @[])

      (let [{:start-i start :stop-i stop} props]
        (when (and (<= start i)
                   (< i stop))
          (unless (nil? pos) # this is the root
            (array/push res hc))
          (unless (empty? children)
            (loop [ci :range [0 nof]
                   :let [c (in children ci)]]
              (find-node c i ci res)))))
      res)))

(defn nodes-between
  [hc start-line stop-line &opt pos res]
  (if (string? hc)
    res
    (do
      (def tag (first hc))
      (def props (hc 1))
      (def children (drop 2 hc))
      (def nof (length children))

      (default res @[])

      (let [{:start start :stop stop} props
            [start-l] start
            [stop-l] stop]
        (when (and (<= start-l stop-line)
                   (>= stop-l start-line))
          (unless (nil? pos) # this is the root
            (when (= tag :symbol)
              (array/push res hc)))
          (unless (empty? children)
            (loop [i :range [0 nof]
                   :let [c (in children i)]]
              (nodes-between c start-line stop-line i res)))))
      res)))

(defn get-in-hiccup
  [hiccup path]
  (var el hiccup)
  (loop [i :in path]
    (set el (get el (+ i 2))))
  el)

(defn lc->el
  [hiccup l c]
  (let [path (find-tree-index hiccup l c)]
    (get-in-hiccup hiccup path)))

(defn remove-pos
  [[_ props]]
  (-> props
      (put :start nil)
      (put :stop nil)))

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
  (def path (find-tree-index hiccup l c))

  (let [p (get-in-hiccup hiccup (array/slice path 0 -2))
        c (get-in-hiccup hiccup path)]
    (def to-move @[])

    (while true
      (let [v (array/pop c)]
        (remove-pos v)
        #        (array/push to-move v)

        #                # 2 for tag and props, 1 to get behind c
        (array/insert p (+ 3 (last path)) v))

      (unless (= :ws (get (last c) 0))
        (break)))

    (put-in p [1 :stop] nil)
    (put-in c [1 :stop] nil)))

(defn in-string?
  [hiccup l c]
  (= :string (first (lc->el hiccup l c))))

(defn close-delims
  [ast]
  (if-not (< 1 (length (ast :stack)))
    ast
    (do
      (print "incomplete forms: ")
      #(pp ast)
      (printf "%P" ast)
      (print "automatically closing open forms")
      (loop [[kind _ value] :in (reverse (ast :delimiter-stack))]
        (print "kind: " kind)
        (match kind
          :tuple-start
          (parse ast [[:tuple-stop ")" 0 1]])

          :string-start
          (parse ast [[:string-stop value 0 1]])

          (error (string "no automatic closing defined for " kind))))
      ast)))

# extract the ast, slap it into a :root node
(defn hiccupify
  [ast]
  (let [tree (first (ast :stack))]
    @[:root
      # bogus numbers, should be taken from
      # first and last children instead
      {:start [0 0]
       :start-i 0
       :stop [;(get-in (last tree) [1 :stop]
                       # empty tree
                       [0 0])]
       :stop-i (get-in (last tree) [1 :stop-i]
                       # empty tree
                       0)}
      ;tree]))

(defn code->hiccup
  [code]
  (-> code
      code->ast
      close-delims
      hiccupify))