(import freja/evaling)
(import freja/state)

(evaling/eval-it state/user-env "(+ 1 1)")


(setdyn :pretty-format "%P")

(def code
  ``````
(1)
@[1]
{:a 1}
@{}
``````)

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
    :tuple-stop '(set ")]}")
    :symbol '(some (if-not :d (if-not :s 1)))
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
    :stack @[@[]]})

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
  [line-n kind value start stop]
  # uncomment this for enlive style
  # (will break examples further down though)
  #{:value value
  # :kind kind
  # :start [line-n start]
  # :stop [line-n stop]}
  @[kind @{:start [line-n start]
           :stop [line-n stop]}
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

      :tuple-stop
      (let [d (get (last delim-stack) 2)
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
                @{:start-delim ld
                  :start (get-in ld [1 :start])
                  :stop [line-n stop]
                  :stop-delim (data->obj line-n ;t)}
                ;ls]))
          (error "no match")))

      :string-start (do
                      (array/push stack @[])
                      (array/push delim-stack (data->obj line-n ;t)))

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
                @{:start-delim ld
                  :longstring longstring
                  :buffer buffer
                  :start (get-in ld [1 :start])
                  :stop [line-n stop]
                  :stop-delim (data->obj line-n ;t)}
                content]))
          (error "no match")))

      # else
      (array/push (last stack) (data->obj line-n ;t)))

    (put state :column stop))

  (update state :line inc))

(defn code->ast
  [c]

  (def lines
    (with-dyns [:parse-state @{}]
      (seq [l :in (string/split "\n" c)]
        #(print "l: " l)
        (->> (peg/match token-peg (string l "\n"))
             (filter |(not (nil? $)))))))

  (def state (new-parse-state))

  (loop [l :in lines]
    (parse state l))

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
             :show-ws show-ws}]
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
                   (> stop-c col))
          (unless (nil? pos) # this is the root
            (array/push res pos))
          (unless (empty? children)
            (loop [i :range [0 nof]
                   :let [c (in children i)]]
              (find-tree-index c line col i res)))))
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


(def res (code->ast code))

(def res
  (if-not (< 1 (length (res :stack)))
    res
    (do
      (print "incomplete forms: ")
      (pp res)
      (print "automatically closing open forms")
      (loop [[kind _ value] :in (reverse (res :delimiter-stack))]
        (print "kind: " kind)
        (match kind
          :tuple-start
          (parse res [[:tuple-stop ")" 0 1]])

          :string-start
          (parse res [[:string-stop value 0 1]])

          (error (string "no automatic closing defined for " kind))))
      res)))

# extract the ast, slap it into a :root node
(def hiccup
  (let [tree (first (res :stack))]
    @[:root
      # bogus numbers, should be taken from
      # first and last children instead
      {:start [0 0]
       :stop [;(get-in (last tree) [1 :stop]
                       # empty tree
                       [0 0])]}
      ;tree]))

(print-hiccup hiccup)

(defn in-string?
  [hiccup l c]
  (= :string (first (lc->el hiccup l c))))


(defn test1
  []

  (def code
    ``
(+ 1 2
   (+ 3 4)
   7)
``)

  (def res (code->ast code))

  # extract the ast, slap it into a :root node
  (def hiccup
    (let [tree (first (res :stack))]
      @[:root
        # bogus numbers, should be taken from
        # first and last children instead
        {:start [0 0]
         :stop [99999999999 9999999999]}
        ;tree]))

  (print "full parsed ast")
  (print (string/format "%P" (res :stack)))

  (print)
  (print "---------------------------------------------")
  (print)
  (print "before popping")
  (print)

  (print-hiccup
    hiccup
    :rowbreak-end true
    :show-ws true)

  (print)

  (pop-expr hiccup 1 3)
  # (pop-expr hiccup 1 3)

  (print)
  (print "after popping")

  (print-hiccup
    hiccup
    :rowbreak-end true
    :show-ws true))

(comment
  #
  (test1)
  #
)

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
