(import ./parse-symbols :as ps)

(defn test1
  []

  (def code
    ``
(+ 1 2
   (+ 3 4)
   7)
``)

  (def res (ps/code->ast code))

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

  (ps/print-hiccup
    hiccup
    :rowbreak-end true
    :show-ws true)

  (print)

  (ps/pop-expr hiccup 1 3)
  # (pop-expr hiccup 1 3)

  (print)
  (print "after popping")

  (ps/print-hiccup
    hiccup
    :rowbreak-end true
    :show-ws true))

(do comment
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

 