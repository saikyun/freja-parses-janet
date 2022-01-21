(def code ``
(do
  
  filter
  let
  1 2
  34
  )
``)

(import ./parse-symbols :as ps :fresh true)

(def hc (ps/code->hiccup code))
(def [[_ {:start-i start1 :stop-i stop1}]
      [_ {:start-i start2 :stop-i stop2}]
      [_ {:start-i start3 :stop-i stop3}]] (ps/nodes-between hc 0 4))

(try
  (do
    (assert (= start1 1))
    (assert (= stop1 3))

    (assert (= start2 9) "start2 not 9")
    (assert (= stop2 15) "stop2 not 15")

    (assert (= start3 18) "start3 not 18")
    (assert (= stop3 21) "stop3 not 21"))
  ([err fib]
    (debug/stacktrace fib err "")
    (ps/print-hiccup hc :show-absolute true)))

(def node (last (ps/find-node hc 10)))
(print "found node: ")
(pp node)
(assert (= "filter" (get node 2))
        "did not find filter")