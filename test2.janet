(import ./parse-symbols :as ps)

(setdyn :pretty-format "%P")

(def code
  ``````
  (1)
  
  @[1]
  {:a 1}
  @{}
  ``````)


(do
  (print "lines")
  (def lines
    (with-dyns [:parse-state @{}]
      (seq [l :in (string/split "\n" code)]
        #(print "l: " l)
        (->> (peg/match ps/token-peg (string l "\n"))
             (filter |(not (nil? $)))))))
  
    (pp (->> lines
             #(filter |(get {:tuple-stop 1
             #               :tuple-start 1} (first $) ))
             ))
    )

  


(ps/print-hiccup (ps/code->hiccup code))
