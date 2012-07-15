(letfn*
 [apply-expr__2703__auto__
  (clojure.core/fn
   apply-expr__2703__auto__
   [e__2704__auto__ input*__2705__auto__ out__2706__auto__]
   (if
     (clojure.core/nil? (clojure.core/seq input*__2705__auto__))
     (clojure.core/reverse out__2706__auto__)
     (clojure.core/let
      [start__2707__auto__
       (. java.lang.System (ctco.test.ctco/nanoTime))
       val__2708__auto__
       (clojure.core/apply
        e__2704__auto__
        (clojure.core/first input*__2705__auto__))
       end__2709__auto__
       (. java.lang.System (ctco.test.ctco/nanoTime))
       OUT__2710__auto__
       (clojure.core/cons
        (clojure.core/list
         val__2708__auto__
         (clojure.core/- end__2709__auto__ start__2707__auto__))
        out__2706__auto__)]
      (recur
       e__2704__auto__
       (clojure.core/rest input*__2705__auto__)
       OUT__2710__auto__))))]
 (clojure.core/let
  [old__2711__auto__
   (clojure.core/let
    [start__2700__auto__
     (. java.lang.System (ctco.test.ctco/nanoTime))
     e__2701__auto__
     (defn id [x] x)
     end__2702__auto__
     (. java.lang.System (ctco.test.ctco/nanoTime))]
    (clojure.core/list
     e__2701__auto__
     (clojure.core/- end__2702__auto__ start__2700__auto__)))
   val-time-old*__2712__auto__
   (apply-expr__2703__auto__ old__2711__auto__ '((1)) '())
   new__2713__auto__
   (clojure.core/let
    [start__2700__auto__
     (. java.lang.System (ctco.test.ctco/nanoTime))
     e__2701__auto__
     (let*
      [tramp2732
       (clojure.core/fn
        [thunk2738 flag2739]
        (clojure.core/loop
         [thunk2738 thunk2738]
         (if
           (deref flag2739)
           (do (swap! flag2739 not) thunk2738)
           (recur (thunk2738)))))]
      (clojure.core/let
       [apply-k2733
        (clojure.core/fn
         [k2736 a2737]
         (if (fn? k2736) (k2736 a2737) (do (swap! k2736 not) a2737)))]
       (clojure.core/let
        [flag2734 (atom false)]
        (clojure.core/defn
          id
          ([x] (tramp2732 (id x flag2734) flag2734))
          ([x k2735] (clojure.core/fn [] (apply-k2733 k2735 x)))))))
     end__2702__auto__
     (. java.lang.System (ctco.test.ctco/nanoTime))]
    (clojure.core/list
     e__2701__auto__
     (clojure.core/- end__2702__auto__ start__2700__auto__)))
   val-time-new*__2714__auto__
   (apply-expr__2703__auto__ new__2713__auto__ '((1)) '())]
  (if
    (clojure.core/and
     (clojure.core/not (clojure.core/fn? old__2711__auto__))
     (clojure.core/not (clojure.core/fn? new__2713__auto__)))
    (clojure.test/is
     (clojure.core/= old__2711__auto__ new__2713__auto__))
    (clojure.core/loop
     [val-time-old*__2712__auto__
      val-time-old*__2712__auto__
      val-time-new*__2714__auto__
      val-time-new*__2714__auto__]
     (do
       (clojure.test/is
        (clojure.core/=
         (clojure.core/ffirst val-time-old*__2712__auto__)
         (clojure.core/ffirst val-time-new*__2714__auto__)))
       (recur
        (clojure.core/next val-time-old*__2712__auto__)
        (clojure.core/next val-time-new*__2714__auto__)))))))
