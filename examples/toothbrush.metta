!(import! &self PLN)

(= (kbstatic)
   ((sentence ((Concept X) (stv 0.1 0.99)) (0))
   (sentence ((Concept heated) (stv 0.1 0.99)) (0))
   (sentence ((Concept melted) (stv 0.1 0.99)) (0))
   (sentence ((Concept pliable) (stv 0.1 0.99)) (0))
   (sentence ((Concept madeOfPlastic) (stv 0.1 0.99)) (0))
   (sentence ((Concept applyLighter) (stv 0.1 0.99)) (0))
   (sentence ((Implication (Inheritance (Concept $X) 
                                        (Concept heated))
                           (Inheritance (Concept $X)
                                        (Concept melted))) (stv 0.99 0.99)) (2))))

;takes 30 sec to compile (no reasoning happening here just preparing a fast PLN reasoner with the concepts and implications engraved)
!(PLN.Init (kbstatic))

(= (kbdynamic)
 ( ; if it's heated it will be melted
    (Sentence ( (Inheritance (Concept X) (Concept heated))   (stv 0.99 0.99))   (11) )

 )
)

!(Question (kbdynamic) (Inheritance (Concept X) (Concept melted)) 1)




