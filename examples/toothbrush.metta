!(import! &self PLN)

(= (kbstatic)
   ((sentence ((Concept X) (stv 0.1 0.99)) (0))
    (sentence ((Concept heated) (stv 0.1 0.99)) (0))
    (sentence ((Concept melted) (stv 0.1 0.99)) (0))
    (sentence ((Concept pliable) (stv 0.1 0.99)) (0))
    (sentence ((Concept madeOfPlastic) (stv 0.1 0.99)) (0))
    (sentence ((Concept applyLighter) (stv 0.1 0.99)) (0))))

;takes 30 sec to compile (no reasoning happening here just preparing a fast PLN reasoner with the concepts and implications engraved)
!(PLN.Init (kbstatic))


(= (kbdynamic)
   (
      ; If something is made of plastic, applying the lighter on it will make it heated
      (Sentence (
         (Implication 
            (Inheritance (Concept $X) (Concept madeOfPlastic))
            (Implication 
               (Inheritance (Concept $X) (Concept applyLighter))
               (Inheritance (Concept $X) (Concept heated))
            )
         )   (stv 0.99 0.99))   (1) )
      
      ; if it's heated it will be melted
      (Sentence (
         (Implication 
            (Inheritance (Concept $X) (Concept heated))
            (Inheritance (Concept $X) (Concept melted))
         )   (stv 0.99 0.99))   (2) )

      ; if it's melted it will be pliable
      (Sentence (
         (Implication 
            (Inheritance (Concept $X) (Concept melted))
            (Inheritance (Concept $X) (Concept pliable))
         )   (stv 0.99 0.99))   (3) )




      ; X is made of plastic
      (Sentence ( (Inheritance (Concept X) (Concept madeOfPlastic))   (stv 0.99 0.99))   (10) )

      ; we apply the lighter to it
      (Sentence ( (Inheritance (Concept X) (Concept applyLighter))   (stv 0.99 0.99))   (11) )


   )
)


!(Question (kbdynamic) (Inheritance (Concept X) (Concept melted)) 3)

; doesnt work?
;           (Inheritance (Concept X) (Concept pliable)) 4)

