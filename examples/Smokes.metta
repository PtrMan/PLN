(= (STV (Concept Anna))
   (stv 0.1667 1))

(= (STV (Concept Bob))
   (stv 0.1667 1))

(= (STV (Concept Edward))
   (stv 0.1667 1))

(= (STV (Concept Frank))
   (stv 0.1667 1))

(= (STV (Concept Gary))
   (stv 0.1667 1))

(= (STV (Concept Helen))
   (stv 0.1667 1))

!(Question (
            (Sentence (((XXX Implication) ((XXX Evaluation) (Predicate friend) 
                                                ((XXX List) $1 $2))
                                    ((XXX Implication) ((XXX Evaluation) (Predicate smokes) 
                                                             ((XXX List) $1)) 
                                                 ((XXX Evaluation) (Predicate smokes)
                                                             ((XXX List) $2))))
                       (stv 0.4 0.9)) (1))

            (Sentence (((XXX Implication) ((XXX Evaluation) (Predicate smokes)
                                                ((XXX List) $1)) 
                                    ((XXX Evaluation) (Predicate cancerous) (List $1))) 
                       (stv 0.6 0.9)) (2))

            (Sentence (((XXX Evaluation) (Predicate friend) 
                                   ((XXX List) (Concept Anna) 
                                         (Concept Bob))) 
                       (stv 1.0 0.9)) (3))

            (Sentence (((XXX Evaluation) (Predicate friend) 
                                   ((XXX List) (Concept Anna) 
                                         (Concept Edward)))
                       (stv 1.0 0.9)) (4))

            (Sentence (((XXX Evaluation) (Predicate friend) 
                                   ((XXX List) (Concept Anna)
                                         (Concept Frank)))
                       (stv 1.0 0.9)) (5))

            (Sentence (((XXX Evaluation) (Predicate friend)
                                   ((XXX List) (Concept Edward)
                                         (Concept Frank)))
                       (stv 1.0 0.9)) (6))

            (Sentence (((XXX Evaluation) (Predicate friend)
                                   ((XXX List) (Concept Gary)
                                         (Concept Helen)))
                       (stv 1.0 0.9)) (7))

            (Sentence (((XXX Evaluation) (Predicate friend)
                                   ((XXX List) (Concept Gary) 
                                         (Concept Frank)))
                       (stv 0.0 0.9)) (8))

            (Sentence (((XXX Evaluation) (Predicate smokes)
                                   ((XXX List) (Concept Anna)))
                       (stv 1.0 0.9)) (9))

            ;(Sentence (((XXX Evaluation) (Predicate smokes) 
            ;                       ((XXX List) (Concept Edward)))
            ;          (stv 1.0 0.9)) (10))
           )

           ((XXX Evaluation) (Predicate cancerous)
                       ((XXX List) (Concept Edward)))

           3)
