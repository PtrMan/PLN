;; INFERENCE RULES

;Revision
(= (|- ($T $T1) 
       ($T $T2))
       ($T (Truth_Revision $T1 $T2)))

;Modus Ponens
(= (|- ($A $T1) 
       (((XXX Implication) $A $B) $T2))
   ($B (Truth_ModusPonens $T1 $T2)))


; SymmetricModusPonens rule   see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/symmetric-modus-ponens.scm
(= (|- ($A $TruthA)
       (((XXX Similarity) $A $B) $TruthAB))
   ($B (Truth_SymmetricModusPonens $TruthA $TruthAB)))

; SymmetricModusPonens rule   see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/symmetric-modus-ponens.scm
(= (|- ($A $TruthA)
       (((XXX IntentionalSimilarity) $A $B) $TruthAB))
   ($B (Truth_SymmetricModusPonens $TruthA $TruthAB)))

; SymmetricModusPonens rule   see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/symmetric-modus-ponens.scm
(= (|- ($A $TruthA)
       (((XXX ExtensionalSimilarity) $A $B) $TruthAB))
   ($B (Truth_SymmetricModusPonens $TruthA $TruthAB)))


;Deduction
(= (|- (((XXX Inheritance) (Concept $A) (Concept $B)) $T1)
       (((XXX Inheritance) (Concept $B) (Concept $C)) $T2))
       (((XXX Inheritance) (Concept $A) (Concept $C))
        (Truth_Deduction (STV (Concept $A))
                         (STV (Concept $B))
                         (STV (Concept $C)) $T1 $T2)))

;Induction
(= (|- (((XXX Inheritance) (Concept $C)
                     (Concept $A)) $T1)
       (((XXX Inheritance) (Concept $C)
                     (Concept $B)) $T2))
       (((XXX Inheritance) (Concept $A)
                     (Concept $B)) (Truth_Induction (STV (Concept $A))
                                                    (STV (Concept $B))
                                                    (STV (Concept $C)) $T1 $T2)))
;Abduction
(= (|- (((XXX Inheritance) (Concept $A) (Concept $C)) $T1)
       (((XXX Inheritance) (Concept $B) (Concept $C)) $T2))
       (((XXX Inheritance) (Concept $A) (Concept $B))
        (Truth_Abduction (STV (Concept $A))
                         (STV (Concept $B))
                         (STV (Concept $C)) $T1 $T2)))


;Usage of inheritance for predicates
;unary arg
(= (|- (((XXX Evaluation) (Predicate $x)
                    ((XXX List) (Concept $C))) $T1)
       (((XXX Inheritance) (Concept $S) (Concept $C)) $T2))
       (((XXX Evaluation) (Predicate $x)
                    ((XXX List) (Concept $S))) (Truth_ModusPonens $T1 $T2)))

;binary arg1
(= (|- (((XXX Evaluation) (Predicate $x)
                    ((XXX List) (Concept $C1) (Concept $C2))) $T1)
       (((XXX Inheritance) (Concept $S) (Concept $C1)) $T2))
       (((XXX Evaluation) (Predicate $x)
                    ((XXX List) (Concept $S) (Concept $C2))) (Truth_ModusPonens $T1 $T2)))

;binary arg2
(= (|- (((XXX Evaluation) (Predicate $x)
                    ((XXX List) (Concept $C1) (Concept $C2))) $T1)
       (((XXX Inheritance) (Concept $S) (Concept $C2)) $T2))
       (((XXX Evaluation) (Predicate $x)
                    ((XXX List) (Concept $C1) (Concept $S))) (Truth_ModusPonens $T1 $T2)))

; negation introduction
; takes only one premise (is unary)
(= (|- ($x $t))
       (if (not (== $x ((XXX NotLink) $_))) ;don't derive if it is already a NotLink
           (((XXX NotLink) $x) (Truth_negationIntroduction $t)) ()))

; not elimination rule
; see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/not-elimination.scm#L1-L8
(= (|- (((XXX NotLink) $x) $t))
       ($x (Truth_negationElimination $t)))

