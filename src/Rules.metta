;; INFERENCE RULES

;Revision
(= (|- ($T $T1) 
       ($T $T2))
       ($T (Truth_Revision $T1 $T2)))

;Modus Ponens
(= (|- ($A $T1) 
       ((Implication $A $B) $T2))
   ($B (Truth_ModusPonens $T1 $T2)))

;Deduction
(= (|- ((Inheritance (Concept $A) (Concept $B)) $T1) 
       ((Inheritance (Concept $B) (Concept $C)) $T2))
       ((Inheritance $A $C) 
        (Truth_Deduction (STV $A) 
                         (STV $B)
                         (STV $C) $T1 $T2)))

;Induction
(= (|- ((Inheritance (Concept $C) 
                     (Concept $A)) $T1)
       ((Inheritance (Concept $C) 
                     (Concept $B)) $T2))
       ((Inheritance $A 
                     $B) (Truth_Induction (STV $A) 
                                          (STV $B)
                                          (STV $C) $T1 $T2)))
;Abduction
(= (|- ((Inheritance (Concept $A) (Concept $C)) $T1)
       ((Inheritance (Concept $B) (Concept $C)) $T2))
       ((Inheritance $A $B)
        (Truth_Abduction (STV $A) (STV $B) (STV $C) $T1 $T2)))


;Usage of inheritance for predicates
;unary arg
(= (|- ((Evaluation (Predicate $x)
                    (List (Concept $C))) $T1)
       ((Inheritance (Concept $S) (Concept $C)) $T2))
       ((Evaluation (Predicate $x)
                    (List (Concept $S))) (Truth_ModusPonens $T1 $T2)))

;binary arg1
(= (|- ((Evaluation (Predicate $x)
                    (List (Concept $C1) (Concept $C2))) $T1)
       ((Inheritance (Concept $S) (Concept $C1)) $T2))
       ((Evaluation (Predicate $x)
                    (List (Concept $S) (Concept $C2))) (Truth_ModusPonens $T1 $T2)))

;binary arg2
(= (|- ((Evaluation (Predicate $x)
                    (List (Concept $C1) (Concept $C2))) $T1)
       ((Inheritance (Concept $S) (Concept $C2)) $T2))
       ((Evaluation (Predicate $x)
                    (List (Concept $C1) (Concept $S))) (Truth_ModusPonens $T1 $T2)))



; negation introduction
; takes only one premise (is unary)
; implemented as binary, 2nd premise is ignored
;(= (|- ($x $t) ($dummyTerm $dummyTruth))
;       ; CONTROL : don't derive if it is already a NotLink
;       (If (not (== $x (NotLink $_)))
;              ((NotLink $x) (Truth_negationIntroduction $t))
;       )
;)

; manual test
; !(println (|- (ZZZ (stv 0.5 0.5)) (ZZZb (stv 0.5 0.5)) ))
; manual test
; !(println (|- ((NotLink ZZZ) (stv 0.5 0.5)) (ZZZb (stv 0.5 0.5)) ))
; ; (must not return anything)


; not elimination rule
; see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/not-elimination.scm#L1-L8
(= (|- ((NotLink $x) $t) ($dummyTerm $dummyTruth))
       ($x (Truth_negationElimination $t))
)


; TransitiveSimilarityRule
; see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/transitive-similarity.scm
; TODO LOW : extend to other link types
(= (|- (((Similarity (Concept $A) (Concept $B)) $T_AB)
        ((Similarity (Concept $B) (Concept $C)) $T_BC)))
   ((Similarity (Concept $A) (Concept $C)) (Truth_transitiveSimilarity (STV $A) (STV $B) (STV $C) $T_AB $T_BC))
)








;; InversionRule   see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/inversion.scm
;;
;; <LinkType>
;;   A
;;   B
;; |-
;; <LinkType>
;;   B
;;   A
;;
; TODO LOW : implement and use inversion-consistency as Guardcondition
(= (|- ((Inheritance (Concept $A) (Concept $B)) $tv)
   ((Inheritance (Concept $B) (Concept $A)) $tv)))


; OrBreakdownRule   see https://github.com/hansonrobotics/opencog/blob/aec9b1f267ebad0962dabd1fc34f1bdfcae938ee/opencog/pln/rules/or-breakdown-rule.scm
(= (|- ((Or $A $B) $tvOrAB)  ; Premise 1: (Or A B) with its TV
       ($A $tvA))            ; Premise 2: A with its TV
   ($B (Truth_OrBreakdown $tvOrAB $tvA))) ; Conclusion: B with calculated TV



; SymmetricModusPonensRule   see https://github.com/hansonrobotics/opencog/blob/aec9b1f267ebad0962dabd1fc34f1bdfcae938ee/opencog/pln/rules/symmetric-modus-ponens-rule.scm
; For SimilarityLink
(= (|- (((Similarity (Concept $A) (Concept $B))) $tvAB)
       (((Concept $A)) $tvA))
   ((Concept $B)
    (Truth_SymmetricModusPonens $tvA $tvAB)))

; SymmetricModusPonensRule   see https://github.com/hansonrobotics/opencog/blob/aec9b1f267ebad0962dabd1fc34f1bdfcae938ee/opencog/pln/rules/symmetric-modus-ponens-rule.scm
; For IntensionalSimilarityLink
(= (|- (((IntensionalSimilarity (Concept $A) (Concept $B))) $tvAB)
       (((Concept $A)) $tvA))
   ((Concept $B)
    (Truth_SymmetricModusPonens $tvA $tvAB)))

; SymmetricModusPonensRule   see https://github.com/hansonrobotics/opencog/blob/aec9b1f267ebad0962dabd1fc34f1bdfcae938ee/opencog/pln/rules/symmetric-modus-ponens-rule.scm
; For ExtensionalSimilarityLink
(= (|- (((ExtensionalSimilarity (Concept $A) (Concept $B))) $tvAB)
       (((Concept $A)) $tvA))
   ((Concept $B)
    (Truth_SymmetricModusPonens $tvA $tvAB)))


; ImplicationImplicantConjunctionRule   see https://github.com/opencog/pln/blob/75815f9f21ad899f44eccba1921fa253a7c6216f/opencog/pln/rules/wip/implication-implicant-conjunction.scm
; Implication Implicant Conjunction Rule
; commented because we don't yet implement preconditions!
; TODO MID : implement preconditions in deriver as some sort of lambda function call. Quote the code here
;(= (|- (((Implication $A $C) $tvAC)                     ; Premise 1: (A => C) has TV $tvAC
;        ((Implication $B $C) $tvBC)                     ; Premise 2: (B => C) has TV $tvBC
;        (IsTrue (not ($A == $B)))))                     ; GuardCondition: A is not identical to B
;                                                        ; (IsTrue ensures the condition must evaluate to True)
;       ((Implication (And $A $B) $C)                    ; Conclusion: ((A & B) => C)
;        (Truth_ImplicationImplicantConjunction (STV $A)  ; TV of the conclusion is calculated by this function
;                                               (STV $B)
;                                               (STV $C)
;                                               $tvAC
;                                               $tvBC)))
;)


; ImplicationImplicantDisjunctionRule   see https://github.com/opencog/pln/blob/75815f9f21ad899f44eccba1921fa253a7c6216f/opencog/pln/rules/wip/implication-implicant-disjunction.scm
; --- Implication Implicant Disjunction Rule ---
; ImplicationLink <TV1>
;    A
;    C
; ImplicationLink <TV2>
;    B
;    C
; |-
; ImplicationLink <TV>
;    OrLink
;       A
;       B
;    C
; Condition: A != B

; In MeTTa, PredicateNode $A becomes (Predicate $A) or similar.
; OrLink A B becomes (Or (Predicate $A) (Predicate $B))
(= (|- ((Implication (Predicate $A) (Predicate $C)) $tvAC)    ; Premise 1: A => C
       ((Implication (Predicate $B) (Predicate $C)) $tvBC)    ; Premise 2: B => C
;       (, (match &self ($A $B) (if (== $A $B) (empty) $SUCCESS))) ; GuardCondition: A is not B
    )
   ((Implication (Or (Predicate $A) (Predicate $B)) (Predicate $C)) ; Conclusion: (A or B) => C
    (Truth_ImplicationImplicantDisjunction (STV (Predicate $A))  ; Truth value of A
                                           (STV (Predicate $B))  ; Truth value of B
                                           $tvAC                 ; Truth value of A => C
                                           $tvBC                 ; Truth value of B => C
                                           )))
   ; Note: The Scheme code retrieves (cog-mean C) but doesn't use it in the formula.
   ; If it were needed, (STV (Predicate $C)) would be passed to Truth_ImplicationImplicantDisjunction.


; EquivalenceToImplicationRule   see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/equivalence-to-implication.scm
(= (|- ((Equivalence (Concept $A) (Concept $B)) (stv $sEQ $cEQ)) ; Premise 1: Match Equivalence and its TV
;       (IsTrue (> $cEQ 0.0))                                    ; GuardCondition: Precondition: confidence of EQ > 0
   )
   ((Implication (Concept $A) (Concept $B))
    (Truth_EquivalenceToImplication (STV (Concept $A))    ; Get STV of Concept A
                                    (STV (Concept $B))    ; Get STV of Concept B
                                    (stv $sEQ $cEQ)))     ; Pass STV of Equivalence premise
)



; ImplicationImplicantDisjunctionRule   see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/implication-implicant-disjunction.scm
; --- Implication Implicant Disjunction Rule ---
; ImplicationLink A C
; ImplicationLink B C
; A != B
; |-
; ImplicationLink (OrLink A B) C
;
; We assume $A, $B, $C are variables representing the core names/identifiers of concepts.
; (Concept $A) constructs the actual concept atom.
(= (|- ((Implication (Concept $A) (Concept $C)) $tvAC)      ; Premise 1: A => C
       ((Implication (Concept $B) (Concept $C)) $tvBC)      ; Premise 2: B => C
;       (Not (== $A $B))                                    ; GuardCondition: A is not identical to B
   )
   ((Implication (Or (Concept $A) (Concept $B)) (Concept $C)) ; Conclusion: (A or B) => C
    (Truth_ImplicationImplicantDisjunction
        (STV (Concept $A))  ; Truth value of Concept A
        (STV (Concept $B))  ; Truth value of Concept B
        (STV (Concept $C))  ; Truth value of Concept C
        $tvAC               ; Truth value of premise A => C
        $tvBC               ; Truth value of premise B => C
    )))





