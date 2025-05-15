;; TRUTH FORMULAS

; Deduction formula: PLN book "1.4 Truth-value Formulas", page 15:
; borrowed from https://github.com/trueagi-io/hyperon-pln/blob/main/metta/pln/dependent-types/DeductionDTL.metta
(= (Truth_Deduction (stv $Ps $Pc)
                    (stv $Qs $Qc)
                    (stv $Rs $Rc)
                    (stv $PQs $PQc)
                    (stv $QRs $QRc))
   (if (and (conditional-probability-consistency $Ps $Qs $PQs)
            (conditional-probability-consistency $Qs $Rs $QRs))
       ;; Preconditions are met
       (stv (if (< 0.9999 $Qs)                  ; avoid division by 0
                ;; Qs tends to 1
                $Rs
                ;; Otherwise
                (+ (* $PQs $QRs) (/ (* (- 1 $PQs) (- $Rs (* $Qs $QRs))) (- 1 $Qs))))
            (min $Pc (min $Qc (min $Rc (min $PQc $QRc)))))
       ;; Preconditions are not met
       (stv 1 0)))

; Induction formula: PLN book "Appendix A: Comparison of PLN Rules with NARS Rules", page 307
(= (Truth_Induction (stv $sA $cA)
                    (stv $sB $cB)
                    (stv $sC $cC)
                    (stv $sAB $cAB)
                    (stv $sBC $cBC))
   (stv (+ (/ (* (* $sBA $sBC) $sB)
              $sA)
           (* (- 1 (/ (* $sBA $sB) 
                      $sA))
              (/ (- $sC (* $sB $sBC))
                 (- 1 $sB))))
        (Truth_w2c (min $cAB $cBc)))) ;confidence TODO check

; Abduction formula: PLN book "Appendix A: Comparison of PLN Rules with NARS Rules", page 307
(= (Truth_Abduction (stv $sA $cA)
                    (stv $sB $cB)
                    (stv $sC $cC)
                    (stv $sAB $cAB)
                    (stv $sBC $cBC))
   (stv (+ (/ (* (* $sAB $sCB) $sC)
              $sB)
           (/ (* sC (* (- 1 sAB) (- 1 sCB)))
              (- 1 sB)))
        (Truth_w2c (min $cAB $cBc)))) ;confidence TODO check

;Modus Ponens: PLN book "5.7.1 Modus Ponens", page 111:
(= (Truth_ModusPonens (stv $f1 $c1) (stv $f2 $c2)) 
   (stv (+ (* $f1 $f2) (* 0.02 (- 1 $f1)))
        (min $c1 $c2)))

;Revision: PLN Book "5.10.2 A Heuristic Revision Rule for Simple Truth-values", page 116:
(: Truth_c2w (-> Number Number))
(= (Truth_c2w $c)
   (/ $c (- 1 $c)))

(: Truth_w2c (-> Number Number))
(= (Truth_w2c $w)
   (/ $w (+ $w 1)))

(= (Truth_Revision (stv $f1 $c1) (stv $f2 $c2))
   (let* (($w1 (Truth_c2w $c1)) ($w2 (Truth_c2w $c2)) ($w  (+ $w1 $w2))
          ($f (/ (+ (* $w1 $f1) (* $w2 $f2)) $w)) ($c (Truth_w2c $w)))
          (stv (min 1.00 $f) 
               (min 1.0 (max (max $c $c1) $c2)))))

; negation
; see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/negation-introduction.scm#L41
(= (Truth_negationIntroduction (stv $s $c))
   (stv (- 1.0 $s) $c))

; negation elimination
; see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/not-elimination.scm#L34
(= (Truth_negationElimination (stv $s $c))
   (stv (- 1.0 $s) $c))




(= (Truth_transitiveSimilarity (stv $sA $ca) (stv $sB $cB) (stv $sC $cC) (stv $sAB $cAB) (stv $sBC $cBC))
   ; compute strength
   ; see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/formulas.scm#L166
   (let*
        (($T1 (/ (* (+ 1 (/ $sB $sA)) $sAB) (+ 1 $sAB)))
         ($T2 (/ (* (+ 1 (/ $sC $sB)) $sBC) (+ 1 $sBC)))
         ($T3 (/ (* (+ 1 (/ $sB $sC)) $sBC) (+ 1 $sBC)))
         ($T4 (/ (* (+ 1 (/ $sA $sB)) $sAB) (+ 1 $sAB)))
         ($sConcl (invert (- (+ (invert (+ (* $T1 $T2) (* (negate $T1) (/ (- $sC (* $sB $T2)) (negate $sB)))))
                      (invert (+ (* $T3 $T4) (* (negate $T3) (/ (- $sC (* $sB $T4)) (negate $sB)))))) 1.0))))
            (stv  $sConcl  (min $cAB $cBC))
   )
)




; OrBreakdownRule   see https://github.com/hansonrobotics/opencog/blob/aec9b1f267ebad0962dabd1fc34f1bdfcae938ee/opencog/pln/rules/or-breakdown-rule.scm
(= (Truth_OrBreakdown (stv $sOrAB $cOrAB) (stv $sA $cA))
   (stv (/ $sOrAB (- 1.0 $sA))  ; Use 1.0 for floating point arithmetic
        (min $cOrAB $cA)))


; SymmetricModusPonensRule   see https://github.com/hansonrobotics/opencog/blob/aec9b1f267ebad0962dabd1fc34f1bdfcae938ee/opencog/pln/rules/symmetric-modus-ponens-rule.scm
(= (Truth_SymmetricModusPonens (stv $sA $cA) (stv $sAB $cAB))
   (let* (($s_notAB 0.2)  ; Hardcoded from the Scheme formula
          ($c_notAB 1.0)) ; Hardcoded from the Scheme formula
     (stv (+ (* $sA $sAB)
             (* (* $s_notAB (negate $sA)) (+ 1.0 $sAB)))
          (min (min $cAB $c_notAB) $cA))))


(= (ImplicationImplicantConjunctionStrength $sC $sAC $sBC)
   (min (/ (* $sAC $sBC) $sC) 1.0)) ; Capped at 1.0 as in Scheme

(= (ImplicationImplicantConjunctionConfidence $cAC $cBC)
   (min $cAC $cBC))

; ImplicationImplicantConjunctionRule   see https://github.com/opencog/pln/blob/75815f9f21ad899f44eccba1921fa253a7c6216f/opencog/pln/rules/wip/implication-implicant-conjunction.scm
; Implication Implicant Conjunction Rule
(= (Truth_ImplicationImplicantConjunction (stv $sA $cA) (stv $sB $cB) (stv $sC $cC) (stv $sAC $cAC) (stv $sBC $cBC))
   (let* (($strength-A $sA) ($strength-B $sB) ($strength-C $sC)
          ($strength-AC $sAC) ($strength-BC $sBC)
          ($conf-AC $cAC) ($conf-BC $cBC))
     ; Conditional probability consistency checks:
     ; P(C|A) <= P(C)/P(A)  =>  $sAC <= $sC / $sA
     ; P(C|B) <= P(C)/P(B)  =>  $sBC <= $sC / $sB
     ; Also ensure denominators are not zero for the checks.
     (if (and (> $strength-A 0) (> $strength-B 0) (> $strength-C 0) ; Avoid division by zero and ensure meaningful probabilities
              (<= $strength-AC (/ $strength-C $strength-A))
              (<= $strength-BC (/ $strength-C $strength-B)))
         (stv (ImplicationImplicantConjunctionStrength $strength-C $strength-AC $strength-BC)
              (ImplicationImplicantConjunctionConfidence $conf-AC $conf-BC))
         Empty ; Or another symbol indicating failure/conditions not met.
               ; The rule interpreter should handle Empty TV by not adding the conclusion.
     )
   )
)



; --- Truth Value Calculation for Implication Implicant Disjunction ---
;
; Formula based on the Scheme *code*:
; Numerator: P(C,A) + P(C,B) - P(C,A) * P(C,B) * P(A) * P(B)
; Denominator: P(A) + P(B) - P(A)P(B)
(= (ImplicationImplicantDisjunctionStrength_CODE $sA $sB $sAC $sBC)
   (let* (($CinterA (* $sAC $sA))
          ($CinterB (* $sBC $sB))
          ($Numerator (+ $CinterA $CinterB (* $CinterA $CinterB $sA $sB -1.0)))
          ($Denominator (+ $sA $sB (* $sA $sB -1.0))))
     (if (== $Denominator 0.0) ; Avoid division by zero, return neutral/undefined strength
         0.5 ; Or handle as error, or return specific STV like (stv 0.5 0.0)
         (/ $Numerator $Denominator))))

; Formula based on the Scheme *comment* (more standard probabilistic interpretation):
; Numerator: P(C,A) + P(C,B) - P(C,A)P(C,B)  (assuming P(C,A,B) = P(C,A)P(C,B))
; Denominator: P(A) + P(B) - P(A)P(B)
;(= (ImplicationImplicantDisjunctionStrength_COMMENT $sA $sB $sAC $sBC)
;   (let* (($CinterA (* $sAC $sA))
;          ($CinterB (* $sBC $sB))
;          ($Numerator (+ $CinterA $CinterB (* $CinterA $CinterB -1.0)))
;          ($Denominator (+ $sA $sB (* $sA $sB -1.0))))
;     (if (== $Denominator 0.0)
;         0.5
;         (/ $Numerator $Denominator))))

; ImplicationImplicantDisjunctionRule   see https://github.com/opencog/pln/blob/75815f9f21ad899f44eccba1921fa253a7c6216f/opencog/pln/rules/wip/implication-implicant-disjunction.scm
; We will use the _CODE version to match the provided Scheme code.
(= (Truth_ImplicationImplicantDisjunction (stv $sA $cA) (stv $sB $cB) (stv $sAC $cAC) (stv $sBC $cBC))
   (stv (ImplicationImplicantDisjunctionStrength_CODE $sA $sB $sAC $sBC)
        (min $cAC $cBC)))


; EquivalenceToImplicationRule   see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/equivalence-to-implication.scm
; --- Equivalence to Implication Truth Value Formula ---
; Inputs:
;   $stvA: Truth value of Concept A (e.g., (stv $sA $cA))
;   $stvB: Truth value of Concept B (e.g., (stv $sB $cB))
;   $stvEQ: Truth value of (Equivalence A B) (e.g., (stv $sEQ $cEQ))
(= (Truth_EquivalenceToImplication (stv $sA $cA) (stv $sB $cB) (stv $sEQ $cEQ))
   (let* (($sAB (if (> (* $sEQ $cEQ) 0.99) ; Scheme: (< 0.99 (* sEQ cEQ))
                   $sEQ
                   (/ (* (+ 1.0 (/ $sB $sA)) $sEQ) (+ 1.0 $sEQ))
               )))
     (stv $sAB $cEQ) ; Confidence of conclusion is confidence of Equivalence premise
   )
)



; --- Implication Implicant Disjunction Strength Formula ---
; Corresponds to the arithmetic in implication-implicant-disjunction-side-effect-free-formula
; Inputs: sA, sB (strengths of concepts A, B)
;         sAC, sBC (strengths of implications A=>C, B=>C)
(= (ImplicationImplicantDisjunctionStrength $sA $sB $sAC $sBC)
   (let* (($CinterA (* $sAC $sA))
          ($CinterB (* $sBC $sB))
          ($Numerator (+ $CinterA $CinterB (* $CinterA $CinterB $sA $sB -1.0))) ; As per Scheme code
          ; Alternative Numerator based on comment P(C,A,B)=P(C,A)P(C,B):
          ; ($Numerator (+ $CinterA $CinterB (* $CinterA $CinterB -1.0)))
          ($Denominator (+ $sA $sB (* $sA $sB -1.0))))
     (if (== $Denominator 0.0) ; Avoid division by zero, return neutral or error
         0.0 ; Or some other appropriate value like an Undefined STV
         (/ $Numerator $Denominator))))

; ImplicationImplicantDisjunctionRule   see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/implication-implicant-disjunction.scm
; --- Truth Value Calculation for Implication Implicant Disjunction ---
; Takes STVs of concepts A, B, C and STVs of premises (A => C), (B => C)
(= (Truth_ImplicationImplicantDisjunction (stv $sA $cA) (stv $sB $cB) (stv $sC $cC) (stv $sAC $cAC) (stv $sBC $cBC))
   (stv (ImplicationImplicantDisjunctionStrength $sA $sB $sAC $sBC)
        (min $cAC $cBC)))
        ; Note: $sC and $cC are passed (as C is extracted in Scheme) but $sC is not used in the strength formula.








; Or Introduction Rule   see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/or-introduction.scm
; --- Or Introduction Truth Value Formula ---
; Takes the truth values of A and B, returns the truth value for (Or A B).
;(= (Truth_OrIntroduction (stv $sA $cA) (stv $sB $cB))
;   (stv (- (+ $sA $sB) (* $sA $sB))  ; Strength: sA + sB - sA*sB
;        (min $cA $cB)))             ; Confidence: min(cA, cB)



; And Introduction Rule   see https://github.com/opencog/pln/blob/master/opencog/pln/rules/wip/and-introduction.scm
; --- Truth Value Calculation for Conjunction (And) ---
; Takes the truth values of A and B, returns the truth value for (And A B).
; $tvA = (stv $sA $cA)
; $tvB = (stv $sB $cB)
;(= (Truth_And (stv $sA $cA) (stv $sB $cB))
;   (stv (* $sA $sB) (min $cA $cB)))
