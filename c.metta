!(bind! nn0 (py-atom nn0))

(= (calcTruthNn_conj $aStrength $aConf  $bStrength $bConf)
    ((py-dot nn0 nnA (-> Number Number Number Number Number Number Expression)) $aStrength (- 1.0 $aStrength) $aConf  $bStrength (- 1.0 $bStrength) $bConf)
)



; !(println! ((py-dot nn0 nnA (-> Number Number Number Number Number Number Expression)) 0.1 0.9 0.8 0.1 0.9 0.8))

!(println! (calcTruthNn_conj 0.1 0.8  0.1 0.8))

!(println! (calcTruthNn_conj 0.9 0.8  0.9 0.8))

!(println! (calcTruthNn_conj 0.01 0.8  0.01 0.8))

!(println! (calcTruthNn_conj 0.99 0.8  0.99 0.8))



!(println! "")
!(println! "result should be ideally 0.25   A")


!(println! (calcTruthNn_conj 0.1 0.8  0.9 0.8))
!(println! (calcTruthNn_conj 0.9 0.8  0.1 0.8))


!(println! "")
!(println! "result should be ideally 0.25   B")

!(println! (calcTruthNn_conj 0.02 0.8  0.98 0.8))
!(println! (calcTruthNn_conj 0.98 0.8  0.02 0.8))


