
module Main where

-- build and run with
--    cabal build && cabal run && python OUTplotA.py

import Control.Monad

import Control.Monad.ST
import Data.Array.ST
import Data.Array

--import System.Random.MWC (createSystemRandom)
import Control.Monad.Primitive (PrimMonad)

import System.Random.MWC
import Statistics.Distribution  (ContGen(..))
import Statistics.Distribution.Beta  (betaDistr)


-- for conversion of [Double] to python code of real valued list
import Data.List (intersperse)


-- for writing script to plot to file
import System.IO

convCtoW :: Double -> Double
convCtoW conf =
    conf / (1.0 - conf)

convWtoC :: Double -> Double
convWtoC w =
    w / (w + 1.0)

data StvType =
    Stv Double Double
    deriving (Show, Eq)

convAlphaBetaToStv :: (Double, Double) -> StvType
convAlphaBetaToStv (alpha, beta) =
    let
        weightPos = alpha - 1.0
        weightNeg = beta  - 1.0
        w = weightPos + weightNeg

        -- strength (similar to "belief" ratio)
        strength = weightPos / w

        -- confidence derived from weight
        conf = convWtoC w
    in
        Stv strength conf


-- convert STV to alpha and beta
convStvToAlphaBeta :: StvType -> (Double, Double)
convStvToAlphaBeta (Stv strength conf) =
    let
        w = convCtoW conf

        -- #print(f'DBG strength={strength}')
        -- #print(f'DBG w       ={w}')
    
        weightPos = strength * w

        alpha = weightPos + 1.0
        beta = (w - weightPos) + 1.0
    in
        (alpha, beta)


checkIsAlphaBetaValidBayesPrior :: (Double, Double) -> Bool
checkIsAlphaBetaValidBayesPrior (alpha, beta) =
    -- a valid bayes prior has to have alpha > 1.0 and beta > 1.0
    alpha > 1.0 && beta > 1.0


-- check for NaN and return 0.0 if it is a NaN
safeNaN :: Double -> Double
safeNaN x = if isNaN x then 0.0 else x


convIntToFloat :: Int -> Double
convIntToFloat x = fromIntegral x

convFloatToInt :: Double -> Int
convFloatToInt x = truncate x

-- from Nil's math for conjunction TV-fn
--
-- see https://github.com/opencog/tv-toolbox/blob/master/README.md for explaination of the formula
--
-- matpr(n, pa, pb, x) :=
--  sqrt(pa pb (1 - pa)(1 - pb)) / sqrt(2 pi n (pa - x)(pb - x) x(x - pb - pa + 1) *
--  
--  (
--  pa**pa pb**pb (1 - pa)**(1-pa) (1 - pb)**(1-pb)  /
--     (pa - x)**(pa - x) (pb - x)**(pb-x) x**x (x - pb - pa + 1)**(x - pb - pa + 1)
--   ) 
--   ** n

{-
calcMatpr :: Int -> Double -> Double -> Double -> Double
calcMatpr n pa pb x =
    let
        termMulA = sqrt(pa * pb * (1.0 - pa)*(1.0 - pb)) / sqrt(2.0 * pi * (convIntToFloat n) * (pa - x) * (pb - x) * x * (x - pb - pa + 1.0))   :: Double 
        termMulB = ( ( pa**pa    * pb**pb  * (1.0 - pa)**(1.0-pa) * (1.0 - pb)**(1.0-pb) )   /     ( (pa - x)**(pa - x) * (pb - x)**(pb-x) * x**x * (x - pb - pa + 1.0)**(x - pb - pa + 1.0) )  )   ** (convIntToFloat n)   :: Double 
    
    in termMulA * termMulB
-}






normalizeDistr :: [Double] -> [Double]
normalizeDistr xs =
    let total = sum xs
        factor = (1.0 / total) -- * (1.0 / convIntToFloat (length xs))
    in if total == 0 || isNaN total || isInfinite total
        then replicate (length xs) 0.0
        else map (* factor) xs

-- Given a normalized distribution and a selection value between 0.0 and 1.0,
--
-- return the index where the cumulative sum >= selVal.
findIndexByCumulative :: [Double] -> Double -> Int
findIndexByCumulative distr selVal =
        go distr selVal 0 0.0
    where
        go [] _ idx _ = idx - 1  -- should not happen for well-formed distributions
        go (x:xs) target idx acc
            | acc + x >= target = idx
            | otherwise          = go xs target (idx + 1) (acc + x)



-- type for tuple of probability of premise A and probability of premise B  and probability of uniform distr AB
data ProbTupleType =
    ProbTuple Double Double Double
    deriving (Show, Eq)



-- processes the pairs of (PA, PB, PAB) and aggregates the distributions to one distribution
processPairsToAggregatedDistr :: [ProbTupleType] -> [Double]
processPairsToAggregatedDistr pairs = runST $ do
    
    
    
    let
        nilHorizonN = 100   :: Int -- Nil's "horizon" parameter "n"
        nBuckets = 201   :: Int
    
    arrDistrC <- newArray (0, nBuckets-1) 0.0   :: ST s (STArray s Int Double)
    
    arrDistrCTemp <- newArray (0, nBuckets-1) 0.0   :: ST s (STArray s Int Double)
    
    

    forM_ pairs $ \(ProbTuple pA pB pAb) -> do
        
        {- code for using stirling distribution
        -- compute distribution for this sample  (pa pb)
        forM_ [0 .. nBuckets-1] $ \idx -> do
            let x = convIntToFloat idx / convIntToFloat nBuckets + (1.0 / convIntToFloat nBuckets) * 0.5   :: Double
                val = safeNaN  $  calcMatpr nilHorizonN pa pb x
            writeArray arrDistrCTemp idx val
        
        -- add distribution "arrDistrCTemp" to distribution "arrDistrC"
        forM_ [0..nBuckets-1] $ \idx -> do
            a <- readArray arrDistrCTemp idx
            b <- readArray arrDistrC idx
            writeArray arrDistrC idx (a + b)
        -}
        
        
        {-
        -- put sample directly into conclusion distribution without bernoilli/stirling
        let conclPhi = pa * pb
        let idxWrite = convFloatToInt ( conclPhi * (convIntToFloat (nBuckets - 1) ) )
        valAtIdx <- readArray arrDistrC idxWrite
        writeArray arrDistrC idxWrite (valAtIdx + 1.0) -- put sample
        -}
        
        
        
        -- condition
        -- see https://github.com/ngeiswei/tv-toolbox/tree/beta-product-xp
        
        let boundLower = max (pA+pB-1.0) 0.0
        let boundUpper = min pA pB
        
        let conclPhi   = boundLower + (boundUpper-boundLower)*pAb
        
        let idxWrite = convFloatToInt ( conclPhi * (convIntToFloat (nBuckets - 1) ) )
        valAtIdx <- readArray arrDistrC idxWrite
        writeArray arrDistrC idxWrite (valAtIdx + 1.0) -- put sample
    
    
    getElems arrDistrC





-- sample distribution by array of CDF values (where to sample)
sampleDistrBy :: [Double] -> [Double] -> [Double]
sampleDistrBy distr arrSampleCdf =
    let n = length distr
    in map (\selVal ->
                let idx = findIndexByCumulative distr selVal
                in convIntToFloat idx / convIntToFloat (n - 1)
           ) arrSampleCdf



convToPythonArray :: [Double] -> String
convToPythonArray xs =
    "[" ++ (concat . intersperse ", " . map show) xs ++ "]"




-- convert from distr with sum 1.0 to normalized PDF distribution
convNormalizedDistrToPdfDistr :: [Double] -> [Double]
convNormalizedDistrToPdfDistr arr =
	-- TODO TODO TODO
	arr



-- Builds the full Python script string
buildPythonPlotScript :: [Double] -> String
buildPythonPlotScript arr =
    unlines
        [ "import matplotlib.pyplot as plt"
        , ""
        , "data = " ++ convToPythonArray arr
        , ""
        , "# Plot the data"
        , "plt.plot(data,)"
        , "plt.title('DistConcl')"
        , "plt.xlabel('idxBucket')"
        , "plt.ylabel('count')"
        , ""
        , "# Save the plot as an image"
        , "plt.savefig('OUTplotA.png', dpi=300, bbox_inches='tight')"
        ]

-- Writes the script to OUTplotA.py
writePythonPlot :: [Double] -> IO ()
writePythonPlot arr = do
    let script = buildPythonPlotScript arr
    writeFile "OUTplotA.py" script



z2 :: IO ()
z2 = do
    
    let nSamplesPerPair = 35501
    
    
    
    -- naive result: case with high concl.conf and concl.strength ~ 0.5*0.5
    --let stvPremiseA = Stv 0.5 0.89
    --let stvPremiseB = Stv 0.5 0.89
    
    -- naive result: gives result where  concl.strength  is almost  a.strength*b.strength
    let stvPremiseA = Stv 0.7 0.98
    let stvPremiseB = Stv 0.7 0.98
    
    -- naive result: case with to high uncorrected conf
    --let stvPremiseA = Stv 0.2 0.89
    --let stvPremiseB = Stv 0.2 0.89
    
    -- naive result: case with to high conf even with correction
    -- fixed with new XP variant!
    --let stvPremiseA = Stv 0.95 0.89
    --let stvPremiseB = Stv 0.95 0.89
    
    
    
    -- naive result: interesting conclusion
    --let stvPremiseA = Stv 0.5 0.29
    --let stvPremiseB = Stv 0.7 0.39
    
    -- naive result: gives uncertain conclusion which is skewed towards 0.0
    --let stvPremiseA = Stv 0.7 0.98
    --let stvPremiseB = Stv 0.7 0.2
    
    

    -- naive result: case when concl.conf > min( a.conf, b.conf )
    --let stvPremiseA = Stv 0.98 0.89
    --let stvPremiseB = Stv 0.98 0.89
    
    -- naive result: case when conf of concl is to low
    --let stvPremiseA = Stv 0.2 0.7
    --let stvPremiseB = Stv 0.2 0.7
    
    -- naive result: case when conf of concl is to low
    --let stvPremiseA = Stv 0.5 0.3
    --let stvPremiseB = Stv 0.5 0.3
    
    --
    --let stvPremiseA = Stv 0.2 0.9
    --let stvPremiseB = Stv 0.2 0.9
    -- concl  Stv 1.859179797190865e-2 0.9509574018435561
    
    -- naive result: case when conf of concl is to low
    --let stvPremiseA = Stv 0.2 0.7
    --let stvPremiseB = Stv 0.2 0.7
    -- concl Stv (-2.212187522538186e-2) 0.8458145557750192
    
    -- ?
    --let stvPremiseA = Stv 0.5 0.9
    --let stvPremiseB = Stv 0.9 0.5
    
    --let stvPremiseA = Stv 0.5 0.95
    --let stvPremiseB = Stv 0.5 0.95
    
    let (premiseAAlpha, premiseABeta) = convStvToAlphaBeta stvPremiseA
    let (premiseBAlpha, premiseBBeta) = convStvToAlphaBeta stvPremiseB
    
    print (premiseAAlpha, premiseABeta)

    
    gen <- createSystemRandom
    
    -- generate pairs of sample which is  Prob of A and Prob of B
    pairs <- replicateM nSamplesPerPair $ do
        --phiA <- genContVar (betaDistr 15.3 5.9) gen
        --phiB <- genContVar (betaDistr 15.3 5.9) gen
        phiA <- genContVar (betaDistr premiseAAlpha premiseABeta) gen
        phiB <- genContVar (betaDistr premiseBAlpha premiseBBeta) gen
        phiAb <- uniform gen
        return (ProbTuple phiA phiB phiAb)

    -- DBG
    --print pairs
    
    let distrConcl = normalizeDistr  $  processPairsToAggregatedDistr pairs   :: [Double]
    
    -- DBG conclusion distribution
    --print distrConcl
    
    
    
    --let strA = convToPythonArray distrConcl
    
    -- DBG conclusion distribution
    --print strA
    
    -- generate python code to plot distribution
    let distrConclAsPdf = convNormalizedDistrToPdfDistr distrConcl
    writePythonPlot distrConclAsPdf
    
    
    let nSamplesOfConcl = 10000
    
    -- generate random numbers we use for CDF sampling of conclusion distribution
    arrConclSampleCdf <- replicateM nSamplesOfConcl (uniformR (0.0 :: Double, 1.0 :: Double) gen) -- generate random doubles in [0,1)
    
    
    -- sample from the conclusion  using CDF array
    let arrConclSamples = sampleDistrBy distrConcl arrConclSampleCdf
    
    -- DBG
    --print arrConclSamples
    
    
    -- now we >estimate< the parameters of the beta distribution
    
    let
        n = convIntToFloat (length arrConclSamples)
        -- * calculate sample mean
        sampleMean = sum arrConclSamples / n

        -- * calculate variance
        diffs = map (\x -> (x - sampleMean) ** 2) arrConclSamples
        sampleVar = sum diffs / (n - 1.0)

        -- now we put "sampleMean" and "sampleVar" into the formula of the method-of-moments estimate
        helperInner = (sampleMean * (1.0 - sampleMean)) / sampleVar
        alphaRoof = sampleMean * (helperInner - 1.0)
        betaRoof  = (1.0 - sampleMean) * (helperInner - 1.0)
    
    print ""
    print "sample mean    sample variance"
    print (sampleMean, sampleVar)
    
    print ""
    print "estimated alpha and beta:"
    print (alphaRoof, betaRoof)
    
    
    -- * convert alpha and beta to STV
    let stvConclBeforeCorrection = convAlphaBetaToStv (alphaRoof, betaRoof)
    
    print stvConclBeforeCorrection
    
    
    -- * correct evidence
    let (Stv conclStrengthBeforeCorrection conclConfBeforeCorrection) = stvConclBeforeCorrection
    let weightBeforeCorrection = convCtoW conclConfBeforeCorrection
    
    let (Stv premiseAStrength _) = stvPremiseA
    let (Stv premiseBStrength _) = stvPremiseB
    let weightCorrectionFactor = premiseAStrength*premiseBStrength
    
    let weightAfterCorrection = weightBeforeCorrection * weightCorrectionFactor
    let conclConfAfterCorrection = convWtoC weightAfterCorrection
    let stvConclAfterCorrection = Stv conclStrengthBeforeCorrection conclConfAfterCorrection
    let stvConcl = stvConclAfterCorrection
    
    print stvConcl 
    
    -- we need to check if the parameterization of the beta distribution is a valid bayes prior
    let conclIsValid = checkIsAlphaBetaValidBayesPrior (convStvToAlphaBeta stvConcl)
    
    print "conclIsValid="
    print conclIsValid
    
    
    return ()
    
    




main :: IO ()
main = do


    z2
    
    return ()
    


{-
-- calculate Nil's new version of the TVfn 
calcBetaProductXp :: Double -> Double -> IO ()
calcBetaProductXp pA pB = do
	
	
	gen <- createSystemRandom
	
	-- loop to find pAb which satisfies the constraint
	let loop currentA = do
         
		-- sample pAb from beta(1.0, 1.0)
		pAb <- genContVar (betaDistr 1.0 1.0) gen
	
	
		-- check for condition
		-- see https://github.com/ngeiswei/tv-toolbox/tree/beta-product-xp
	
		let constraint = max (pA+pB+1.0) 0.0 <= pAb && pAb <= min pA pB
	
		if constraint
			then return pAb
			else do
				loop 0
	
	let pAb = loop 0
	
	return pAb
-}
