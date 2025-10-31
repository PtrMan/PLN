


# pip install scipy


import scipy.stats

import random
import math

def clamp(arg, minVal, maxVal):
    return max(min(arg, maxVal), minVal)


# /param p probability of the actual distribution
# /param q probability of the source distribution to learn
def calcKlDivergenceSingle(p, q):

    h = p/q
    #h = max(h, 1e-9)# HACK to avoid numerical error
    
    # HACK to avoid numerical error
    if h == 0.0:
        return math.nan
    

    return p*math.log(h)



'''
# compute alpha and beta of beta distribution given mean and standard-deviation
def calcAlphaAndBetaFromMeanAndStdDev(mean, stdDev):
    #print(mean)
    #print(stdDev)


    n = ( mean * (1.0 - mean) ) / (stdDev*stdDev)
    alpha = mean * n
    beta = (1.0 - mean) * n

    #print(n)
    #print(alpha)
    #print(beta)


    return alpha, beta
'''

def convCtoW(conf):
    return conf / (1.0 - conf)

def convWtoC(w):
    return w / (w + 1.0)

# convert STV to alpha and beta
def convStvToAlphaBeta(strength, conf):
    w = convCtoW(conf)

    #print(f'DBG strength={strength}')
    #print(f'DBG w       ={w}')

    weightPos = strength * w

    alpha = weightPos + 1.0
    beta = w + 1.0 + 1.0

    #print('DBG =>')


    #print(alpha)
    #print(beta)

    #dlpdlpdlplpdlpd

    return (alpha, beta)

def convAlphaBetaToStv(alpha, beta):

    # prior is byes prior. so we must subtract 1.0 to get weights
    weightPos = alpha - 1.0
    weightNeg = beta - 1.0

    w = weightPos + weightNeg

    # calc STV.s
    strength = weightPos / w

    # # calc STV.c
    conf = convWtoC(w)


    return (strength, conf)



###########
# testing area

if False:
    alpha = 2.0
    beta = 1.222

    x = 0.55
    cdfVal = scipy.stats.beta.cdf(x, alpha, beta)
    print(f'cdf={cdfVal}')

    exit(0)




#########

class BucketedDistribution(object):
    def __init__(self):
        pass
	
    def reset(self):
        self.buckets = [0] * 500 # 200

    def putSample(self, phi):
        # add mass at probability "phi" for this sample

        # phi is the chosen probability

        bucketIdx = int(phi * (len(self.buckets)-1))
        #print(bucketIdx) # DBG
        self.buckets[bucketIdx] += 1

    # compute distribution
    def calcDistribution(self):

        # compute mass
        mass = 0
        for iv in self.buckets:
            mass += iv

        phis = []

        for bucketIdx in range(len(self.buckets)):
            # compute probability at the bucketIdx
            phi = float(self.buckets[bucketIdx]) / mass
			
            phis.append(phi)
		
        return phis


# helper class to compute result distribution for a given input distributions 
class CalcDistribution(object):
    def __init__(self):
        # beta distribution A
        self.alphaA = 1.0
        self.betaA = 1.0
        
        # beta distribution B
        self.alphaB = 1.0
        self.betaB = 1.0
        
        self.rawSamples = [] # raw samples as they got sampled
    
    def init(self):
        self.distr = BucketedDistribution()
        self.distr.reset()
    
    def sampleAndStore(self):
		
        phiA = scipy.stats.beta.rvs(self.alphaA, self.betaA, size=1)[0] # sample distribution A
        phiB = scipy.stats.beta.rvs(self.alphaB, self.betaB, size=1)[0] # sample distribution B
        
        # apply rule according to probability theory
        phiRes = phiA*phiB # intersection

        #print(f'phiA={phiA}  phiB={phiB}    phiRes={phiRes}') # DBG
        
        # store sample
        self.distr.putSample(phiRes)
        
        # store into raw array
        self.rawSamples.append(phiRes)
        
	
    def sampleNtimes(self, n):
        for it in range(n):
            self.sampleAndStore()








#########



class FittingTarget(object):
    def init(self):
        #self.alphaA, self.betaA = calcAlphaAndBetaFromMeanAndStdDev(0.7, 0.07)

        #self.alphaB, self.betaB = calcAlphaAndBetaFromMeanAndStdDev(0.5, 0.063)

        pass
        
        self.targetDistr = None # target distribution
    
    # calculate the distribution
    #
    # should be called after changing the beta disstributions
    def calcDistribution(self):
        self.targetDistr = CalcDistribution()
        self.targetDistr.alphaA = self.alphaA
        self.targetDistr.betaA = self.betaA
        self.targetDistr.alphaB = self.alphaB
        self.targetDistr.betaB = self.betaB
        self.targetDistr.init()
        
        nSamples = 12000 # 8000
        
        self.targetDistr.sampleNtimes(nSamples)

    #def calcErrorSum(self, fittingCurrentlyMean, fittingCurrentlyStdDev):
    def calcErrorSum(self, fittingCurrentlyMode, fittingCurrentlyConf):
        

        fittingCurrentlyAlpha, fittingCurrentlyBeta = convStvToAlphaBeta(fittingCurrentlyMode, fittingCurrentlyConf)

        #fittingCurrentlyAlpha, fittingCurrentlyBeta = calcAlphaAndBetaFromMeanAndStdDev(fittingCurrentlyMean, fittingCurrentlyStdDev)

        """ commented because old code which did combine the input distributions directly
        errorSum = 0.0

        # sampling / integration
        x = 0.01
        nSamplesCnt = 0
        while x < 1.0:

            #print(self.alphaA)
            #print(self.betaA)
            #print(self.alphaB)
            #print(self.betaB)

            vA = scipy.stats.beta.pdf(x, self.alphaA, self.betaA)

            vB = scipy.stats.beta.pdf(x, self.alphaB, self.betaB)

            #print(f'distrA.v={vA}') # print sampled value
            #print(f'distrB.v={vB}') # print sampled value


            # compute unnormalized value for conclusion
            vConcl = vA*vB

            ###print('')
            ###print(f'unnormalizedConcl.v = {vConcl}')



            # now we need to use vConcl for fitting

            vTarget = scipy.stats.beta.pdf(x, fittingCurrentlyAlpha, fittingCurrentlyBeta)

            ## old attempt with squared loss
            #errorVal = (vConcl - vTarget)**2
            #errorVal = float(errorVal) # force python type

            # new attempt with loss based on KL-divergence
            errorVal = calcKlDivergenceSingle(vTarget, vConcl)

            errorSum += errorVal

            x += 0.1
            nSamplesCnt += 1
		"""
        
        
        errorSum = 0.0
        nSamplesCnt = 0
        
        approxDistr = self.targetDistr.distr.calcDistribution()
        
        for itBucketIdx in range(len(approxDistr)):
            phi = itBucketIdx / (len(approxDistr) - 1)  + (1.0 / len(approxDistr))*0.5
            
            #print(f'phi={phi}') # DBG
            
            phiTarget = scipy.stats.beta.pdf(phi, fittingCurrentlyAlpha, fittingCurrentlyBeta)
            
            phiApprox = approxDistr[itBucketIdx]
            
            # new attempt with loss based on KL-divergence
            errorVal = calcKlDivergenceSingle(phiTarget, phiApprox)
            
            if errorVal == float("inf") or math.isnan(errorVal):
            #if math.isnan(errorVal):
                continue # we skip it!
            
            #print(phiApprox)
            
            #print(f'errorVal={errorVal}') # DBG
            
            errorSum += errorVal
            nSamplesCnt += 1
        
        if nSamplesCnt == 0:
            return math.inf
        
        errorSum /= nSamplesCnt # divide by number of samples
        
        return errorSum





def calcResult(stvA, stvB, startMode = 0.5):

    fittingTarget = FittingTarget()


    fittingTarget.alphaA, fittingTarget.betaA = convStvToAlphaBeta(stvA[0], stvA[1])
    #print(fittingTarget.alphaA, fittingTarget.betaA)
    fittingTarget.alphaB, fittingTarget.betaB = convStvToAlphaBeta(stvB[0], stvB[1])

    #fittingTarget.alphaA, fittingTarget.betaA = calcAlphaAndBetaFromMeanAndStdDev(0.7, 0.07)
    #print(fittingTarget.alphaA, fittingTarget.betaA)
    #fittingTarget.alphaB, fittingTarget.betaB = calcAlphaAndBetaFromMeanAndStdDev(0.5, 0.063)

    #exit(0)


    fittingTarget.init()
    
    # update the distribution
    fittingTarget.calcDistribution()
    
    
    
    fittingStrategy = "method-of-moments"
    #fittingStrategy = "approximation"
    
    
    if fittingStrategy == "method-of-moments":
        # parameter estimation : method of moments
        #
        # see https://en.wikipedia.org/wiki/Beta_distribution "Method of moments"
        
        # * calculate normalized distribution
        #phis = fittingTarget.targetDistr.distr.calcDistribution() # is this wrong?
        #phis = fittingTarget.targetDistr.distr.buckets # wrong
        samples = fittingTarget.targetDistr.rawSamples
        
        #print(samples)
        
        # * calculate sample mean
        sampleMean = (1.0 / len(samples)) * sum(samples)
        
        # * calculate variance
        accuHelper = 0.0
        for iv in samples:
            diffHelper = iv - sampleMean
            accuHelper += (diffHelper*diffHelper)
        
        sampleVar = (1.0 / (len(samples) - 1)) * accuHelper
        
        
        # HACK HACK HACK
        #sampleMean = 0.5
        #sampleVar = 0.07
        
        
        
        print(f"sampleMean = {sampleMean}") # DBG
        print(f"sampleVar  = {sampleVar}") # DBG
        
        
        condValLeftside = sampleVar
        condValRightside = sampleMean * (1.0 - sampleMean)
        #print(f"{condValLeftside} < {condValRightside}")
        
        # now we put "sampleMean" and "var" into the formula of the method-of-moments estimate
        helperInner = (sampleMean*(1.0 - sampleMean))/sampleVar
        
        alphaRoof = sampleMean * ( helperInner - 1.0 )
        
        betaRoof = (1.0 - sampleMean) * ( helperInner - 1.0 )
        
        
        # HACK
        alphaRoof = max(alphaRoof, 1.0+1e-4)
        
        
        print("")
        print(f"alphaRoof = {alphaRoof}")
        print(f"betaRoof  = {betaRoof}")
        
        
        # ! ! ! ! ! ! STV from these alphaRoof and betaRoof taken as alpha and beta doesn't make any sense!!"!!!!
        #raise "TODO : how to get from alphaRoof and betaRoof to alpha and beta"
        
        
        stv = convAlphaBetaToStv(alphaRoof, betaRoof)
        
        return {
            'stv': stv,
            'errorSum': 0.0
        }
    
    


    # for curve fitting
    #fittingCurrentlyMean = 0.46 # mean of beta-distribution
    #fittingCurrentlyStdDev = 0.049 # standard-deviation of beta-distribution


    # initial values which should capture most result distributions rather well
    fittingBestMode = startMode
    #fittingBestStdDev = 0.112
    fittingBestConf = 0.89

    fittingBestErrorSum = 1.0e9




    rng = random.Random()


    # type of what we are fitting
    # 'mode' : mode
    # 'stdDev' : standard-deviation
    fittingType = 'mode'


    # iterations for approximation
    nIterations =  200 # 100


    for fittingType in ['mode', 'mixed']: # ['mode', 'stdDev', 'mixed']:

        currentIt = 0
        while currentIt < nIterations:

            fittingCurrentlyMode = fittingBestMode
            fittingCurrentlyConf = fittingBestConf
            ###if fittingType == 'mode':
            ###    fittingCurrentlyMode = fittingCurrentlyMode + rng.uniform(-1.0, 1.0)*0.1
            ###else: # else we are fitting stdDev
            ###    fittingCurrentlyStdDev = fittingCurrentlyStdDev + rng.uniform(-1.0, 1.0)*0.01
            
            enFittingMode = False
            enFittingStddev = False
            
            if fittingType == 'mode':
                enFittingMode = True
            elif fittingType == 'stdDev':
                enFittingStddev = True
            elif fittingType == 'mixed':
                enFittingMode = True
                enFittingStddev = True
            
            
            if enFittingMode:
                fittingCurrentlyMode = fittingCurrentlyMode + rng.uniform(-1.0, 1.0) * 0.005 #*0.01
            
            if enFittingStddev:
                fittingCurrentlyConf = fittingCurrentlyConf + rng.uniform(-1.0, 1.0)*0.01


            fittingCurrentlyMode = clamp(fittingCurrentlyMode, 0.0+1e-4, 1.0-1e-4)
            fittingCurrentlyConf = clamp(fittingCurrentlyConf, 0.001, 0.99999)

            errorSum = fittingTarget.calcErrorSum(fittingCurrentlyMode, fittingCurrentlyConf)

            if False:
                print('')
                #print(f'mean={fittingCurrentlyMode} stdDev={fittingCurrentlyStdDev}')
                print(f'errorSum={errorSum}')

            if errorSum < fittingBestErrorSum:
                fittingBestMode = fittingCurrentlyMode
                fittingBestConf = fittingCurrentlyConf

                fittingBestErrorSum = errorSum

            currentIt += 1

    if False:
        print('\n'*3)
        print('best fit:')
        print(f'mode={fittingBestMode} conf={fittingBestConf}')
        print(f'errorSum={fittingBestErrorSum}')
    
    return {
        'stv': (fittingBestMode, fittingBestConf),
        'errorSum': fittingBestErrorSum
    }




# manual test
if False:
    startMode = 0.25
    res0 = calcResult((0.5, 0.9), (0.5, 0.9), startMode)
    print(res0) # result should be 0.25

    exit(0)


# manual test
if False:

    # result should be exactly mode=0.5
    res0 = calcResult((0.4, 0.8), (0.6, 0.8))
    print(res0)

    # result should be exactly mode=0.5
    res0 = calcResult((0.1, 0.7), (0.9, 0.7))
    print(res0)



    # result should be exactly mode=0.9
    res0 = calcResult((0.9, 0.7), (0.9, 0.7))
    print(res0)

    # result should be exactly mode=0.1
    res0 = calcResult((0.1, 0.7), (0.1, 0.7))
    print(res0)


    raise Exception('FIN')






stvA = (0.7, 0.8)
stvB = (0.5, 0.8)

#arrConfOptions = [0.01, 0.5, 0.9, 0.99]
arrConfOptions = [0.01, 0.5, 0.99] # <---


#arrFreqOptions = [0.0001, 0.1, 0.25, 0.5, 0.75, 0.9, 0.999]
arrFreqOptions = [0.1, 0.9]
arrFreqOptions = [0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99] # <--



# for TESTING
#arrConfOptions = [0.01, 0.99]
#arrFreqOptions = [0.1, 0.5, 0.9]


strTrainingDat = ''


strTrainingDat += 'def run0(dat):\n'


for confA in arrConfOptions:
    for strengthA in arrFreqOptions:
        for confB in arrConfOptions:
            for strengthB in arrFreqOptions:
                
                stvA = (strengthA, confA)
                stvB = (strengthB, confB)
                
                # start mode 0.5 is "unbiased"
                startMode = 0.5
                
                # set the startMode to A*B to bias the search toward the correct result
                #
                # this depends on the approximated truth-function!!!
                startMode = strengthA*strengthB

                res0 = calcResult(stvA, stvB, startMode)
                stvResultStrength, stvResultConf = res0['stv']

                strThisDatapoint = f'    dat.append(([{stvA[0]}, {1.0-stvA[0]}, {stvA[1]}, {stvB[0]}, {1.0-stvB[0]}, {stvB[1]}], [{stvResultStrength}, {stvResultConf}]))'

                # print training data
                print(strThisDatapoint)

                strTrainingDat += strThisDatapoint + '\n'

f = open('trainingdat0.py', 'w')
f.write(strTrainingDat)
f.close()


