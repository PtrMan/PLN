


# pip install scipy


import scipy.stats

import random

def clamp(arg, minVal, maxVal):
    return max(min(arg, maxVal), minVal)


# compute alpha and beta of beta distribution given mean and standard-deviation
def calcAlphaAndBetaFromMeanAndStdDev(mean, stdDev):
    n = ( mean * (1.0 - mean) ) / (stdDev*stdDev)
    alpha = mean * n
    beta = (1.0 - mean) * n
    return alpha, beta

def convCtoW(conf):
    return conf / (1.0 - conf)

def convWtoC(w):
    return w / (w + 1.0)

# convert STV to alpha and beta
def convStvToAlphaBeta(strength, conf):
    w = convCtoW(conf)
    
    alpha = strength * (w - 2.0) + 1.0
    beta = w - alpha

    return (alpha, beta)

def convAlphaBetaToStv(alpha, beta):
    #strength = alpha / (alpha + beta)
    #conf = (alpha + beta) / ((alpha + beta) + 1.0)

    w = alpha + beta
    
    # calc STV.s
    strength = (alpha - 1.0) / (w - 2.0) # STV.s = mode

    # calc STV.c
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


class FittingTarget(object):
    def init(self):
        #self.alphaA, self.betaA = calcAlphaAndBetaFromMeanAndStdDev(0.7, 0.07)

        #self.alphaB, self.betaB = calcAlphaAndBetaFromMeanAndStdDev(0.5, 0.063)

        pass

    def calcErrorSum(self, fittingCurrentlyMean, fittingCurrentlyStdDev):

        fittingCurrentlyAlpha, fittingCurrentlyBeta = calcAlphaAndBetaFromMeanAndStdDev(fittingCurrentlyMean, fittingCurrentlyStdDev)


        errorSum = 0.0

        # sampling / integration
        x = 0.01
        nSamplesCnt = 0
        while x < 1.0:

            vA = scipy.stats.beta.pdf(x, self.alphaA, self.betaA)

            vB = scipy.stats.beta.pdf(x, self.alphaB, self.betaB)

            ###print(f'distrA.v={vA}') # print sampled value
            ###print(f'distrB.v={vB}') # print sampled value

            # compute unnormalized value for conclusion
            vConcl = vA*vB

            ###print('')
            ###print(f'unnormalizedConcl.v = {vConcl}')



            # now we need to use vConcl for fitting

            vTarget = scipy.stats.beta.pdf(x, fittingCurrentlyAlpha, fittingCurrentlyBeta)

            errorSquared = (vConcl - vTarget)**2
            errorSum += errorSquared

            x += 0.1
            nSamplesCnt += 1

        errorSum /= nSamplesCnt # divide by number of samples

        return errorSum







stvA = (0.7, 0.8)
stvB = (0.5, 0.8)

#arrConfOptions = [0.01, 0.5, 0.9, 0.99]
arrConfOptions = [0.01, 0.5, 0.99]

#arrFreqOptions = [0.0001, 0.1, 0.25, 0.5, 0.75, 0.9, 0.999]
arrFreqOptions = [0.1, 0.9]
arrFreqOptions = [0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99]


print('def run0(dat):')


for confA in arrConfOptions:
    for freqA in arrFreqOptions:
        for confB in arrConfOptions:
            for freqB in arrFreqOptions:
                stvA = (freqA, confA)
                stvB = (freqB, confB)



                fittingTarget = FittingTarget()


                fittingTarget.alphaA, fittingTarget.betaA = convStvToAlphaBeta(stvA[0], stvA[1])
                #print(fittingTarget.alphaA, fittingTarget.betaA)
                fittingTarget.alphaB, fittingTarget.betaB = convStvToAlphaBeta(stvB[0], stvB[1])

                #fittingTarget.alphaA, fittingTarget.betaA = calcAlphaAndBetaFromMeanAndStdDev(0.7, 0.07)
                #print(fittingTarget.alphaA, fittingTarget.betaA)
                #fittingTarget.alphaB, fittingTarget.betaB = calcAlphaAndBetaFromMeanAndStdDev(0.5, 0.063)

                #exit(0)

                fittingTarget.init()


                # for curve fitting
                #fittingCurrentlyMean = 0.46 # mean of beta-distribution
                #fittingCurrentlyStdDev = 0.049 # standard-deviation of beta-distribution


                # initial values which should capture most result distributions rather well
                fittingBestMean = 0.5
                fittingBestStdDev = 0.112

                fittingBestErrorSum = 1.0e9




                rng = random.Random()


                # type of what we are fitting
                # 'mean' : mean
                # 'stdDev' : standard-deviation
                fittingType = 'mean'


                # iterations for approximation
                nIterations = 100


                for fittingType in ['mean', 'stdDev']:

                    currentIt = 0
                    while currentIt < nIterations:

                        fittingCurrentlyMean = fittingBestMean
                        fittingCurrentlyStdDev = fittingBestStdDev
                        if fittingType == 'mean':
                            fittingCurrentlyMean = fittingCurrentlyMean + rng.uniform(-1.0, 1.0)*0.1
                        else: # else we are fitting stdDev
                            fittingCurrentlyStdDev = fittingCurrentlyStdDev + rng.uniform(-1.0, 1.0)*0.01
                        
                        # mixed optimization
                        fittingCurrentlyMean = fittingCurrentlyMean + rng.uniform(-1.0, 1.0)*0.1
                        fittingCurrentlyStdDev = fittingCurrentlyStdDev + rng.uniform(-1.0, 1.0)*0.01


                        fittingCurrentlyMean = clamp(fittingCurrentlyMean, 0.0+1e-4, 1.0-1e-4)
                        fittingCurrentlyStdDev = clamp(fittingCurrentlyStdDev, 0.02, 0.5)

                        errorSum = fittingTarget.calcErrorSum(fittingCurrentlyMean, fittingCurrentlyStdDev)

                        if False:
                            print('')
                            print(f'mean={fittingCurrentlyMean} stdDev={fittingCurrentlyStdDev}')
                            print(f'errorSum={errorSum}')

                        if errorSum < fittingBestErrorSum:
                            fittingBestMean = fittingCurrentlyMean
                            fittingBestStdDev = fittingCurrentlyStdDev

                            fittingBestErrorSum = errorSum

                        currentIt += 1

                if False:
                    print('\n'*3)
                    print('best fit:')
                    print(f'mean={fittingBestMean} stdDev={fittingBestStdDev}')
                    print(f'errorSum={fittingBestErrorSum}')

                fittingCurrentlyAlpha, fittingCurrentlyBeta = calcAlphaAndBetaFromMeanAndStdDev(fittingCurrentlyMean, fittingCurrentlyStdDev)

                stvResultStrength, stvResultConf = convAlphaBetaToStv(fittingCurrentlyAlpha, fittingCurrentlyBeta)

                # print training data
                print(f'    dat.append(([{stvA[0]}, {1.0-stvA[0]}, {stvA[1]}, {stvB[0]}, {1.0-stvB[0]}, {stvB[1]}], [{stvResultStrength}, {stvResultConf}]))')





