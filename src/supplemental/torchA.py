import torch
import random

class M0(torch.nn.Module):
    def __init__(self, dimInput, dimHidden, dimOutput):
        super(M0, self).__init__()

        dimHidden2 = 30

        self.m0 = torch.nn.Parameter(torch.randn(dimInput, dimHidden) *  0.09)
        self.bias0 = torch.nn.Parameter(torch.randn(dimHidden) * 0.08)

        self.m1 = torch.nn.Parameter(torch.randn(dimHidden, dimHidden2) *  0.09)
        self.bias1 = torch.nn.Parameter(torch.randn(dimHidden2) * 0.08)


        self.mOut = torch.nn.Parameter(torch.randn(dimHidden2, dimOutput) * 0.09)
        self.biasOut = torch.nn.Parameter(torch.randn(dimOutput) * 0.08)
        
    def forward(self, x):
        t0 = x

        t0 = t0 @ self.m0 + self.bias0
        #t0 = torch.nn.functional.leaky_relu(t0, 0.001)
        t0 = torch.nn.functional.softplus(t0)


        t0 = t0 @ self.m1 + self.bias1
        t0 = torch.nn.functional.elu(t0)

        
        t0 = t0 @ self.mOut + self.biasOut

        return t0





if torch.cuda.is_available(): 
    dev = 'cuda:0' 
else:
    dev = 'cpu'
dev = 'cpu' # force CPU
device = torch.device(dev)




outerIterations = int(120000 * 5.5)

#outerIterations = 5


dimHidden = 8 # 4 # 8

# gives error down to 0.068
dimHidden = 34 # 4 # 8


# ?
#dimHidden = 60




model = M0(6, dimHidden, 2)

#t0 = torch.randn(1, 4)
#
#print(t0)
#
#t1 = model.forward(t0)
#
#print(t1)


class DatGenA(object):
    def __init__(self):
        self.samples = []
    
    def sample(self):
        idxSelSample = random.randint(0, len(self.samples)-1)
        return self.samples[idxSelSample]


datGen = DatGenA()

from trainingdat0 import *
dat = []
run0(datGen.samples)

'''
datGen.samples.append(([0.5, 0.7, 0.7, 0.76], [0.7, 0.8]))
datGen.samples.append(([0.8, 0.7, 0.2, 0.3], [0.753, 0.2344]))
datGen.samples.append(([0.34, 0.01, 0.7, 0.8], [0.683782, 0.66]))
datGen.samples.append(([0.8, 0.872, 0.8236, 0.362563], [0.27244, 0.49827]))
'''


# Construct our loss function and an Optimizer. The call to model.parameters()
# in the SGD constructor will contain the learnable parameters (defined
# with torch.nn.Parameter) which are members of the model.
criterion = torch.nn.MSELoss()
#optimizer = torch.optim.SGD(model.parameters(), lr=1e-3)
optimizer = torch.optim.AdamW(model.parameters(), lr=0.001)

lossAvg = None




for itOuter in range(outerIterations):
    #if (itOuter % 5000) == 0:
    #    print(f'store model')
    #    torch.save(modelA.state_dict(), './model-checkpoint.pytorchModel')
    
    x, y = datGen.sample()



    #x = [0.5, 0.7, 0.7, 0.8]
    #y = [0.7, 0.8]
    x = torch.tensor(x)
    x = x.to(device)
    y = torch.tensor(y)
    y = y.to(device)

    # Forward pass: Compute predicted y by passing x to the model
    yPred = model.forward(x)
    
    

    # Compute and print loss
    printLossEvernN = 7000 # 1800
    
    loss = criterion(yPred, y)
    
    if lossAvg is None:
        lossAvg = loss.item()
    
    a = 0.9993
    lossAvg = a*lossAvg + (1.0-a)*loss.item()
    
    if (itOuter % printLossEvernN) == (printLossEvernN-1):
        print(f'it={itOuter} {loss.item()} lossAvg={lossAvg}')

    # Zero gradients, perform a backward pass, and update the weights.
    optimizer.zero_grad()
    loss.backward()
    optimizer.step()



# export ML model
torch.save(model.state_dict(), 'tvfnModelA.torchmodel')



import numpy

def arrFlatten(arg):
    return numpy.concatenate(arg).tolist()

def convPythonArrToMettaCode(arg):
    t0 = list(map(str, arg))
    t0 = '  '.join(t0)
    return f'({t0})'


if True:
    arr = model.m0.detach().cpu().numpy()
    arr2 = arrFlatten(arr)
    str2 = convPythonArrToMettaCode(arr2)
    print(str2)

    arr = model.bias0.detach().cpu().numpy()
    arr2 = arr
    str2 = convPythonArrToMettaCode(arr2)
    print(str2)



    arr = model.m1.detach().cpu().numpy()
    arr2 = arrFlatten(arr)
    str2 = convPythonArrToMettaCode(arr2)
    print(str2)

    arr = model.bias1.detach().cpu().numpy()
    arr2 = arr
    str2 = convPythonArrToMettaCode(arr2)
    print(str2)



    arr = model.mOut.detach().cpu().numpy()
    arr2 = arrFlatten(arr)
    str2 = convPythonArrToMettaCode(arr2)
    print(str2)

    arr = model.biasOut.detach().cpu().numpy()
    arr2 = arr
    str2 = convPythonArrToMettaCode(arr2)
    print(str2)
