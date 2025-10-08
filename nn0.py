
import torch


class M0(torch.nn.Module):
    def __init__(self, dimInput, dimHidden, dimOutput):
        super(M0, self).__init__()

        dimHidden2 = 30
        dimHidden2 = 14
        
        
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
        t0 = torch.nn.functional.leaky_relu(t0, 0.001)
        #t0 = torch.nn.functional.softplus(t0)


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




dimHidden = 25

dimHidden = 60


model = M0(6, dimHidden, 2)

model.load_state_dict(torch.load('tvfnModelA.torchmodel', weights_only=True))

def nnA(v0, v1, v2, v3, v4, v5):
    t0 = [v0, v1, v2, v3, v4, v5]
    t0 = torch.tensor(t0)

    t1 = model.forward(t0)

    t1 = t1.detach().cpu().numpy().tolist()

    return t1


'''
def nnA(v0, v1, v2, v3, v4, v5):
    return [v0, v1, v2, v3, v4, v5]
'''
