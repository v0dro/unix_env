# What is the regression in the scatter_add performance?
import os
import sys
import torch
import time
import numpy
from IPython import get_ipython

Ms=256
Ns=512
dim = 0
top_power = 2
ipython = get_ipython()

plot_name = os.path.basename(__file__)
branch = sys.argv[1]
fname = open(plot_name + ".csv", "a+")

for pM in range(top_power):
    M = Ms * (2 ** pM)
    for pN in range(top_power):
        N = Ns * (2 ** pN)
        input_one = torch.rand(M, N)
        index = torch.tensor(numpy.random.randint(0, M, (M, N)))
        res = torch.randn(M, N)

        test_case = f"{M}x{N}"

        tobj = ipython.magic("timeit -o res.scatter_add_(dim, index, input_one)")

        fname.write(f"{test_case},{branch},{tobj.average},{tobj.stdev}\n")

# foo=ipython.magic("timeit -o res.scatter_add_(1, idx2, src2)")

fname.close()
