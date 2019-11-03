from __future__ import print_function
import torch
import numpy as np
"""
Tensor 张量
"""
x = torch.empty(size=(5, 3))

print(x)

x = torch.rand(5, 3)

print(x)
print(x.size())

"""
操作
"""
y = torch.rand(size=(5, 3))

print(x + y)
print(torch.add(x, y))
result = torch.empty(size=(5, 3))
torch.add(x, y, out=result)
print(result)
y.add_(x)
print(y)
print(x[:, 1])
x = torch.randn(4, 4)
y = x.view(16)
z = x.view(-1, 8)
print(x.size(), y.size(), z.size())
"""
Numpy Bridge
"""
a = torch.ones(5)
print(a)
b = a.numpy()
print(b)
a.add_(1)
print(a)
print(b)

a = np.ones(5)
b = torch.from_numpy(a)
np.add(a, 1, out=a)
print(a)
print(b)
# 除了 CharTensor 之外，CPU 上所有 Tensor 都支持与 Numpy 互相转换

"""
CUDA Tensors 
在确定 cuda 可用的情况下，可以使用 .cuda 方法将 Tensors 在 GPU 上运行
"""
if torch.cuda.is_available():
    x = x.cuda()
    y = y.cuda()
    print(x + y)
