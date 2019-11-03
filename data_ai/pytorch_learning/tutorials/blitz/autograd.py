import torch

"""
# 自动求导：自动微分
pytorch 神经网络的核心是 autograd 求导包，它对张量上所有操作都提供了自动微分操作
"""

"""
# 变量
autograd 包的核心类是 autograd.Variable （4.x 之后和 Tensor 进行了合并）
一旦完成了计算，就可以调用 .backward 方法，然后梯度会自动进行计算
还可以通过 .data 属性访问原始的张量，关于该变量梯度会被累计到 .grad 上

还有一个对于自动求导实现非常重要的类 Function
Variable 和 Function 是相互联系的，并且它们构建了一个非循环的图，编码了一个完整的计算历史信息
每一个 variable 都有一个 .grad_fn 属性，它引用了一个已经创建了 Variable 的 Function

在 Variable 上调用 .backward，如果 Variable 是标量，不必传递任何参数，如果是更多，则需要制定 grad_output 参数
该参数是一个匹配形状的张量
"""

x = torch.tensor(torch.ones(2, 2), requires_grad=True)
print(x)
y = x + 2
print(y)
print(y.requires_grad)
# pycharm inspection error
print(y.grad_fn)

z = y * y * 3
out = z.mean()
print(z, out)
"""
# 梯度
"""
out.backward()
print(x.grad)

x = torch.randn(3, requires_grad=True)
y = x * 2
while y.norm() < 1000:
    y = y * 2
print(y)
gradients = torch.tensor(data=[0.1, 1.0, 0.0001], dtype=torch.float)
y.backward(gradients)
print(x.grad)