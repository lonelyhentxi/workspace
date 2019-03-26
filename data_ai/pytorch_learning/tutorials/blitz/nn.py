import torch
import torch.nn as nn
import torch.nn.functional as F

"""
# 神经网络
torch.nn 是专门为神经网络设计的模块化接口，它构建于 Autograd 之上，可以用来定义和运行神经网络
nn.Module 是其中最重要的类，可以看成一个网络的封装，包含网络各层的定义和 forward 方法
调用 forward(input) 方法，可以返回前向传播的结果

一个典型的神经网络训练过程如下：
- 定义具有一些可学习参数（或权重）的神经网络
- 迭代输入数据集
- 通过网络处理输入
- 计算损失
- 将梯度传播回网络
- 更新网络的权重，通常使用简单的更新规则： weight = weight - learning_rate * gradient
"""


class Net(nn.Module):
    def __init__(self):
        super(Net, self).__init__()
        # 卷积层 1 表示输入图片为单通道，6 表示输出通道数量，5 表示卷积核为 5*5 核心
        self.conv1 = nn.Conv2d(1, 6, 5)
        self.conv2 = nn.Conv2d(6, 16, 5)
        # 仿射层/全连接层： y = Wx + b
        self.fc1 = nn.Linear(16 * 5 * 5, 120)
        self.fc2 = nn.Linear(120, 84)
        self.fc3 = nn.Linear(84, 10)

    def forward(self, x):
        # 在由多个输入平面组成的输入信号上应用 2D 最大池化
        # (2,2) 代表池化的步幅
        x = F.max_pool2d(F.relu(self.conv1(x)), (2, 2))
        return x
        x = x.view(-1, self.num_flat_features(x))
        x = F.relu(self.fc1(x))
        x = F.relu(self.fc2(x))
        x = self.fc3(x)

    def num_flat_features(self, x):
        size =