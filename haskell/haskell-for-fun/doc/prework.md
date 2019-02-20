# 前置工作

---

**如果你是怀着和笔者一样好奇才入坑的,一起奋斗吧**

人的好奇心真是太可怕了,学习python的时候,感受到了`reduce`和`filter`以及其他神器的妙用

然后...

就顺便查了一下函数式语言,然后...

就入了haskell的大坑,顿时感觉智商弱爆了

笔者使用的教材是haskell趣学指南

---

## 0.0 haskell很有趣,这就够了

此段文字直接摘录自haskell趣学指南

>本书主要面向已经有一些命令式编程经验(如c c++ java python等),想要尝试一下haskell的读者.还没有编程基础?没关系,像你这样聪明的小伙子一定能学好haskell!<br /><br />haskell这门语言给我的第一印象是太晦涩.然而一旦迈过入门的门槛,随后的学习就顺畅多了.在一开始的学习中,haskell可能稍显古怪,但是不要放弃学习.学习haskell的体验就像重头开始学习编程,它很有趣,而且能促使你按照不同的方式思考.<br /><br />如果在学习中遇到困难,freenode 网络的#haskell频道将是提问的绝佳去处.那儿人们友善,耐心而且照顾新人.对haskell初学者而言,这是一个宝贵的资源

---

## 0.1 那么, haskell是什么?

**定义**

haskell是一种纯函数式语言(purely functional programming language)

**特征**

- 函数式语言通过函数来描述问题"是什么",~~而不像命令式语言执行操作需要给电脑安排一组待执行的命令~~
- 函数式语言中,变量一旦赋值,就不能更改了
- 在纯函数语言中,函数没有任何副作用
  - 函数式编程语言中函数能做的唯一一件事就是求值并返回结果
  - 若以同样的参数调用一个函数两次,得到的结果总是相同的--引用透明性
- haskell是*惰性(lazy)的
  - 若非特殊指明,在真正需要结果之前,haskell是不会真正执行函数的
  - 只有当你需要计算结果时,haskell才会执行最少量的计算,到足够展示结果为止
  - 例如:`doubleMe(doubleMe(doubleMe(xs)))`  ~~在命令式语言中,一般会遍历一遍容器xs,返回一次操作后的新的容器,然后重复以上操作三次~~,在haskell中,不强迫输出结果时,程序只会和你说"我待会做",然后放到一边.在你需要时,进行一次遍历
- haskell是静态类型的,当编译程序时,需要明确变量的类型:意味着错误更容易在编译时发现
- haskell有强大的类型系统,支持类型推导
- haskell优雅而间接,采用了许多高级概念

---

## 0.2 准备工具

- 你仅仅需要一个编译器和一个编辑器
- 如今最常用的haskell编译器是ghc
- 最简单的方式是下载haskell platform (本文提供了两种平台的安装方式) <br />
[haskell platform](https://hackage.haskell.org/platform)
- haskell像python一样,支持脚本运行和交互式运行,交互器为ghci
  - 安装完毕后,在linux shell或者windows cmd输入ghci即可进入交互式模式
  - 假设你在脚本文件function.hs中定义几个函数,切换工作目录到那里,在ghci中输入`:l function`即可将这些函数转载进入ghci
  - 一旦修改这些内容,可以使用以上指令再次装载


### 0.2.1 windows下安装步骤

1. 在[haskell platform](https://hackage.haskell.org/platform)下载对应的完整安装包(X86\X64,鉴于国内网络稳定性,请选择完整安装包)
2. 运行安装程序并按照说明进行操作,记住您的安装目录
3. 在cmd运行`cabal user-config init`以生成cabal(haskell的包管理器,类似于python的pip)的配置文件,此时请记住生成文件的目录
4. 修改cabal配置文件:<br />
   - 使用记事本,notepad++,vscode等工具打开配置文件
   - 以下是在默认路径下的修改,若您更改了haskell platform的安装目录,请将以下的`C：\ Program Files \ Haskell Platform \ 8.0.2 \`改为`安装目录\]+
    版本\`,如果您在安装时只有安装目录,未在路径中添加版本,则为`安装目录`

>extra-prog-path：C：\ Program Files \ Haskell Platform \ 8.0.2 \ msys \ usr \ bin
extra-lib-dirs：C：\ Program Files \ Haskell Platform \ 8.0.2 \ mingw \ lib
extra-include-dirs：C：\ Program Files \ Haskell Platform \ 8.0.2 \ mingw \ include

5. 您可以从开始菜单启动WinGHCi,或者在cmd控制台键入ghci以启动ghci


### 0.2.2 linux下安装

**通用方法tarball安装** 

1. 在[haskell platform](https://hackage.haskell.org/platform)找到通用方法,下载对应tarball文件
2. 通过running安装
```
$ tar xf ...正在下载档案...
$ sudo ./install-haskell-platform.sh
```
3. 如果你有一个系统默认情况下与位置无关的可执行文件（如Ubuntu 16.10及以上），你应该在以下位置编辑GHC设置文件`usr/local/haskell/ghc-___/lib/ghc-___/settings`并将“编译器支持-no-pie”标志从“NO”更改为“YES”。

**包管理工具安装**

0. (可选)在[haskell platform](https://hackage.haskell.org/platform)查询你平台的对应方案
1. 如果你了解的平台的包管理方式,使用对应的软件安装会非常便捷<br />
如笔者的centos7,可以一键完成安装和配置
```
yum install -y ghc
```

**从头编译安装**

什么?你对自己的技术有信心,不想采用上面那些简单的方式?朋友,我们的目的是haskell,而不是linux,好吗?

相信对于喜爱从头编译安装的你来说,自己安装并不是什么问题,这里就不写了

