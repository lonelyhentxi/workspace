# 相信类型

---

强大的类型是haskell的秘密武器。在haskell中，每一个表达式都会在编译时得到明确的类型，提高代码安全性。haskell中一切都有类型，它支持类型推导（type inference）。下面这些关于haskell类型的信息都是至关重要的。

---

## 2.1 显式类型声明

**GHCi操作**

`:t`命令,后面跟上任何合法表达式,就可以检查该表达式的类型

```haskell
ghci> :t 'a'
'a' :: Char
ghci> :t True
True :: Bool
ghci> :t "Hello!"
"Hello!" :: [Char]
ghci> :t (True,'a')
(True,'a') :: (Bool,Char)
```

- `::`读作它的类型为.凡是明确的类型,其首字母必须大写.
- 函数也有类型.编写函数时,给他一个显式的类型声明是一个好习惯
- 比较短的函数不必多此一举

```haskell
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c|c<-st,c 'elem' ['A'..'Z']]
```

- 一般函数声明就如上所示
- 对于多个参数,使用`paramtype1->paramtype2..->returntype`的形式即可
- 如果不确定函数的类型,对函数使用`:t`进行类型推导即可

---

## 2.2 haskell的常见类型

<table>
<tr>
<th>类型</th>
<th>说明</th>
</tr>
<tr>
<td>Int/整数</td>
<td>基本类似于自然界整数,只是有界</td>
</tr>
<tr>
<td>Interger/整数</td>
<td>无界整数</td>
</tr>
<tr>
<td>Float/单精度浮点数</td>
<td>-</td>
</tr>
<tr>
<td>Double/双精度浮点数</td>
<td>-</td>
</tr>
<tr>
<td>Bool/布尔值</td>
<td>-</td>
</tr>
<tr>
<td>Char/字符</td>
<td>属于Unicode字符集</td>
</tr>
<tr>
<td>元组</td>
<td>取决于数目与类型,空元组类型为( )</td>
</tr>
</table>

元组中最大项数为62

---

## 2.3 类型变量

```haskell
ghci> : head
ghci :: [a] -> a
```

- 凡是类型,其首字母必须大写,我们注意到,这个`a`应该是个类型变量(type variable),意味着它可能是任意类型
-  通过了类型变量,可以在类型安全(type safe)的前提下,轻而易举的写出多种类型的函数,与其他语言的泛型类似
- 使用变量类型的函数称为*多态函数(ploymorphic function)
- 在类型变量上,命名时使用多个字符是合法的,不过约定俗成使用单个字符

```haskell
ghci> :t fst
fst :: (a,b)->a
```

不同的类型变量a,b,并不特指二者类型不同

---

## 2.4 类类型入门

- *类类型(typeclass)*是定义行为的接口.如果一个类是某类型类的实例(instance),那他必定体现了该类型所描述的行为
- 类型类是一组函数的集合,如果将某类型实现为某类型类的实例,那就需要为这类型提供这些函数的相应实现

```haskell
ghci> :t (==)
(==) :: (Eq a)=>a->a->Bool
--意思为:相等性函数去两个相同类型的值作为参数并返回一个布尔值
--而这两个参数的类型同为Eq的实例
```

- 和判断相等性的==类似,haskell中的很多运算符都是函数
- 如果一个函数的名字皆为特殊字符,则默认为中缀函数
- 检查中缀函数的类型,触底传递给其他函数调用或作为前缀函数使用,就必须用括号括起来
- `=>`符号,它的左侧叫做类型约束
- 千万不能把haskell中的类型类与面向对象语言中的类(class)混淆

### 2.4.1 Eq类型类

- Eq类型类用于可判断相等性的类型
- 如果某个类型变量声明了属于Eq的类型约束,那么他就必然定义了`==`和`/=`

```haskell
ghci> 5 == 5
True
```

### Ord类型类

- Ord类型类用于可比较大小的类型

```haskell
ghci> it (>)
(>) ::  (Ord a)=> a-> a->Bool
```

- Ord类型有GT,LT,EQ三种值,分别表示大于,小于和等于

### 2.4.3 Show类型类

- Show类型类的实例为可以表示为字符串的类型
- 目前为止我们提到的除了函数以外的类型都是Show的实例

```haskell
ghci> show 3
"3"
```

show函数可以将任何一Show类型类的实例转换为字符串

### 2.4.4 Read类型类

Read类型类可以看做与Show相反的类型类.同样我们提到的所有类型都是他的实例.

```haskell
ghci> read "8.2" + 3.8
12.0
```

read函数可以取一个字符串并将它作为参数转为Read类型的某个实例

```haskell
ghci> read "4"
```
- GHCi这时候会报错,之前我们得到结果,都进行了进一步运算,并间接推算了类型
- 而此时,无法得到结果,因为他无法推断是哪个类型

```haskell
ghci> :t read
read :: (Read a)=>String->a
```

- String是[Char]的别名,只是更容易书写
- read返回值是Read类型的实例,若我们用不到这个值,就永远不会知道返回值的类型,要解决这一问题,可以使用*类型注解(type annotation)
- 类型注解跟在表达式后,通过`::`分隔,用来显式的告知haskell某表达式的类型

```haskell
ghci> read "5" :: Int
5
```

通常,编译器可以认出大部分类型,但是在必要的时候我们需要提供最少的信息

```haskell
-- 举个例子
ghci> [read "True",False,False,True]
```
编译器甚至可以实现这样的推断

### 2.4.5 Enum类型类

**特点**

- Enum类型类的实例都是有连续顺序的,他们的值都是可以枚举的
- 可以在区间中使用这些类型
- 这些类型每一个值都有相应的*后继(successer)*和*前趋(predcesser)*分别可以通过`succ`和`pred`函数得到.

**成员**

- 该类型类主要包含
  - Bool
  - ()
  - Char
  - Ordering
  - Int
  - Interger
  - Float
  - Double

### 2.4.6 Bounded类型类

Bounded类型类的实例都有一个上限和下限,通过`maxBound`和`minBound`得到,且这两个函数类型都为`(Bounded a)=>a`,他们是多态常量(ploymorphic constant)

这种类型类有有趣的属性,如果是嵌套的类型,且每一层都有都属于,则总体也将属于

### 2.4.7 Num类型类

Num是一个表示数值的类型类,它的实例类型都具有数的特征

只有已经属于Show和Eq的类型实例,才能成为Num类型类的实例

### 2.4.7 Floating类

Floating类型仅包含Float和Double两种浮点类型,用于储存浮点数

一般这种类型用来进行某种浮点运算,比如sin,cos,sqrt

### 2.4.9 Interger类型类

包含Int和Interger

```haskell
-- 这个函数在处理字数时会非常有用
fromIntergeral :: (Interger a,Num b)=>a->b
--fromIntergeral的类型签名中用到了多个类型约束,这是合法的,只要放在括号里用逗号隔开即可
length :: [a]->Int
ghci> fromIntergeral(length [1,2,3,4]) + 3.2
7.2
-- 转换了生成的数的类型,避免报错
```

### 2.4.10

- 类型类定义的是抽象的接口
- 一个类型可以作为多个类型类的实例,一个类型类可以有多个类型作为实例
- 有时,只有属于一个类型类之后,才能在属于另一个类型类--这称为*先决条件(prerequisite)