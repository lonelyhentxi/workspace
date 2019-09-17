# 函数的语法

---

本章讲解的是haskell中的语法,借助他们可以使得你的函数更加可读,更富有表现力.

---

## 3.1 模式匹配

模式匹配(pattern matching)通过检查特定的结构来检查是否匹配,并按照模式从中解析出数据

类似于c++等语言中函数和运算符的重载

```haskell
lucky :: Int -> String
lucky 7 = "Lucky number seven!"
lucky x = "You are out of luck, sorry!"
```

*万能模式(catchall pattern)*

总能够匹配输入的参数,将其绑定到模式中的名字供我们使用

*递归函数*

如果一个函数的定义中调用了自身,那么,他将被称为递归函数

```haskell
factorial :: Int -> Stirng
factorial 0 =1
factorial n = n*factorial (n-1)
```

*防止崩溃*

尽量在定义模式时,设置一个万能模式,以防止程序崩溃

### 3.1.1 元组的模式匹配

```haskell
addVector :: (Double,Double)->(Double,Double)->(Double,Double)
addVector a b =(fst a + fst b,snd a + snd b)

-- 更好的表达

addVector :: (Double,Double)->(Double,Double)->(Double,Double)
addVector (x1,y1) (x2,y2)=(x1+x2,y1+y2)
```
既然没有内建的对三元组取项的操作,我们也可以自己写

```haskell
first :: (a,b,c)->a
first (x,_,_)->x

-- 以下省略
```
这里就和列表推导式的`_`一样,只用一个泛变量(generic variable)占位即可

### 3.1.2 列表与列表推导式的模式匹配

**列表推导式模式匹配**

在列表推导式中也可以使用模式匹配,而且一旦模式匹配失败,就简单的挪到下一个元素,而不会被包含在列表内

```haskell
ghci> let xs = [(1,3),(4,3),(2,4),(5,3),(5,6),(3,1)]
ghci> [a+b|(a,b)<-xs]
[4,7,6,8,11,4]
```

**列表模式匹配**

对于普通列表也可以使用模式匹配
- 使用`[]`来匹配空列表
- 使用`:[]`来匹配非空列表(列表本质是`::...[]`的语法糖)
- `x:xs`模式在haskell中运用非常广泛,尤其对于递归函数,但只能匹配长度大于等于一的列表

ps:`error`函数,会生成一个运行时错误,用参数中的字符串表示错误的描述,它会直接导致程序崩溃

- `x:[]`,`x:y:[]`形式等同于`[x]`,`[x,y]`形式的匹配
- `(x:y:_)`这样的模式就不能用方括号表示了
- 不能在模式中使用`++`,加入试图在模式匹配中使用,编译器将无法分辨前者和后者

### 3.1.3 As模式

*as模式(as-pattern)*允许我们按模式把一个值分割成多个项,同时还保留对整体的引用

```haskell
-- 示例如下
first :: String -> String
first "" = "Empty String, Whopps!"
first all@(x:xs)="The first letter of " ++ all ++ " is " ++ [x]
```

---

## 3.2 注意, 哨卫

**哨卫(guard)**

哨卫用于检查参数的性质是否为真,类似if,但在处理多个条件分支时,哨卫的可读性更高,且与模式匹配更加契合

```haskell
-- 示例
bmiTell :: Double ->String
bmiTell bmi
    | bmi <18.5 = promptString1
    | otherwise = promptString2
```

- 在第一个哨卫之前,函数名和参数之间并没有`=`
- 通过单引号,不仅可以中缀的形式调用前缀函数,甚至可以以中缀的形式定义函数

---

## 3.3 where

非参数的局部关键字,可以使用`where`关键字定义,它们跟在稍微后面,对于哨卫的每个分支都可以使用
```
bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= skinny = prompt1
    | otherwise = prompt2
    where bmi =weight/height^2
          skinny = 18.5
```

**局部变量**

- 局部变量定义都必须对齐于同一列,否则haskell难以区分属于哪个代码块
- where中定义的名字只对本函数及本模式可用
- 想要在多个函数和模式中使用,需要全局变量

**where里的模式匹配**

```haskell
--第一个例子
...
where bmi = weight / height ^ 2
      (skinny,normal,fat)=(18.5,25.0,30.0)

--第二个例子
initials :: String -> String ->String
initials firstname lastname = [f] ++ '.' ++ [l] ++ 'l'
    where (f:_) = firstname
          (l:_) = lastname
```

**where块中的函数**

```haskell
calcBims :: [(Double,Double)]->[Double]
calcBims xs = [bmi w h | (w,h)<-xs]
    where  bmi weight height = weight / height^2
```

---

## 3.4 let

let表达式和where绑定很是类似
- where允许我们在函数底部绑定变量,对包括哨卫在内的整个函数可见
- let是一个表达式,允许我们在任何位置定义局部变量,且不能对其他哨卫可见

```haskell
cylinder :: Double -> Double -> Double
Cylinder r h =
    let sideArea=2*pi*r*h
        topArea=pi*r^2
    in sideArea+2*topArea
--以下是格式
let <bindings> in <expression>
```
- 于是,区别就在于,let是个表达式,表达式一定有返回值,便可以放在任意位置
- 因为let是一个表达式,不能在不同的哨卫中被使用
- 另一些人喜欢where是因为允许将主函数体距离函数声明与类型变量更远些,可读性更高

### 3.4.1 几个常见用法

```haskell
ghci> 4*(let a=9 in a+1)+2
42
```

**在局部域中定义函数**

```haskell
ghci> [let square x=x*x in (square 5,square 3,square 2)]
[(25,9,4)]
```
**在一行中绑定多个名字**

```haskell
ghci> (let a=100;b=200;c=300 in a*b*c,let foo="Hey";bar="there!" in foo ++ bar)
(6000000,"Hey there!")
```

**当从一个元组中取值时**

```haskell
ghci> (let(a,b,c)=(1,2,3) in a+b+c)*100
600
```

### 3.4.2 列表推导式中的let

```haskell
calcBmis :: [(Double,Double)]->[Double]
calcBmis xs = [bmi|(w,h)<-xs,let bmi = w/h^2,bmi>25.0]
```
- 在这里,let表达式的用法和谓词差不多,只不过它是用来绑定变量
- 列表推导式的`(w,h)<-xs`部分被称为生成器

### 3.4.3 GHCi中的let

- 直接在GHCi部分定义函数和常量时,let的in部分可以省略
- 如果省略,名字的定义,将会在整个会话过程中可见

---

## 3.5 表达式

- `case`表达式类似c\c++中的`switch`,它取一个变量,对应不同的值并选择代码块并执行
- 实际上,函数定义中的模式匹配不过是case表达式的语法糖而已

```haskell
--下面两段表达式实际上是等价的
head' :: [a]->a
head' []=error "No head for empty list!"
head' (x:_)=x
--case表达式的形式
head' :: [a] -> a
head' xs= case xs of [] ->error "No head for empty lists!"
                     (x:_)->x
--case表达式的语法结构如下
case expression of pattern -> result
                   pattern -> result
                   ...
```
非常简单如果一个模式没有匹配,就交给下一个模式,若到最后没有匹配,就产生错误

**优势**

函数参数的匹配模式只能在定义函数时使用,而case表达式的匹配模式可以用在任意一个地方

