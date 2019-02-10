# 你好,递归

---

本章我们好好审视了递归,并体会了为何递归在haskell中有着如此的重要性

借助递归的方法,学习了寻找简洁而优雅的求解方法

---

**递归**

- 递归是指在函数的定义中应用自身的方式
- 按照递归的思想,一般倾向于将问题展开为同样的子问题,并不断的对问题进行展开和求解,直到抵达问题的*基准条件(base case)*为止,基准条件中,问题不必做分解,而由程序员明确的给出一个非递归的结果

**递归与haskell**

递归在haskell中至关重要:
- 在haskell中,重要的不是给出求解的步骤,而是定义问题与解的描述

---

# 4.1 不可思议的最大值

下面我们用maximum函数举一个例子.以下是maximum函数的一种递归的实现

```haskell
-- maximum取一组可排序的元素组成的列表而返回其中的最大值
maximum' :: (Ord a)=>[a]->a
maximum' []=error "maximum of empty list"
maximum' [x]=x
maximum' (x:xs)=max x (maximum' xs)
```
---

# 4.2 更多的几个递归函数的实现

### 4.2.1 replicate

`replicate`函数取一个整数和一个元素作为参数,返回一个包含多个重复元素的列表

```haskell
replicate' :: Int=>a-> [a]
replicate' n x
    | n<=0 =[]
    | otherwise =x:replicate' (n-1) x
```

### 4.2.2 take

```haskell
take' :: (Num i,Ord i)=>i->[a]->[a]
take' n _
    | n<=0 =[]
take' _ [] =[]
take' n (x:xs)=x:take' (n-1) xs
```

### 4.2.3 reverse

```haskell
reverse' :: [a]->[a]
reverse' []=[]
reverse' (x:xs)=reverse' xs ++ [x]
```

### 4.2.4 repeat

```haskell
repeat' :: a->[a]
repeat' x = x:repeat' x
```

这个例子虽然没有显式的基准条件,但由于haskell是惰性的,只要使用take能够在某位置截断他即可

### 4.2.5 zip

```haskell
zip' :: [a]->[b]->[(a,b)]
zip' _ [] =[]
zip' [] _ =[]
zip' (x:xs) (y:ys)=(x,y):zip' xs ys
```

### 4.2.6 elem

```haskell
elem' :: (Eq a)=>a -> [a]->Bool
elem' a []=False
elem' [] a=True
elem' a (x:xs)
    | a==x = True
    | a/=x = elem' a xs
```

---

## 4.3 快点,排序

取一组可比较的元素构成的列表,并对其进行求解,这一问题天生就适合使用递归进行解决

接下来使用快速排序作为例子

### 4.3.1 算法思路

- 取得一个待排序的列表,取得它的第一个元素,找出列表中所有比5小的元素安排到5的左侧,大的安排到右侧,在这里这个作为比较标准的元素被称为*基准(pivot)*
- 然后对基准的左侧和右侧分别递归地调用同一个函数

### 4.3.2 编写代码

```haskell
quicksort :: (Ord a)=>[a]->[a]
quicksort []=[]
quicksort (x:xs)= 
    let smallerOrEqual = [a|a<-xs,a<=x]
        larger = [a|a<-xs,a>x]
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger
```

---

# 4.4 递归的思考

**递归的固定模式**

- 先定义基准条件,也就是应对特殊输入的简单非递归解
- 然后将问题分解为一个或多个子问题,并递归的调用自身
- 最后基于子问题里得到的结果,组合成为最终解


