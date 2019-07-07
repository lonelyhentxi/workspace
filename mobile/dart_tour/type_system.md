# The Dart Type System

dart 类型安全，它使用一种静态类型检查和运行时检查的结合，尽管类型是强制性的，但是由于类型推导的存在，类型标注是可选的。

## What is soundness

Soundness 确保程序不会进入错误的状态。一个 sound 的类型系统永远不会进入一个表达式静态类型和其值的实际类型不相符的状态。

## Soundness 的好处

- 在编译期暴露类型相关问题
- 可读性更强的代码
- 增强代码可维护性
- 更好的 AOT 编译

## 通过静态分析的小技巧

- 对于覆盖方法使用 Sound 返回类型
- 对于覆盖方法的参数使用 Sound 类型
- 不要让一个动态列表作为一个类型列表

对于前两者，请考虑好其继承树位置。对于第三点，如下会报错：

```dart
class Cat extends Animal { ... }

class Dog extends Animal { ... }

void main() {
  List<Cat> foo = <dynamic>[Dog()]; // Error
  List<dynamic> bar = <dynamic>[Dog(), Cat()]; // OK
}
```

## 运行时检查

分析器会检查运行时错误，例如基类向派生类的隐式转型。

## 类型推导

分析器可以作用在字段、方法、局部变量以及大多数的泛型参数上。当分析器不能得到足够的信息时，它将使用 `dynamic` 类型。

## 替代类型

超类在大多数情况下可以替代其子类使用，对于数组，将超类赋给子类数组会引发隐式强转。