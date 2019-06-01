# Dart Tour

## Variables

```dart
var name = "Bob";
dynamic name = "Bob";
String name = "Bob";
Object name = "Bob";
```

`var` 变量类型不可变，相当于 `c#` 的 `var`。`dynamic` 可以动态推断方法（运行时）。可以直接显式声明方法。`Object` 是所有类型的基类，被赋予该类型将只能使用基类方法。

## Default Value

```dart
int lineCount;
assert(lineCount==null)
```

未初始化的变量将以 `null` 作为初始值，即使是数值类型也不意外。

## Final and Const

```dart
final name = "Bob";
final String nickname = "Bobby";
const bar = 10000;
const double atm = 1.01325 * bar;
var foo = const [];
final bar = const [];
const baz = [];
```

`final` 是静态常量，在第一次使用时初始化；`const` 是编译期常量；可以使用等式右边的 `const` 关键字，意思相当于 `c++` 的 `constexpr`，这样左边可以使用 `var` 进行推导。`final` 和 `const` 可以在后方显式声明变量。

## Built-in Types

以下类型是内建类型：

- numbers
- strings
- booleans
- lists(also known as arrays)
- sets
- maps
- runes(for expressing Unicode in a string)
- symbols

### numbers

#### int

整数不会比64位更大，依赖于 `Dart VM` 的平台，可以是 `-2^63 - 2^63-1`。当编译到 `JavaScript` 时，只会允许 `-2^53 - 2^53-1`。

#### double

64位浮点数遵守 `IEEE754` 标准。数值计算方法附加在这些类型上，而非类似的 `Math.floor` 等。

#### 用法示例

```dart
var x = 1;
var hex = 0xDEABEEF;
var y = 1.1;
var exponents = 1.42e5;
double z = 1;
var one  = int.parse('1');
var onePointOne = double.parse('1.1');
String oneAsString = 1.toString();
String piAsString = 3.14159.toStringAsFixed(2);
```

常量可以进行编译期计算。

### Strings

`Dart` 的字符串是 UTF-16 码点。

```dart
var s1 = 'String';
var s2 = '\'String';
var s3 = '$s1';
var s4 = '${s2}';
var s5 = """
what do you think?
""";
var s6 = r'what do you think?';
```

### Booleans

该类型只有两个单例对象。

### Lists

```dart
var list = [1, 2, 3];
list[1] = 1;
var constantList = const [1,2,3];
```

可以使用 `spread operator(...)`，`null-aware spread operator(...?)`，提供了插入多个值的方法。

```dart
var list = [0,...list];
var list2 = [0,...?list];
```

Dart 2.3 开始支持了 `collection if` 和 `collection of` 语法，可以方便的在集合 中使用逻辑控制流。

```dart
var nav = [
    'Home',
    'Furniture',
    'Plants',
    if (promoActive) 'Outlet'
];
var listOfInts = [1,2,3];
var listOfStrings = [
    '#0',
    for (var i in listOfInts) '#$i'
];
```

### Sets

初始化

```dart
var halogens = {'fluorine', 'chlorine', 'bromine', 'iodine', 'astatine'};
var names = <String>{};
```

操作

```dart
var elements = <String>{};
elements.add('fluorine');
elements.addAll(halogens);
```

也可以使用集合展开和控制流操作。

### Maps

```dart
var nobleGases = {
  2: 'helium',
  10: 'neon',
  18: 'argon',
};
```

访问使用的也是 `operator[]`;

### Runes

Dart 使用 16 位码点。

```dart
main() {
    var clapping = '\u{1f44f}';
    print(clapping);
    print(clapping.codeUnits);
    print(clapping.runes.toList());
    Runes input = new Runes(
        '\u2665 \u{1f605} \u{1f60e} \u{1f47b} \u{1f596} \u{1f44d}');
    print(new String.fromCharCodes(input));
}
```

### Symbols 

类似于 JavaScript 和 Ruby 中的 `Symbol`。

```dart
#radix
#bar
```

## Functions

代表所有可以被赋予值或者当做参数传递的函数。

```dart
bool isNoble(int atomicNumber) {
    return _nobleGases[atomicNumber] != null;
}

isNoble(atomicNumber) {
    return _nobleGases[atomicNumber] != null;
}

bool isNoble(int atomicNumber) => _nobleGases[atomicNumber] != null;
```

以上分别是传统的函数（带类型定义）、推导的定义（返回值为 dynamic 类型）、lambda 定义。

### 可选参数

#### 可选命名参数

```dart
enableFlags(bold: true, hidden: false); // 调用
void enableFlags({bool bold, @required bool hidden}) { // 定义
    ……
}
```

#### 可选位置参数

```dart
String say(String from, String msg, [String device]) {
    var result = '$from say $msg';
    if (device != null) {
        result = '$result with a $device';
    }
    return result;
}
```

#### 默认参数值

```dart
void enableFlags({bool bold = false, bool hidden = false}) { ... }
```

### 主函数

每个应用必须有一个主函数作为应用的入口点。`main` 函数返回 `void` 并且还有一个可选的 `List<String>` 参数列表。

### 函数是一等对象

可以被赋值、传递、修改。

### 匿名函数

```dart
var list = ['apples','bananas','oranges'];
list.forEach((item) {
    print('${list.indexOf(item)}:$item');
})
```

### 语法作用域

`dart` 遵循语法作用域，函数可以嵌套声明。

### 语法闭包

```dart
Function makeAddr(num addBy) {
    return (num i) => addBy + i;
}
```

### 返回值

如果没有返回值，则为 `null`;