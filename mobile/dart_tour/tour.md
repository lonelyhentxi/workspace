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

## 操作符

### 算术操作符

- \+
- \-
- \-expr
- \*
- /
- \\
- ~/ (整除)
- %

### 相等性和关系操作符

- ==
- !=
- \>
- \<
- \>=
- \<=

### 类型测试操作符

- as 类型转换
- is 类型检真
- is! 类型检假

### 赋值操作符

- =
- -=
- /=
- %=
- \>\>=
- ^=
- +=
- *=
- ~/=
- /</<=
- &=
- |=

### 逻辑操作符

- !expr
- ||
- &&

### 位和位移运算符

- &
- |
- ^
- ~expr
- \<\<
- \>\>

### 条件运算符

- condition ? expr1 : expr2
- expr1 ?? expr2

### 级联标记

```dart
querySelector('#confirm')
  ..text = 'Confirm'
  ..classes.add('important')
  ..onClick.listen((e)=>window.alert('Confirmed!'))
```

可以将再一个对象上的多个操作连接起来。

### 其他运算符

- \(\) 函数应用
- \[\] 列表访问
- \. 成员访问
- ?. 存在性访问

## 控制流语句

### If Else

```dart
if (isRaining()) {
  you.bringRainCoat();
} else if (isSnowing()) {
  you.wearJacket();
} else {
  car.putTopDown();
}
```

### For Loops

```dart
for(var i=0;i<5;i++) {
    message.write("!");
}
```

**注意**：循环闭包表现相当于 `es6` 中的 `let`、`const`，而非 `var`，每次都会视为一个新的值。

可迭代对象也会有 `forEach` 方法。

### While 和 Do While

`类 C`。

### Break 和 Continue

`类 C`，其中 `continue` 操作一般可以用可迭代对象替代。

### Switch 和 Case

`Dart` 中使用 `==` 比较整数、字符串、和运行时常量等。比较的两方必须是同种类型。`枚举类型` 可以在 `Switch` 语句中工作良好。

```dart
var command = 'OPEN';
switch (command) {
  case 'CLOSED':
    executeClosed();
    break;
  case 'PENDING':
    executePending();
    break;
  case 'APPROVED':
    executeApproved();
    break;
  case 'DENIED':
    executeDenied();
    break;
  case 'OPEN':
    executeOpen();
    break;
  default:
    executeUnknown();
}
```

### Assert

```dart
assert(condition, optionalMessage)
```

## 异常

Dart 能够使用 `throw catch` 模式，

### Throw

```dart
throw FormatExpection('Expected at least 1 section.');
throw 'Out of llamas';
```

### Catch

```dart
try {
  breedMoreLlamas();
} on OutOfLlamasException {
  buyMoreLlamas();
} on Exception catch (e) {
  print('Unknown exception: $e');
} catch (e,s) {
  print('Something really unknown: $e');
}
```

其中 `catch` 函数的第二个参数是 `StackTrace`。

### Finally

保证一定会执行。

## 类

Dart 是一门基于混入的面向对象语言。

### 使用类成员

`.` 操作符，`?.` 操作符可以在 `null` 时短路。

### 使用构造器

1. 不同于其他`类 C` 面向对象语言，使用 `new` 操作符和不使用都可以，并且有相同的效果。
2. 静态工厂函数也可以被视为构造函数。

```dart
var p1 = Point(2, 2);
var p2 = Point.fromJson({'x': 1, 'y': 2});
var p1 = new Point(2, 2);
var p2 = new Point.fromJson({'x': 1, 'y': 2});
```

某些类提供了常量构造器。为了创建编译期常量，需要在构造器名前使用 `const` 关键字：

```dart
var a = const ImmutablePoint(1,1);
const pointAndLine = const {
    'point': const [const ImmutablePoint(0,0)],
    'line': const [const ImmutablePoint(1,10),const ImmutablePoint(-2,11)],
};
const pointAndLine = {
  'point': [ImmutablePoint(0, 0)],
  'line': [ImmutablePoint(1, 10), ImmutablePoint(-2, 11)],
};
```

由于常量要求所有成员都是常量，可以只在变量修饰符上声明为常量。

### 获得对象类型

```dart
print('The type of a is ${a.runtimeType}');
```

得到运行时类型，其类型为 `Type`。

### 实例变量

类内字段在未初始化的情况下将为 `null`。

### 构造器

```dart
class Point {
    num  x, y;

    Point(num x,num y) {
        this.x = x;
        this.y = y;
    }
}
```

#### 默认构造器

无参数。

#### 构造器不可继承                                                            

#### 命名构造器

```dart
class Point {
  num x, y;

  Point(this.x, this.y);

  Point.origin() {
    x = 0;
    y = 0;
  }
}
```

#### 触发非默认超类构造器

```dart
class Person {
    String firstName;
    
    Person.fromJson(Map data) {
        print('in Person');
    }
}

class Employee extends Person {
    Employee.fromJson(Map data): super.fromJson(data) {
        print('in Employee');
    }
}

main() {
    var emp = new Employee.fromJson({});
    if(emp is Person) {
        emp.firstName = "Bob";
    }
    (emp as Person).firstName = "Bob";
}
```

#### 初始化列表

```dart
Point.fromJson(Map<String, num> json)
    : x = json['x'],
      y = json['y'] {
  print('In Point.fromJson(): ($x, $y)');
}
```

在开发期，还可使用 `assert` 帮助排除问题。

#### 常量构造器

```dart
class ImmutablePoint {
    static final ImmutablePoint origin =
        const ImmutablePoint(0,0);
    
    final num x, y;

    const ImmutablePoint(this.x, this.y);
}
```

#### 工厂函数

可以使用 `factory` 关键字来创建工厂函数。

```dart
class Logger {
    final String name;
    bool mute = false;
    static final Map<String, Logger> _cache = <String, Logger>();
    factory Logger(String name) {
        if(__cache.containsKey(name)) {
            return _cache[name];
        } else {
            final logger = Logger._internal(name);
            _cache[name] = logger;
            return logger;
        }
    }
    Logger._internal(this.name);
    void log(String msg) {
        if(!mute) print(msg);
    }
}
```

### 方法

#### 实例方法

略

#### 访问器

```dart
class Rectange {
  num left, top, width, height;

  Rectangle(this.left, this.top, this.width, this.height);

  num get right => left + width;
  set right(num value) => left = value - width;
  num get bottom => top + height;
  set bottom(num value) => top = value - height;
}
```

#### 抽象方法

```dart
abstract class Doer {
    void doSomething();

}

class EffectiveDoer extends Doer {
    void doSomething() {}
}
```

### 抽象类

使用 `abstract` 修饰符修饰的类是抽象类，表示该类不能被实例化。抽象类用于拥有某些实现的接口。如果想要初始化抽象类，可以使用工厂构造器。

```dart
abstract class AbstractContainer {
    void updateChildren();
}
```

### 隐式接口

每个类都会隐式的实现一个含有自己全部函数签名的接口。

```dart
class Person {
    final _name;
    Person(this._name);
    String greet(String who) => 'Hello, $who. I am $_name.';
}

class Impostor implements Person {
    get _name => '';
    String greet(String who) => 'Hi $who. Do you know who I am?';
}

String greetBob(Person person) => person.greet('Bob');
```

### 拓展类

```dart
class Television {
  void turnOn() {
    _illuminateDisplay();
    _activateIrSensor();
  }
}
class SmartTelevision {
  void turnOn() {
    super.turnOn();
    _bootNetworkInterface();
    _initializeMemory();
    _upgradeApps();
  }
}
```

#### 覆盖方法

超类可以覆盖实例方法，访问器、设置器。可以使用 `@override` 标记来显式标记覆盖一个成员函数。

```dart
class SmartTelevision extends Television {
    @override
    void turnOn() { ... }
}
```

#### 覆盖操作符

```dart
class Vector {
  final int x, y;

  Vector(this.x, this.y);

  Vector operator +(Vector v) => Vector(x + v.x, y + v.y);
  Vector operator -(Vector v) => Vector(x - v.x, y - v.y);
}
```

#### noSuchMethod

类似 `ruby` 里的类似方法，代理所有的未找到方法。

```dart
class A {
    @override
    void noSuchMethod(Invocation invocation) {
        print('You tried to use a non-existent member: ' + 
            '${invocation.memberName}'
        );
    }
}
```

你只能在存在以下某个条件时才能够唤起未实现方法：

- 接受者含有静态类型 dynamic
- 接受者定义了未实现的方法（或者抽象方法）、并且动态类实现了 `noSuchMethod`。

### 枚举类型

默认情况下，`enum` 其值为整数，可以使用 `enum.value.index` 获取器获取其值。

**注意**

1. 不能作为超类，混入，实现。
2. 不能显式实现 `enum` 的超类。

### 混入

混入是一种向多类架构中重用类代码的方法。

```dart
mixin Musical {
    bool canPlayPiano = false;
  bool canCompose = false;
  bool canConduct = false;

  void entertainMe() {
    if (canPlayPiano) {
      print('Playing piano');
    } else if (canConduct) {
      print('Waving hands');
    } else {
      print('Humming to self');
    }
  }

class Musician extends Performer with Musical {}

class Maestro extends Person with Musical, Aggressive, Demented {
    Maestro(String maestroName) {
        name = maestroName;
        canConduct = true;
    }
}
```

混入也可拓展

```dart
mixin MusicalPerformer on Musician {
    // ...
}
```

### 类变量和方法

使用 `static` 关键字来实现类级别的变量和方法。

#### 静态变量

```dart
class Queue {
  static const initialCapacity = 16;
}
```

#### 静态方法

略

## 泛型

### 为何使用泛型

- 为了类型安全，泛型通常是必要的
- 减少代码重复
- 适当的声明泛型可能导致更好的代码生成

### 使用集合字面值

```dart
var names = <String>['Seth','Kathy','Lars'];
var uniqueNames = <String>{'Seth','Kathy','Lars'};
var pages = <String, String>{
  'index.html': 'Homepage',
  'robots.txt': 'Hints for web robots',
  'humans.txt': 'We are people, not machines'
};
```

#### 使用参数类型构造器

```dart
var nameSet = Set<String>.from(names);
var views = Map<int,View>();
```

#### 泛型容器及其包含

```dart
var names = List<String>();
names.addAll(['Seth', 'Kathy', 'Lars']);
print(names is List<String>); // true
```

java 的虚拟机对泛型采用擦除，在 Java 中只能测试类型是否是 `List` 而不能测试 `List<String>`。

#### 限制参数化类型

```dart
class Foo<T extends SomeBaseClass> {
  // Implementation goes here...
  String toString() => "Instance of 'Foo<$T>'";
}
```

### 使用泛型方法

```dart
T first<T>(List<T> ts) {
  T tmp = ts[0]
  return tmp;
}
```

## 库和可见性

`import` 和 `library` 指令能够帮助创建模块化和共享的代码库。库不仅提供 API，而且也提供一系列隐私。开始于 `_` 的标识符的可见性是库级别的。每一个 dart 应用都是一个库，即时不使用 `library` 指令。

### 导入库

import 声明了如何从另一个库中导入一个命名空间。例如：

```dart
import 'dart:html';
```

唯一需要的参数是库的 URI，对于内置库，需要 `dart:` 格式。对于其他的库，需要文件系统路径或者包名格式，例如：

```dart
import `package:test/test.dart`;
```

### 重定向

其中可以使用 `as` 来重命名。使用 `show`、`hide` 来进行部分导入。

#### 懒加载

懒加载的原因：

- 减少程序初始化时间
- 尝试替代性算法
- 载入很少使用的功能

```dart
import 'package:greeting/hello.dart' deferred as hello;

Future greet() async {
  await hello.loadLibrary();
  hello.printGreeting();
}
```

多遍加载仍然只会加载一次，注意以下问题：

- 延迟加载库的常量在导入的时候是不可用的。 只有当库加载完毕的时候，库中常量才可以使用。
- 在导入文件的时候无法使用延迟加载库中的类型，如果需要使用类型，考虑把接口类移动到另外一个库中，让两个库都分别导入这个类。
- dart 隐含地导入了 `loadLibrary` 方法，该方法返回一个 `Future`.

## 异步支持

dart 中有很多返回 `Future` 和 `Stream` 的函数，这些函数是异步的。当他们收拾好一些耗时的操作之后返回，而不去等待操作完成。

`await` 和 `async` 关键词支持异步编程，帮助写出类似同步的代码。

### 处理 `Future`

```dart
Future main() async {
  checkVersion();
  print('In main: version is ${await lookUpVersion}');
}
```

### 处理 `Stream`

当需要异步流时，有两种选择：

- 使用循环 `await`（`await for`）
- 使用 `Stream API`

使用 `async for` 之前，确认它确实能够使得代码更加清楚，并且你确实希望要等待所有结果。例如，UI 事件监听器请不要使用，因为它没有终点。

```dart
await for (varOrType identifier in expression) {
  // Execute each time the stream emits a value.
}
```

执行过程如下：

1. 等待流发出一个值。
2. 执行循环体。
3. 重复 1 和 2 直到流被关闭。

只有 `break` 和 `return` 可以改变控制流。

## 生成器

当需要延迟产生一个值序列的时候，考虑使用生成器函数。

- 同步生成器：返回一个可迭代对象。
- 异步生成器：返回一个流对象。

```dart
// 同步
Iterable<int> naturralsTo(int n) sync* {
  int k = 0;
  while(k < n) yield k++;
}
// 异步
Stream<int> asynchronousNaturalsTo(int n) async * {
  if (n>0) {
    yield n;
    yield* naturalsDownFrom(n-1);
  }
}
```

当函数是递归的时候，可以使用 `yield*` 以提升性能：

```dart
Iterable<int> naturalsDownFrom(int n) sync* {
  if(n>0) {
    yield n;
    yield* naturalsDownFrom(n-1);
  }
}
```

## 可调用类

实现 `call` 方法的类可以被调用。

## Isolates

多数计算机，甚至移动平台上，有着多核 CPU。为了充分利用这些核心，开发者传统上使用共享内存线程的方法来运行并发。然而，多线程共享数据通常会导致很多潜在的问题，并导致代码运行出错。

所有的 dart 代码运行在 isolates 中而不是线程。每个 isolate 都有自己的堆内存，并且确定状态不能被其他 isolate访问。

## Typedefs

typedef 用以为函数类型命名或者重命名，防止其类型丢失。

```dart
typedef int Compare(Object a, Object b);

class SortedCollection {
  Compare compare;

  SortedCollection(this.compare);
}

 // Initial, broken implementation.
 int sort(Object a, Object b) => 0;

main() {
  SortedCollection coll = new SortedCollection(sort);
  assert(coll.compare is Function);
  assert(coll.compare is Compare);
}
```

## 元数据

使用元数据可以为代码添加其他额外信息，元数据注解是以 `@` 为开头，后面是一个编译期常量或者调用一个常量构造函数。有三个注解所有的 dart 代码都可以使用，他们是 `@deprecated`、 `@override` 和 `@proxy`。

```dart
class Television {
  @deprecated
  void activate() {
    turnOn();
  }

  void turnOn() {
    print('on!');
  }
}
```

可以定义并使用自己的注解

```dart
library todo;
class todo {
  final String who;
  final String what;

  const todo(this.who, this.what);
}

@todo('seth','make this do something')
void doSomething() {
  print('do something');
}
```

## 注释

单号注释和多行注释和 C++ 类似，多行注释可以嵌套。

### 文档注释

以 `///` 开始，和 java 类似，具体参考 [Dart 文档生成指南](https://dart.dev/guides/language/effective-dart/documentation)