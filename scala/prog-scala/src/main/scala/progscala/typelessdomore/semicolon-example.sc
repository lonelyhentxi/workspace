def equalsign(s: String) = // 行尾等号表示下一行还有未结束的代码
  println("equalsign: " + s)

def equalsign2(s: String) = { // 行尾花括号表示还有未结束的代码
}

def commas(s1:String,
           s2: String) = Console.
print("comma: " + s1 +
"，" + s2)
// 末尾的句号、逗号、操作符都可以表明，下一行还有未结束的代码
// 因此最安全的方式是每行结尾加上分号

// var 是不可变变量