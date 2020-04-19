// # 用文件和名空间组织代码
package com {
  // scala 支持使用嵌套块结构来定义包的作用域
  package example {
    package pkg1 {

      class Class11 {
        def m = "m11"
      }

      class Class12 {
        def m = "m12"
      }
    }

    package pkg2 {
      class Class21 {
        def m = "m21"
        def makeClass11 = {
          new pkg1.Class11
        }
        def makeClass12 = {
          new pkg1.Class12
        }
      }
    }
  }
}