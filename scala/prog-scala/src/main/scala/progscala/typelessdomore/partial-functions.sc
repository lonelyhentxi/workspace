// 偏函数指不处理所有的输入，只处理那些能与至少一个 case 匹配的输入
val pf1:PartialFunction[Any,String] = { case s:String => "YES" }
val pf2:PartialFunction[Any,String] = { case d:String => "YES" }

val pf = pf1 orElse pf2

def tryPF(x: Any, f: PartialFunction[Any,String]): String =
  try { f(x).toString } catch { case _:MatchError => "ERROR!" } // 捕获偏函数无法匹配的分支

def d(x: Any, f:PartialFunction[Any,String]): String =
  f.isDefinedAt(x).toString // 显示偏函数对某输入值是否有匹配


