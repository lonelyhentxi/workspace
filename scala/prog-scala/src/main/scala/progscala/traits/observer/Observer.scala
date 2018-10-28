package progscala.traits.observer

trait Observer[-State] {
  def receiveUpdate(state: State): Unit
}

trait Subject[State] {
  private var observers: List[Observer[State]] = Nil

  def addObserver(observer: Observer[State]): Unit =
    observers ::= observer // 将 observer 安排到 observers 前面

  def notifyObservers(state: State): Unit =
    observers foreach (_.receiveUpdate(state))
}

// 声明未包含方法实现的类的时候必须使用 abstract 关键字
