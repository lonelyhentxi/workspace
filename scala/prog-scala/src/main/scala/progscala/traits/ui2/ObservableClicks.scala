package progscala.traits.ui2
import progscala.traits.observer._

trait ObservableClicks extends Clickable with Subject[Clickable] {
  abstract override def click(): Unit = {
    super.click()
    notifyObservers(this)
  }
  // abstract 关键字提醒了读者和编译器，尽管提供了方法体，但是 click 方法没有完全实现
}
