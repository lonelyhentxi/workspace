package progscala.traits.ui
import progscala.traits.observer._

class ObservableButton(name:String) extends Button(name) with Subject[Button] {
  override def click(): Unit = {
    super.click()
    notifyObservers(this)
  }
}
// 使用了 trait，功能等同于 CallbackButton
