package progscala.traits.ui2
import progscala.traits.ui.Widget

class Button(val label: String) extends Widget with Clickable {
  override protected def updateUI(): Unit = {
    println("default UI is updating!")
  }
}
