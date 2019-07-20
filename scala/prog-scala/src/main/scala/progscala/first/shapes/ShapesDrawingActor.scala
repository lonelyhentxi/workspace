package progscala.first.shapes

object Messages {
  object Exit
  object Finished
  case class Response(messages: String)
}

import akka.actor.Actor

class ShapesDrawingActor extends Actor {
  import Messages._

  // 该函数是一个接受 any 返回 unit 的偏函数
  // ！是一个方法名，遵循了 erlang 关于 actor 的规范
  // sender ! response = sender.!(response)
  override def receive: Receive = {
    case s: Shape =>
      s.draw(str=>println(s"ShapesDrawingActor: $str")) // ruby?
      sender ! Response(s"ShapesDrawingActor: $s drawn")
    case Exit =>
      println(s"ShapesDrawingActor: exiting...")
      sender ! Finished
    case unexpected =>
      val response = Response(s"ERROR: Unknown message: $unexpected")
      println(s"ShapesDrawingActor: $response")
      sender ! response
  }
}
