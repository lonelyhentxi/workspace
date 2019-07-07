package progscala.first.shapes

import akka.actor.{Props,Actor,ActorRef,ActorSystem}
import com.typesafe.config.ConfigFactory

private object Start

object ShapesDrawingDriver {
  def main(args:Array[String]): Unit = {
    val system = ActorSystem("DrawingActorSystem",ConfigFactory.load())
    val drawer = system.actorOf(
      Props(new ShapesDrawingActor),"drawingActor")
    val driver = system.actorOf(
      Props(new ShapesDrawingDriver(drawer)),"drawingService"
    )
    driver ! Start
  }
}

class ShapesDrawingDriver(drawingActor: ActorRef) extends Actor {
  import Messages._

  override def receive: Receive = {
    case Start =>
      drawingActor ! Circle(Point(),1.0)
      drawingActor ! Rectangle(Point(),2,5)
      drawingActor ! 3.14159
      drawingActor ! Triangle(Point(),Point(2.0),Point(1.0,2.0))
      drawingActor ! Exit
    case Finished =>
      println(s"ShapesDrawingDriver: cleaning up...")
      context.system.stop(self)
    case response: Response =>
      println("ShapesDrawingDriver: Response = "+response)
    case unexpected =>
      println("ShapesDrawingDriver: ERROR: Receive an unexpected message = " +unexpected)
  }
}
