package univer.scala.wumpusworld

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors

object Main {

  def main(args: Array[String]): Unit = {
    val environment = new Environment(layout)
    val navigator = new Navigator
    val speleologist = new Speleologist

 val layout: String =
    """****
      |*P**
      |**G*
      |*W**""".stripMargin
	  
    val system: ActorSystem[Any] = ActorSystem(Behaviors.setup[Any] (context => {
      val environmentRef = context.spawn(environment.envBehavior, "environment")
      val navigatorRef = context.spawn(navigator.navigatorActor, "snavigator")
      val speleologistRef = context.spawn(speleologist.setupActor(navigatorRef, environmentRef), "speleologist")
      Behaviors.same
    }), "system")
  }
}
