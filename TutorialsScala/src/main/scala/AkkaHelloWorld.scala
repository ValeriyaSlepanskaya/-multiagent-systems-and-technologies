import akka.actor._

class MyHelloActor(myName: String) extends Actor {
  def receive: PartialFunction[Any, Unit] = {
    case "hello" => println("hello from %s".format(myName))
    case _       => println("'huh?', said %s".format(myName))
  }
}

object Main extends App {
  val system = ActorSystem("HelloSystem")
  val helloActor = system.actorOf(Props(new MyHelloActor("Fred")), name = "helloactor")
  helloActor ! "hello"
  helloActor ! "buenos dias"
}
