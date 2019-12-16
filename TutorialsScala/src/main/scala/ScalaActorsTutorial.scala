import akka.actor._

case object Ping
case object Pong
case object Start
case object Stop

class Ping(pong: ActorRef) extends Actor {
  var count = 0

  def incrementAndPrint {
    count += 1
    println("Ping: pong " + count)
  }

  def receive = {
    case Start =>
      incrementAndPrint
      pong ! Ping
    case Pong =>
      incrementAndPrint
      if (count > 99) {
        sender ! Stop
        println("Ping: stop")
        context.stop(self)
      } else {
        sender ! Ping
      }
  }
}

class Pong extends Actor {
  def receive: PartialFunction[Any, Unit] = {
    case Ping =>
      println("Pong: ping")
      sender ! Pong
    case Stop =>
      println("Pong: stop")
      context.stop(self)
  }
}

object PingPongTest extends App {
  val system = ActorSystem("PingPongSystem")
  val pong = system.actorOf(Props[Pong], name = "Pong")
  val ping = system.actorOf(Props(new Ping(pong)), name = "Ping")
  ping ! Start
}