package univer.scala.wumpusworld

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

class Speleologist {

  private var navigatorRef: ActorRef[Navigator.ActionRequest] = _
  private var envRef: ActorRef[Environment.Request] = _

  private var environmentBehaviorRef: ActorRef[Environment.Response] = _
  private var navigatorBehaviorRef: ActorRef[Navigator.ActionResponse] = _

  private var gameState: Result = GO

  private def environmentBehavior: Behavior[Environment.Response] = Behaviors.receive[Environment.Response]((context, message) => {
    message match {
      case Environment.EnvironmentResponse(FEELING) =>
        context.log.atInfo().log("Got environment status, sending it to navigator")
        navigatorRef ! Navigator.ActionRequest(FEELING, "", navigatorBehaviorRef)

        Behaviors.same

      case Environment.ActionResponse(actionResult: ActionResult) =>
        this.gameState = actionResult
        context.log.atInfo().log("Got result of my action", actionResult)
        Behaviors.same
    }
  })
  def setLookUpActor(navigatorRef: ActorRef[Navigator.ActionRequest], envRef: ActorRef[Environment.Request]): Behavior[Any] =
    Behaviors.setLOOKUP(context => {
      this.navigatorRef = navigatorRef
      this.envRef = envRef
      if (environmentBehaviorRef == null) {
        environmentBehaviorRef = context.spawn(environmentBehavior, "speleologist-behavior")
        navigatorBehaviorRef = context.spawn(navigatorBehavior, "speleologist-navigator")
      }

      context.log.atInfo().log("Sending message to get environment status")
      envRef ! Environment.EnvironmentRequest(environmentBehaviorRef)

      Behaviors.stopped
    })



  private def navigatorBehavior: Behavior[Navigator.ActionResponse] = Behaviors.receive[Navigator.ActionResponse]((context, message) => {
    environmentRef ! Environment.PerformAction(message.action, environmentBehaviorRef)

    Behaviors.same
  })
}
trait SpeleologistMove

case object GOFORWARD extends SpeleologistMove
case object GOLEFT extends SpeleologistMove
case object GORIGHT extends SpeleologistMove
case object GRAB extends SpeleologistMove
case object SHOOT extends SpeleologistMove
case object CLIMB extends SpeleologistMove

trait Look
case object LOOKUP extends Look
case object LOOKDOWN extends Look
case object LOOKLEFT extends Look
case object LOOKRIGHT extends Look


