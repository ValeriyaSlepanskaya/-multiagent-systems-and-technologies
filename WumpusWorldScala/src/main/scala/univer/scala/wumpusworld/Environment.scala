package univer.scala.wumpusworld

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import univer.scala.wumpusworld.Environment.{ActionResponse, EnvironmentResponse, Request}

class Environment(layout: String) {

  val envBehavior: Behavior[Request] = Behaviors.receive((context, message) => {

    val killSwitch = message match {
      case Environment.EnvironmentRequest(sender) => {
        val environmentState = composeCurrentState()
        sender ! EnvironmentResponse(environmentState)

        false
      }

      case Environment.PerformAction(action, sender) => {
        performAction(action)

        val result = if (!agentAlive) DEATH else if (isGoldTaken) TAKEGOLD else FO
        sender ! ActionResponse(result)

        result != GO
      }
    }

    if (killSwitch) Behaviors.stopped else Behaviors.same
  })

  private val wumpusPositions: RoomPosition = parseWumpusPosition(layout)
  private val goldPosition: RoomPosition = parseGoldPosition(layout)
  private val pitPosition: RoomPosition = parsePitPosition(layout)
  private var isGoldTaken: Boolean = false
  private var isWumpusKilled: Boolean = false
  private var agentHasArrow: Boolean = true
  private val roomSize: (Int, Int) = parseRoomSize(layout)
  private var speleologistPosition: RoomPosition = RoomPosition(0, 0)
  private var speleologistDirection: Direction = Right
  private var wumpusJustKilled: Boolean = false
  private var speleologistBumped: Boolean = false
  private var agentAlive: Boolean = true

  def getSymbolCoordinates(layout: String, symbol: Char): RoomPosition = {
    val rows = layout.split("\r\n")
    val symbolIndexes = rows.map(_.indexOf(symbol))

    val roomPosition = symbolIndexes.zipWithIndex.maxBy(_._1)
    RoomPosition(roomPosition._1, roomPosition._2)
  }

  def parseWumpusPosition(layout: String): RoomPosition = {
    val symbol = 'W'

    getSymbolCoordinates(layout, symbol)
  }

  def parsePitPosition(layout: String): RoomPosition = {
    val symbol = 'P'

    getSymbolCoordinates(layout, symbol)
  }

  def parseRoomSize(layout: String): (Int, Int) = {
    val rows = layout.split("\r\n")
    val height = rows.length
    val width = rows(0).length
    (height, width)
  }

  def parseGoldPosition(layout: String): RoomPosition = {
    val symbol = 'G'

    getSymbolCoordinates(layout, symbol)
  }

  private def composeCurrentState(): WumpusFeeling = {
    var STENCH: Boolean = false
    var GLITTER: Boolean = false
    var BREEZE: Boolean = false
    var SCREAM: Boolean = false
    var BUMP: Boolean = false

    val pos = speleologistPosition

    val adjacentRooms = List(RoomPosition(pos.x - 1, pos.y), RoomPosition(pos.x + 1, pos.y),
      RoomPosition(pos.x, pos.y - 1), RoomPosition(pos.x, pos.y + 1))

    for (r <- adjacentRooms) {
      if (wumpusPositions == r) stench = true
      if (pitPosition == r) breeze = true
    }
    if (pos == goldPosition) glitter = true
    if (wumpusJustKilled) scream = true
    if (speleologistBumped) bump = true

    val result = new WumpusPercept(glitter, stench, breeze, bump, scream)

    result
  }

  def performAction(speleologistAction: SpeleologistAction): Unit = {

    if (wumpusJustKilled) {
      wumpusJustKilled = false
    }

    speleologistAction match {
      case GRAB => tryToGrabGold()
      case CLIMB => climb()
      case TURNRIGHT => turnRight()
      case TURNLEFT => turnLeft()
      case FORWARD => moveSpeleologistForward()
      case SHOOT => tryToKillWumpus()
    }
  }

  def calculateNewPosition(): RoomPosition = {
    val oldPosition = speleologistPosition
    val newPosition = speleologistDirection match {
      case DOWN => RoomPosition(speleologistPosition.x, speleologistPosition.y + 1)
      case LEFT => RoomPosition(speleologistPosition.x - 1, speleologistPosition.y)
      case RIGHT => RoomPosition(speleologistPosition.x + 1, speleologistPosition.y)
      case UP => RoomPosition(speleologistPosition.x, speleologistPosition.y - 1)
    }

    if (newPosition.x >= roomSize._1 || newPosition.x < 0 || newPosition.y < 0 || newPosition.y >= roomSize._2)
      oldPosition else newPosition

  }

  def moveSpeleologistForward(): Unit = {
    val newSpeleologistPosition = calculateNewPosition()
    if (newSpeleologistPosition == speleologistPosition) {
      speleologistBumped = true
    } else {
      speleologistPosition = newSpeleologistPosition
    }
  }

  private def turnRight(): Unit = speleologistDirection match {
    case UP => speleologistDirection = RIGHT
    case DOWN => speleologistDirection = LEFT
    case LEFT => speleologistDirection = UP
    case RIGHT => speleologistDirection = DOWN
  }

  private def turnLeft(): Unit = speleologistDirection match {
    case UP => speleologistDirection = LEFT
    case DOWN => speleologistDirection = RIGHT
    case LEFT => speleologistDirection = DOWN
    case RIGHT => speleologistDirection = UP
  }

  def tryToGrabGold() = if (speleologistPosition == goldPosition) {
    isGoldTaken = true
    println("Gold was taken!")
    println("Speleologist wins!")
  } else {
    println("Speleologist tried to grab gold, but it wasn't there!")
    println("Speleologist lost!")
  }

  def climb() = {
    this.agentAlive = false
  }

  def tryToKillWumpus(): Unit = if (isAgentFacingWumpus(speleologistPosition, speleologistDirection) && agentHasArrow) {
    this.wumpusJustKilled = true
    this.isWumpusKilled = true
    this.agentHasArrow = false
  }

  private def isAgentFacingWumpus(position: RoomPosition, direction: Direction): Boolean = {
    val wumpus = this.wumpusPositions;
    direction match {
      case UP => position.x == wumpus.x && position.y < wumpus.y
      case DOWN =>  position.x == wumpus.x && position.y > wumpus.y
      case RIGHT => position.y == wumpus.y && position.x < wumpus.x
      case LEFT => position.y == wumpus.y && position.x > wumpus.x
    }
  }
}

object Environment {
  sealed trait Request
  sealed trait Response

  case class EnvironmentRequest(sender: ActorRef[Response]) extends Request
  case class EnvironmentResponse(percept: WumpusPercept) extends Response
  case class PerformAction(action: SpeleologistAction, sender: ActorRef[Response]) extends Request
  case class ActionResponse(actionResult: ActionResult) extends Response
}

case class RoomPosition(x: Int, y: Int)

trait Direction

case object UP extends Direction
case object DOWN extends Direction
case object LEFT extends Direction
case object RIGHT extends Direction
