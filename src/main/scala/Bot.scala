class ControlFunctionFactory {
  def create: (String) => String = new ControlFunction().respond _
}

class ControlFunction {

  def respond(input: String): String = {
    val (opcode, paramMap) = CommandParser(input)

    if( opcode == "React" ) {
      val gen = paramMap("generation").toInt
      if(gen == 0)
        new MasterBot(paramMap).run()
      else
        new SlaveBot(paramMap).run()
    } else ""
  }
}

class MasterBot(paramMap: Map[String, String]) extends Bot(paramMap) {

  override val bonks = Rules.MasterBonks

  override val master = true

  override val moveRatings = Bot.MasterMoveRaters

}

class SlaveBot(paramMap: Map[String, String]) extends Bot(paramMap) {

  override val bonks = Rules.SlaveBonks

  override val master = false

  override val moveRatings = Bot.SlaveMoveRaters

  override val debug = false

}

object Bot {
  val Heading = "heading"

  val MoveRatingThreshold = 0.99

  val MoveCertainty = 0.8

  val ExplodeCertainty = 0.6

  val MaxSlaves = 400

  val MasterMoveRaters =  List(AvoidDamage, Harvest, AvoidMasters, AvoidCorners)

  val SlaveMoveRaters = List(AvoidDamage, Harvest, Spread, Attack, AvoidAnnihilation)
}

abstract class Bot(paramMap: Map[String, String]) extends Moving with Exploding {
  import Bot._


  val debug = false
  val bonks: Set[Char]
  val master: Boolean
  val slave = !master
  val moveRatings: List[MoveRater]

  val view = View(paramMap("view"))
  val gen = paramMap("generation").toInt
  val slaves = paramMap("slaves").toInt
  val energy = paramMap("energy").toInt
  val heading = paramMap.get(Bot.Heading).map(XY(_))

  var commands = List[Cmd]()

  def add(c: Cmd) = {
    commands = c +: commands
  }

  def run(): String = {
    computeMove()
    computeExplosion()

    if(!exploding) {
      computeSpawn()
    }

    execute()
  }

  def execute(): String = {
    val moveAndHeading: List[Cmd] = maybeMove.map {
      case (xy, _) => List(Cmd.set(Heading, xy), Cmd.move(xy))
    }.to[List].flatten

    (moveAndHeading ++ commands).mkString("|")
  }
}