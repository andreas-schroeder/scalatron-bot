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

object Rules {

  val Master      = 'M'
  val EnemyMaster = 'm'

  val Slave       = 'S'
  val EnemySlave  = 's'

  val Snorg       = 'b'
  val Fluppet     = 'B'

  val Toxifera    = 'p'
  val Zugar       = 'P'

  val Wall        = 'W'
  val Unknown     = '?'

  val ExplosionEnemies = Set(EnemyMaster, EnemySlave, Snorg)

  val SlaveBonks  = Set(Wall, Snorg, EnemySlave, Slave, EnemyMaster)
  val MasterBonks = Set(Wall, Snorg, EnemyMaster)
}

class MasterBot(paramMap: Map[String, String]) extends Bot(paramMap) {

  override val bonks = Rules.MasterBonks

  override val master = true

  override val moveRatings = List(AvoidDamage, Harvest, AvoidMasters, AvoidCorners)

}

class SlaveBot(paramMap: Map[String, String]) extends Bot(paramMap) {

  override val bonks = Rules.SlaveBonks

  override val master = false

  override val moveRatings = List(AvoidDamage, Harvest, Spread, Attack)

  override val debug = false

}

object Bot {
  val Heading = "heading"

  val MoveRatingThreshold = 0.99

  val MoveCertainty = 0.8

  val MaxSlaves = 400
}

abstract class Bot(paramMap: Map[String, String]) extends Exploding with Moving {
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

case class SingleMoveRating(move: XY, rating: Double, rater: String)

case class TotalMoveRating(move: XY, rating: Double)

trait MoveRater {

  def name:   String
  def weight: Double

  def rate(bot: Bot, candidate: XY): Double

  def beastSpeed(bot: Bot): Double = if (bot.master) 0.75 else 0.5

  def countAround(bot: Bot, candidate: XY, c: Char): Int =
    candidate.around.count(pos => bot.view.cellAtRelPos(pos) == c)

  def rate(pos: Seq[XY], m: XY, mult: Double = 1.0): Double =
    pos.map(p => Math.pow(Bot.MoveCertainty, mult * (p - m).length)).sum
}

object AvoidDamage extends MoveRater {
  import Rules._

  override val name = "D"
  override val weight: Double = 1.0

  override def rate(bot: Bot, candidate: XY) = {
    import bot._
    val snorgs = if (master) -150 * rate(view.all(Snorg), candidate, mult = 1.5) else 0
    val snorgNeighbor = {
      val snorgSpeed = beastSpeed(bot)
      -150 * (snorgSpeed + (1 - snorgSpeed) * Bot.MoveCertainty)
    }

    val cell = view.cellAtRelPos(candidate) match {
      case Toxifera => -100
      case _        =>
        countAround(bot, candidate, Snorg) * snorgNeighbor
    }

    snorgs + cell
  }
}

object Harvest extends MoveRater {
  import Rules._

  override val name = "H"
  override val weight: Double = 1.0

  override def rate(bot: Bot, candidate: XY) = {
    import bot._

    val zugars   = 100 * rate(view.all(Zugar), candidate)
    val fluppets = 200 * rate(view.all(Fluppet), candidate, mult = 1 + beastSpeed(bot))

    fluppets + zugars
  }
}

object AvoidCorners extends MoveRater {
  import Rules._

  override val name = "U"
  override val weight: Double = 1.0

  override def rate(bot: Bot, candidate: XY) = {
    import bot._

    -10 * rate(view.all(Unknown), candidate)
  }
}

object AvoidMasters extends MoveRater {
  import Rules._

  override val name = "M"
  override val weight: Double = 1.0

  override def rate(bot: Bot, candidate: XY) = {
    import bot._

    -10 * rate(view.all(EnemyMaster), candidate)
  }
}

object Spread extends MoveRater {
  import Rules._

  override val name = "S"
  override val weight: Double = 1.0

  override def rate(bot: Bot, candidate: XY) = {
    import bot._

    val avoid = -5 * rate(view.all(Slave), candidate)
    val cell = -10 * countAround(bot, candidate, Slave)

    avoid + cell
  }
}

object Attack extends MoveRater {
  import Rules._

  override val name = "A"
  override val weight: Double = 1.0

  override def rate(bot: Bot, candidate: XY) = {
    import bot._

    if(master)  0 else bot.energy / 2.0 * rate(view.all(EnemyMaster), candidate)
  }
}


case class Cmd(ops: String, atts: (String, Any)*) {

  override val toString = {
    val kvs = atts.map{ case (k,v) => s"$k=$v" }.mkString(",")
    s"$ops($kvs)"
  }
}

object Cmd {

  def status(text: String) = Cmd("Status", "text" -> text)

  def move(xy: XY) = Cmd("Move", "direction" -> xy)

  def set(key: String, value: Any) = Cmd("Set", key -> value)

  def spawn(dir: XY, energy: Int) = Cmd("Spawn", "direction" -> dir, Bot.Heading -> dir, "energy" -> energy)

  def explode(size: Int) = Cmd("Explode", "size" -> size)

  def markCell(pos: XY, color: String) = Cmd("MarkCell", "position" -> pos, "color" -> color)

  def drawLine(from: XY, to: XY, color: String) = Cmd("DrawLine", "from" -> from, "to" -> to, "color" -> color)

}
