package hive


class MasterBot(paramMap: Map[String, String]) extends Bot(paramMap) {

  override val bonks = Rules.MasterBonks

  override val master = true

  override val reactors = Bot.MasterReactors
}

class SlaveBot(paramMap: Map[String, String]) extends Bot(paramMap) {

  override val bonks = Rules.SlaveBonks

  override val master = false

  override val reactors = Bot.SlaveReactors

  override val debug = false

}

object Bot {
  val Heading = "heading"

  val WinnerRatingThreshold = 0.99

  val MoveCertainty = 0.8

  val ExplodeCertainty = 0.6

  val MaxSlaves = 400

  val MasterReactors = List(DamageAvoidR, HarvestR, MasterAvoidR, CornerAvoidR)

  val SlaveReactors = List(DamageAvoidR, HarvestR, SpreadR, CornerAvoidR, ExplodeR, AnnihilationAvoidR)

}

abstract class Bot(paramMap: Map[String, String]) {

  import Bot._

  val debug = false
  val bonks: Set[Char]
  val master: Boolean
  val slave = !master
  val reactors: List[Reactor]

  val view = View(paramMap("view"))
  val gen = paramMap("generation").toInt
  val slaves = paramMap("slaves").toInt
  val energy = paramMap("energy").toInt
  val collision = paramMap.get("collision").map(XY(_))
  val heading = paramMap.get(Bot.Heading).map(XY(_))

  var commands = List[Cmd]()

  var maybeMove: Option[XY] = None

  def exploding: Boolean = commands.exists(_.isInstanceOf[Cmd.Explode])

  def add(c: Cmd) = c match {
    case Cmd.Move(xy) => maybeMove = Some(xy)
    case _ => commands = c +: commands
  }

  def run(): String = {
    Reactor.process(this)
    execute()
  }

  def execute(): String = {
    val moveAndHeading: List[Cmd] = maybeMove.map {
      xy => List(Cmd.Set(Heading, xy), Cmd.Move(xy))
    }.to[List].flatten

    (moveAndHeading ++ commands).mkString("|")
  }

  def free(xy: XY): Boolean = !bonks.contains(view.cellAtRelPos(xy))

  def gradient(n: Double) = {
    def hex(n: Double) = Integer.toHexString(Math.min(255, Math.max(0, n.toInt)))
    val r = hex((255 * n) / 100)
    val g = hex((255 * (100 - n)) / 100)
    val b = "00"
    s"#$r$g$b"
  }
}