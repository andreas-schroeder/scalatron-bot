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

  val ExplodeCertainty = 0.8

  val MaxSlaves = 400

  val MasterReactors = List(DamageAvoidR, SpawnR, HarvestR, MasterAvoidR, CornerAvoidR)

  val SlaveMoveReactors = List(DamageAvoidR, HarvestR, SpreadR, ExplodeR, AnnihilationAvoidR)

  val SlaveReactors  = SlaveMoveReactors ++ List(SpawnR)

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
  val heading = paramMap.get(Bot.Heading).map(XY(_))

  var commands = List[Cmd]()

  var maybeMove: Option[(XY, Double)] = None

  def add(c: Cmd) = {
    commands = c +: commands
  }

  def run(): String = {
    val reacts = Reactor.compatibles(this, reactors)
    reacts.foreach { r =>
      r.cmd match {
        case Cmd.Move(dir) => maybeMove = Some((dir, 0.0))
        case c => add(c)
      }
    }

    execute()
  }

  def execute(): String = {
    val moveAndHeading: List[Cmd] = maybeMove.map {
      case (xy, _) => List(Cmd.Set(Heading, xy), Cmd.Move(xy))
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