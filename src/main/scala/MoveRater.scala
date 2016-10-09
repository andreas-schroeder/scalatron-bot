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
  override val weight: Double = 0.8

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
  override val weight: Double = 0.5

  val factor = 1.0 / 4

  override def rate(bot: Bot, candidate: XY) = {
    import bot._


    val masterBots = factor * energy * rate(view.all(EnemyMaster), candidate, mult = 1.5)
    val slaveBots = factor * energy * rate(view.all(EnemySlave), candidate, mult = 2)

    masterBots + slaveBots
  }
}

object AvoidAnnihilation extends MoveRater {

  import Rules._

  override val name = "A"
  override val weight: Double = 0.8

  override def rate(bot: Bot, candidate: XY) = {
    import bot._

    - energy * countAround(bot, candidate, EnemyMaster)
  }
}