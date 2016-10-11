package hive
import Rules._

object HarvestR extends Reactor {

  override val name = "H"
  override val weight: Double = 1.0

  override def react(bot: Bot, moves: Seq[XY]): Seq[React] = {
    import bot._

    val ownSpeed = if(bot.master) 1.0 else 2.0

    val master = view.all(Master).filter(_ == XY.Zero)
    val slaves = view.all(Slave).filter(_ == XY.Zero)

    def closer(target: XY, speed: Double, others: Seq[XY]) =
      others.exists{ pos => (target - pos).length / speed < target.length / ownSpeed }

    def otherCloser(target: XY) = closer(target, 2.0, slaves) || closer(target, 1.0, master)

    val huntableZugars = view.all(Zugar).filterNot(otherCloser)
    val huntableFluppets = view.all(Fluppet).filter(otherCloser)

    for {
      move <- moves
      zugars   = 100 * rate(huntableZugars, move)
      fluppets = 200 * rate(huntableFluppets, move, mult = 1 + beastSpeed(bot))
      rating = weight * (zugars + fluppets)
    } yield React(Cmd.Move(move), name, rating)
  }
}
