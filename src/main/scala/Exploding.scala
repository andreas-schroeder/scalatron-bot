trait Exploding {
  this: Bot =>

  val weight: Double = 0.6

  import Exploding._

  def exploding: Boolean = commands.exists(_.ops == "Explode")

  def computeExplosion(): Unit =
    if(slave) {
      val maybeExploding = bestExplosion(energy, view)

      (maybeExploding, maybeMove) match {
        case (Some((radius, r1)), Some((move, r2))) =>
          if (weight * Bot.ExplodeCertainty * r1 > r2) {
            maybeMove = None
            add(Cmd.explode(radius))
          }
        case (Some((radius, r1)), None) =>
          add(Cmd.explode(radius))
        case _ => ()
      }
    }
}

object Exploding {
  def bestExplosion(energy: Int, view: View, offset: XY = XY.Zero): Option[(Int, Double)] = {
    val targets = view.all(Rules.ExplosionEnemies).map(_ - offset)

    val energyWin = for {
      blastRadius <- 2 to 6
      reachableTargets = targets.filter(_.length <= blastRadius)
      if reachableTargets.nonEmpty
    } yield blastRadius -> explosionDamage(energy, blastRadius, reachableTargets)

    if (energyWin.isEmpty)
      None
    else
      Some(energyWin.maxBy(_._2))
  }

  def explosionDamage(energy: Int, blastRadius: Int, targets: Seq[XY]) = {
    val blastArea = blastRadius * blastRadius * Math.PI
    val energyPerArea = energy / blastArea
    val damageAtCenter = -200 * energyPerArea

    def damage(target: XY): Double = {
      val distanceFromCenter = target.length
      damageAtCenter * (1  - (distanceFromCenter / blastRadius))
    }

    targets.map(damage).sum
  }
}
