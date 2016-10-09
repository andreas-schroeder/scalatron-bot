trait Exploding {
  this: Bot =>

  def exploding: Boolean = commands.exists(_.ops == "Explode")

  def computeExplosion(): Unit = {
    val maybeExploding = bestExplosion()

    (maybeExploding, maybeMove) match {
      case (Some((radius, r1)), Some((move, r2))) =>
        if (r1 > r2) {
          maybeMove = None
          add(Cmd.explode(radius))
        }
      case (Some((radius, r1)), None) =>
        add(Cmd.explode(radius))
      case _ => ()
    }
  }

  def bestExplosion(): Option[(Int, Double)] =
    if(master)
      None
    else {
      val targets = view.all(Rules.ExplosionEnemies)

      val energyWin = for {
        blastRadius <- 2 to 10
        reachableTargets = targets.filter(_.length <= blastRadius)
        if reachableTargets.nonEmpty
      } yield blastRadius -> explosionDamage(blastRadius, reachableTargets)

      if (energyWin.isEmpty)
        None
      else
        Some(energyWin.maxBy(_._2))
    }

  def explosionDamage(blastRadius: Int, targets: Seq[XY]) = {
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
