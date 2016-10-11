package hive

import Rules._

object Weights {
  val ExplodeR = 1.0

  val DamageAvoidR = 0.8

  val SpawnR = 1.0
}

object ExplodeR extends Reactor {
  override def name: String = "E"

  override def weight: Double = Weights.ExplodeR

  val damageFactor = 0.9
  val energyFactor = 1.1

  override def react(bot: Bot, moves: Seq[XY]): Seq[React] = {
    import bot._

    val certainty = Bot.ExplodeCertainty

    def adjustRating(rating: Double) =
      weight * (- energyFactor * energy + rating * certainty * damageFactor)

    val explodeNow = bestExplosion(energy, view)
      .map { case (radius, rating) => React(Cmd.Explode(radius), name, adjustRating(rating)) }

    val explodeLater =
      for {
        move <- moves
        explosion <- bestExplosion(energy, view, offset = move)
        rating = - energyFactor * energy + explosion._2
      } yield (move, rating)

    val hunt =
      for {
        move <- moves
        if !explodeLater.exists(_._1 == move)
        masterBots = blastEnergy(energy, 3) * rate(view.all(EnemyMaster), move, mult = 1.5, certainty)
        slaveBots = blastEnergy(energy, 3) * rate(view.all(EnemySlave), move, mult = 2, certainty)
      } yield (move, masterBots + slaveBots)

    val moveReacts = (explodeLater ++ hunt)
      .map { case (move, rating) => React(Cmd.Move(move), name, adjustRating(rating)) }

    moveReacts ++ explodeNow
  }

  def bestExplosion(energy: Int, view: View, offset: XY = XY.Zero): Option[(Int, Double)] = {
    val targets = view.all(Rules.ExplosionEnemies).map(_ - offset)

    val energyWin = for {
      blastRadius <- 2 to 6
      reachableTargets = targets.filter(_.length + 1 <= blastRadius)
      if reachableTargets.nonEmpty
    } yield blastRadius -> explosionDamage(energy, blastRadius, reachableTargets)

    if (energyWin.isEmpty)
      None
    else
      Some(energyWin.maxBy(_._2))
  }

  def blastEnergy(energy: Int, blastRadius: Int): Double = {
    val blastArea = blastRadius * blastRadius * Math.PI
    val energyPerArea = energy / blastArea
    -200 * energyPerArea
  }

  def explosionDamage(energy: Int, blastRadius: Int, targets: Seq[XY]) = {
    val damageAtCenter = blastEnergy(energy, blastRadius)

    def damage(target: XY): Double = {
      val distanceFromCenter = target.length
      Math.min(400, damageAtCenter * (1  - (distanceFromCenter / blastRadius)))
    }

    targets.map(damage).sum
  }
}

object SpawnR extends Reactor {

  override def name: String = "S"

  override def weight: Double = Weights.SpawnR

  override def react(bot: Bot, moves: Seq[XY]): Seq[React] = {
    import bot._
    if (master && energy >= 100 && slaves < Bot.MaxSlaves)
      doSpawn(bot, moves)
    else if (slave && energy >= 175 && slaves < Bot.MaxSlaves)
      doSpawn(bot, moves)
    else
      Seq.empty
  }

  val moveFilter: Cmd => Boolean = c => c.isInstanceOf[Cmd.Move]

  def doSpawn(bot: Bot, moves: Seq[XY]): Seq[React] = {
    import bot._

    val possibleDirs = moves.filter(dir => !bot.maybeMove.contains(dir))
    val maybeDir = Reactor.best(bot, Bot.SlaveReactors, possibleDirs, onlyMoves = true)

    maybeDir.map { case React(Cmd.Move(pos), _, _) =>
      val bots = (bot.energy / 100) + (if (bot.slave && bot.energy % 100 > 75) 1 else 0)
      val energy = if(master) {
        if(bot.energy >= 300) 100 * (bots / 3 * 2) else 100
      } else {
        if(bot.energy >= 200) 100 * (bots / 2) else 100
      }

      React(Cmd.Spawn(pos, energy), name, 500)
    }.to[List]
  }
}

object DamageAvoidR extends MoveReactor {

  override val name = "D"

  override val weight: Double = Weights.DamageAvoidR

  override def ratingFor(bot: Bot, candidate: XY) = {
    import bot._

    val snorgs = if (master) -150 * rate(view.all(Snorg), candidate, mult = 1.5) else 0
    val snorgNeighbor = {
      val snorgSpeed = beastSpeed(bot)
      -150 * (snorgSpeed + (1 - snorgSpeed) * Bot.MoveCertainty)
    }

    val cell = view.cellAtRelPos(candidate) match {
      case Toxifera => -100
      case _ =>
        countAround(bot, candidate, Snorg) * snorgNeighbor
    }

    snorgs + cell
  }
}



object CornerAvoidR extends MoveReactor {

  override val name = "W"
  override val weight: Double = 1.0

  override def ratingFor(bot: Bot, candidate: XY) = {
    import bot._

    -10 * rate(view.all(Wall), candidate)
  }
}

object MasterAvoidR extends MoveReactor {

  override val name = "M"
  override val weight: Double = 1.0

  override def ratingFor(bot: Bot, candidate: XY) = {
    import bot._

    -30 * rate(view.all(EnemyMaster), candidate)
  }
}

object SpreadR extends MoveReactor {

  override val name = "S"
  override val weight: Double = 1.0

  override def ratingFor(bot: Bot, candidate: XY) = {
    import bot._

    val avoid = -10 * rate(view.all(Slave), candidate)
    val cell = -20 * countAround(bot, candidate, Slave)

    avoid + cell
  }
}


object AnnihilationAvoidR extends MoveReactor {

  override val name = "A"
  override val weight: Double = 1.0

  override def ratingFor(bot: Bot, candidate: XY) = {
    import bot._

    - (energy + 350) * countAround(bot, candidate, EnemyMaster)
  }
}