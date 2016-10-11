package hive

object Reactor {

  def possibleMoves(bot: Bot) = XY.Directions.filter(bot.free)

  def process(bot: Bot, debug: Boolean = false): Unit = {
    val moves = possibleMoves(bot).filter(resolveCollision(bot))
    val allReacts = all(bot, bot.reactors, moves, debug = debug)

    val maybeMove = select(allReacts.filter(_.cmd.isInstanceOf[Cmd.Move]), bot.heading.map(h => Cmd.Move(h)))
    val maybeExplode = select(allReacts.filter(_.cmd.isInstanceOf[Cmd.Explode]))

    val options = maybeExplode.to[List] ++ maybeMove.to[List]

    if(options.nonEmpty) {
      val bestOption = (maybeExplode.to[List] ++ maybeMove.to[List]).maxBy(_.rating)
      bot.add(bestOption.cmd)
    }

    if(!bot.exploding)
      SpawnR.react(bot, moves).foreach(r => bot.add(r.cmd))
  }

  def resolveCollision(bot: Bot)(move: XY): Boolean = {
    if(bot.collision.contains(move)) {
      println("collision!")
      val slavesAroundInOrder = move.around.filter(p => bot.view.cellAtRelPos(p) == Rules.Slave)
      slavesAroundInOrder.head == move.negate // allow move only if this bot is the first in the list.
    } else {
      true
    }
  }

  def all(bot: Bot, reactors: Seq[Reactor], moves: Seq[XY], onlyMoves: Boolean = false, debug: Boolean = false): Seq[React] = {
    val reacts =
      for {
        reactor <- reactors
        react <- reactor.react(bot, moves)
        if !onlyMoves || react.cmd.isInstanceOf[Cmd.Move]
      } yield react

    normalize(sum(reacts))
  }

  def best(bot: Bot, reactors: Seq[Reactor], moves: Seq[XY], onlyMoves: Boolean = false, debug: Boolean = false): Option[React] = {
    val allReacts = all(bot, reactors, moves, onlyMoves, debug)
    select(allReacts)
  }

  def sum(reacts: Seq[React]): Seq[React] =
    reacts
      .groupBy(_.cmd)
      .map { case (cmd, ratings) => React(cmd, "", ratings.map(_.rating).sum) }
      .to[Vector]

  def normalize(reacts: Seq[React]): Seq[React] =
    if(reacts.isEmpty) {
      reacts
    } else {
      val ratings = reacts.map(_.rating)
      val min = ratings.min
      val range = ratings.max - min
      if (range == 0)
        reacts
      else
        reacts.map { case React(cmd, n, r) => React(cmd, n, (r - min) / range) }
    }

  def random[T](s: Seq[T]): Option[T] = if(s.isEmpty) None else Some(s(util.Random.nextInt(s.length)))

  def select(reacts: Seq[React], preferred: Option[Cmd] = None): Option[React] = {
    val candidateReacts = reacts.filter(_.rating >= Bot.WinnerRatingThreshold)

    preferred match {
      case Some(c) if candidateReacts.exists(_.cmd == c) =>
        candidateReacts.find(_.cmd == c)
      case _ => random(candidateReacts)
    }
  }

  def explainWinning(reacts: Seq[React], maxReactors: Int = 2): String = {
    val raterDiffs = reacts.groupBy(_.reactor).map { case (reactor, rms) =>
      val diffs =
        for {
          a <- rms
          b <- rms if a != b
        } yield Math.abs(a.rating - b.rating)
      reactor -> diffs.max
    }.to[Vector]

    val maxRaterNames = raterDiffs.sortBy(_._2).reverse.take(maxReactors).map(_._1)
    maxRaterNames.mkString(":")
  }
}

trait Reactor {

  def name:   String

  def weight: Double

  def react(bot: Bot, moves: Seq[XY]): Seq[React]

  def beastSpeed(bot: Bot): Double = if (bot.master) 0.75 else 0.5

  def countAround(bot: Bot, candidate: XY, c: Char): Int =
    candidate.around.count(pos => bot.view.cellAtRelPos(pos) == c)

  def rate(pos: Seq[XY], m: XY, mult: Double = 1.0, certainty: Double = Bot.MoveCertainty): Double =
    pos.map(p => Math.pow(certainty, mult * (p - m).length)).sum
}

trait MoveReactor extends Reactor {

  def react(bot: Bot, moves: Seq[XY]): Seq[React] = {
    for {
      move <- moves
      rating = weight * ratingFor(bot, move)
    } yield React(Cmd.Move(move), name, rating)
  }

  def ratingFor(bot: Bot, candidate: XY): Double
}

case class React(cmd: Cmd, reactor: String, rating: Double)
