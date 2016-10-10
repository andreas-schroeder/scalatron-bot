package hive

object Reactor {

  def compatibles(bot: Bot, reactors: Seq[Reactor], debug: Boolean = false): Seq[React] = {
    val allReacts = all(bot, reactors, debug)
    val winners = allReacts.groupBy(_.cmd.ops).flatMap { case (ops, reacts) =>
        if(ops == "Move") {
          select(reacts, bot.heading.map(h => Cmd.Move(h)))
        } else {
          select(reacts)
        }
    }.to[List]

    val maybeExplode = winners.find(_.cmd.isInstanceOf[Cmd.Explode])
    maybeExplode match {
      case Some(explode) =>
        if(winners.forall(r => r.rating <= explode.rating))
          Seq(explode)
        else
          winners.filterNot(_ == explode)
      case None => winners
    }
  }

  def all(bot: Bot, reactors: Seq[Reactor], debug: Boolean = false): Seq[React] = {
    val reacts =
      for {
        reactor <- reactors
        react <- reactor.react(bot)
      } yield react

    normalize(sum(reacts))
  }

  def best(bot: Bot, reactors: Seq[Reactor], predicate: Cmd => Boolean = _ => true, debug: Boolean = false): Option[React] = {
    val allReacts = all(bot, reactors, debug).filter(r => predicate(r.cmd))
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

  def react(bot: Bot): Seq[React]

  def possibleMoves(bot: Bot) = XY.Directions.filter(bot.free)

  def beastSpeed(bot: Bot): Double = if (bot.master) 0.75 else 0.5

  def countAround(bot: Bot, candidate: XY, c: Char): Int =
    candidate.around.count(pos => bot.view.cellAtRelPos(pos) == c)

  def rate(pos: Seq[XY], m: XY, mult: Double = 1.0, certainty: Double = Bot.MoveCertainty): Double =
    pos.map(p => Math.pow(certainty, mult * (p - m).length)).sum
}

trait MoveReactor extends Reactor {

  def react(bot: Bot): Seq[React] = {
    for {
      move <- possibleMoves(bot)
      rating = weight * ratingFor(bot, move)
    } yield React(Cmd.Move(move), name, rating)
  }

  def ratingFor(bot: Bot, candidate: XY): Double
}

case class React(cmd: Cmd, reactor: String, rating: Double)
