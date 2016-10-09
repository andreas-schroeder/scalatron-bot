
case class SingleMoveRating(move: XY, rating: Double, rater: String)

case class TotalMoveRating(move: XY, rating: Double)

trait Moving {
  this: Bot =>

  import Bot._

  var maybeMove: Option[(XY, Double)] = None

  def computeMove(): Unit =
    this.maybeMove = bestDirection(XY.Directions, moveRatings, debug)

  def bestDirection(directions: List[XY], moveRatings: Seq[MoveRater],debug: Boolean = false): Option[(XY, Double)] = {
    val singleMoveRatings: Seq[SingleMoveRating] = for {
      rater <- moveRatings
      xy <- XY.Directions
      if free(xy)
      rating = rater.weight * rater.rate(this, xy)
    } yield SingleMoveRating(xy, rating, rater.name)

    val totalMoveRatings = singleMoveRatings
      .groupBy(_.move)
      .map { case (m, ratings) => TotalMoveRating(m, ratings.map(_.rating).sum) }
      .to[List]

    val normRatedMoves: List[TotalMoveRating] = {
      val ratings: List[Double] = totalMoveRatings.map(_.rating)
      val min = ratings.min
      val range = ratings.max - min
      totalMoveRatings.map { case TotalMoveRating(m, r) => TotalMoveRating(m, (r - min) / range) }
    }

    val goodMoves = normRatedMoves.filter(_.rating >= MoveRatingThreshold).map(_.move)

    val maybeMove = heading match {
      case Some(h) if goodMoves.contains(h) => heading
      case _ => random(goodMoves)
    }
    if (debug)
      showImportantRaters(singleMoveRatings)

    maybeMove.map { m =>
      val rating = totalMoveRatings.find(_.move == m).get.rating
      m -> rating
    }
  }

  def showImportantRaters(singleMoveRatings: Seq[SingleMoveRating]): Unit = {
    val raterDiffs = singleMoveRatings.groupBy(_.rater).map { case (rater, rms) =>
      val diffs =
        for {
          a <- rms
          b <- rms if a != b
        } yield Math.abs(a.rating - b.rating)
      rater -> diffs.max
    }.to[Vector]

    val maxRaterNames = raterDiffs.sortBy(_._2).reverse.take(3).map(_._1)
    if (maxRaterNames.nonEmpty)
      add(Cmd.status(maxRaterNames.mkString(":")))
  }

  def gradient(n: Double) = {
    def hex(n: Double) = Integer.toHexString(Math.min(255, Math.max(0, n.toInt)))
    val r = hex((255 * n) / 100)
    val g = hex((255 * (100 - n)) / 100)
    val b = "00"
    s"#$r$g$b"
  }

  def free(xy: XY): Boolean = !bonks.contains(view.cellAtRelPos(xy))

  def computeSpawn(): Unit = {
    if (master && energy >= 100 && slaves < 10)
      spawn()
    if (slave && energy >= 200 && slaves < MaxSlaves)
      spawn()
  }

  def spawn(): Unit = {
    val energy = if(master) 100 else this.energy / 2

    val spawnDirs = maybeMove match {
      case Some((move, _)) => XY.Directions.filterNot(_ == move)
      case None => XY.Directions
    }

    bestDirection(spawnDirs, Bot.SlaveMoveRaters).foreach { case (dir, _) =>
      add(Cmd.spawn(dir, energy))
    }
  }

  def random[T](s: Seq[T]): Option[T] = if(s.isEmpty) None else Some(s(util.Random.nextInt(s.length)))
}
