package hive

abstract class Cmd(val ops: String, atts: (String, Any)*) {

  override val toString = {
    val kvs = atts.map{ case (k,v) => s"$k=$v" }.mkString(",")
    s"$ops($kvs)"
  }
}

object Cmd {

  case class Status(text: String) extends Cmd("Status", "text" -> text)

  case class Move(xy: XY) extends Cmd("Move", "direction" -> xy)

  case class Set(key: String, value: Any) extends Cmd("Set", key -> value)

  case class Spawn(dir: XY, energy: Int) extends Cmd("Spawn", "direction" -> dir, Bot.Heading -> dir, "energy" -> energy)

  case class Explode(size: Int) extends Cmd("Explode", "size" -> size)

  case class MarkCell(pos: XY, color: String) extends Cmd("MarkCell", "position" -> pos, "color" -> color)

  case class DrawLine(from: XY, to: XY, color: String) extends Cmd("DrawLine", "from" -> from, "to" -> to, "color" -> color)

}
