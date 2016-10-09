case class Cmd(ops: String, atts: (String, Any)*) {

  override val toString = {
    val kvs = atts.map{ case (k,v) => s"$k=$v" }.mkString(",")
    s"$ops($kvs)"
  }
}

object Cmd {

  def status(text: String) = Cmd("Status", "text" -> text)

  def move(xy: XY) = Cmd("Move", "direction" -> xy)

  def set(key: String, value: Any) = Cmd("Set", key -> value)

  def spawn(dir: XY, energy: Int) = Cmd("Spawn", "direction" -> dir, Bot.Heading -> dir, "energy" -> energy)

  def explode(size: Int) = Cmd("Explode", "size" -> size)

  def markCell(pos: XY, color: String) = Cmd("MarkCell", "position" -> pos, "color" -> color)

  def drawLine(from: XY, to: XY, color: String) = Cmd("DrawLine", "from" -> from, "to" -> to, "color" -> color)

}
