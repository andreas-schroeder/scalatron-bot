package hive

class ControlFunction {

  def respond(input: String): String =
    try {
      val (opcode, paramMap) = CommandParser(input)

      if( opcode == "React" ) {
        val gen = paramMap("generation").toInt
        if(gen == 0) {
          new MasterBot(paramMap).run()
        } else
          new SlaveBot(paramMap).run()
      } else ""
    }
  catch {
    case e : Throwable =>
      e.printStackTrace()
      ""
  }
}
