import hive._

class ControlFunctionFactory {
  def create: (String) => String = new ControlFunction().respond _
}


