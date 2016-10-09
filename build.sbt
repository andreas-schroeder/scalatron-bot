name := "andis-bot"

version := "1.0"

scalaVersion := "2.11.8"

assemblyJarName := "ScalatronBot.jar"

// relative path to local scalatron installation
target in scalatron := baseDirectory.value / "../scalatron/dist"

lazy val scalatron = taskKey[Unit]("Copy bot to local scalatron server")

scalatron := {
  val sourceFile   = assembly.value // execute assembly task.
  val botName      = name.value
  val scalatronDir = (target in scalatron).value.getCanonicalFile

  val botsDir    = scalatronDir / "bots"
  val targetDir  = botsDir / botName
  val targetFile = targetDir / "ScalatronBot.jar"

  streams.value.log.info(s"Publishing bot '$botName' to $botsDir ...")

  if(!botsDir.exists())
    sys.error(s"Scalatron bot directory path $botsDir does not exist")
  if(!targetDir.exists && !targetDir.mkdirs())
    sys.error(s"Unable to create directory $targetDir")
  if(targetFile.exists && !targetFile.delete())
    sys.error(s"Unable to delete target file $targetFile")

  import java.nio.file.Files
  Files.copy(sourceFile.toPath, targetFile.toPath)

  streams.value.log.info(s"Publishing bot '$botName' to $botsDir completed.")
}