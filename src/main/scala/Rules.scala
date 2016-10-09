object Rules {

  val Master      = 'M'
  val EnemyMaster = 'm'

  val Slave       = 'S'
  val EnemySlave  = 's'

  val Snorg       = 'b'
  val Fluppet     = 'B'

  val Toxifera    = 'p'
  val Zugar       = 'P'

  val Wall        = 'W'
  val Unknown     = '?'

  val ExplosionEnemies = Set(EnemyMaster, EnemySlave, Snorg)

  val SlaveBonks  = Set(Wall, Snorg, EnemySlave, Slave, EnemyMaster)
  val MasterBonks = Set(Wall, Snorg, EnemyMaster)
}
