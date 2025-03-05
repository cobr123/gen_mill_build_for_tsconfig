import upickle.default.{macroRW, ReadWriter => RW}


case class TsPackage(scripts: Map[String, String] = Map.empty, dependencies: Map[String, String] = Map.empty, devDependencies: Map[String, String] = Map.empty) {
  def getNpmDeps(pad: Int): String = {
    if (dependencies.nonEmpty) {
      s"""
         |${" " * pad}override def npmDeps: T[Seq[String]] = super.npmDeps() ++ Seq(
         |${" " * pad * 2}${dependencies.map { case (k, v) => s"\"$k@$v\"" }.mkString(s",\n${" " * pad * 2}")}
         |${" " * pad})
         |""".stripMargin
    } else {
      ""
    }
  }

  def getNpmDevDeps(pad: Int): String = {
    if (devDependencies.nonEmpty) {
      s"""
         |${" " * pad}override def npmDevDeps: T[Seq[String]] = super.npmDevDeps() ++ Seq(
         |${" " * pad * 2}${devDependencies.map { case (k, v) => s"\"$k@$v\"" }.mkString(s",\n${" " * pad * 2}")}
         |${" " * pad})
         |""".stripMargin
    } else {
      ""
    }
  }
}

object TsPackage {
  implicit val rw: RW[TsPackage] = macroRW
}