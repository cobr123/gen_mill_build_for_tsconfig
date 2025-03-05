import upickle.default.{macroRW, ReadWriter => RW}

import java.nio.file.Path


case class TsConfig(`extends`: Option[String] = None, `include`: List[String] = List.empty, compilerOptions: Map[String, ujson.Value] = Map.empty, references: List[Map[String, String]] = List.empty) {

  def getSources(pad: Int): String = {
    if (`include`.nonEmpty) {
      s"""
         |${" " * pad}override def sources: Target[Seq[PathRef]] = Task.Source(${`include`.map(str => s"\"$str\"").mkString(", ")})
         |""".stripMargin
    } else {
      ""
    }
  }

  def getCompilerOptions(pad: Int): String = {
    if (compilerOptions.nonEmpty) {
      s"""
         |${" " * pad}override def compilerOptions: T[Map[String, ujson.Value]] = super.compilerOptions() ++ Map(
         |${" " * pad * 2}${
        compilerOptions.map {
          case (k, v) => s"\"$k\" -> ${Main.valueToString(v)}"
        }.mkString(s",\n${" " * pad * 2}")
      }
         |${" " * pad})
         |""".stripMargin
    } else {
      ""
    }
  }

  def getModuleDeps(projectDir: Path, tsConfigPath: Path, pad: Int): String = {
    val res =
      references.flatMap(_.values).map { reference =>
        val packageDir = tsConfigPath.resolve(reference)
        val subModuleName = Main.getPackagePath(projectDir, packageDir)
        if (reference.endsWith(".json")) {
          if (packageDir.getParent == tsConfigPath.getParent) {
            s"build.$subModuleName"
          } else {
            ""
          }
        } else {
          s"build.$subModuleName"
        }
      }.filter(_.nonEmpty).mkString(s",\n${" " * pad * 2}")

    if (res.nonEmpty) {
      s"""
         |${" " * pad}override def moduleDeps: Seq[TypeScriptModule] = Seq(
         |${" " * pad * 2}$res
         |${" " * pad})
         |""".stripMargin
    } else {
      ""
    }
  }
}

object TsConfig {
  implicit val rw: RW[TsConfig] = macroRW
}