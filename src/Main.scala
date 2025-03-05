
import upickle.default._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

object Main {

  def main(args: Array[String]): Unit = {
    // args.head == /path/to/tsconfig.json
    writeMill(Paths.get("."), args.head, "package", true, true)
  }

  def writeMill(dir: Path, tsConfigNameWithPath: String, moduleName: String, isObject: Boolean, isRootModule: Boolean = false): Unit = {
    val tsConfig = dir.resolve(tsConfigNameWithPath)
    val tsConfigDir = tsConfig.getParent
    val tsConfigText = Files.readString(tsConfig, StandardCharsets.UTF_8)
    try {
      if (tsConfig.getName(tsConfig.getNameCount - 1).toString.equalsIgnoreCase("tsconfig.json")) {
        val packageText = Files.readString(tsConfigDir.resolve("package.json"), StandardCharsets.UTF_8)
        val packageMillText = genPackageMillText(tsConfigDir, read[TsPackage](packageText), read[TsConfig](tsConfigText), moduleName, isObject, isRootModule)
        val fileName = if (isRootModule) {
          "build.mill"
        } else {
          "package.mill"
        }
        Files.writeString(tsConfigDir.resolve(fileName), packageMillText, StandardCharsets.UTF_8)
      } else {
        val packageMillText = genPackageMillText(tsConfigDir, TsPackage(), read[TsConfig](tsConfigText), moduleName, isObject, isRootModule)
        Files.writeString(tsConfigDir.resolve(s"$moduleName.mill"), packageMillText, StandardCharsets.UTF_8)
      }
    } catch {
      case ex: Throwable =>
        println(dir, tsConfigNameWithPath, moduleName, isObject)
        println(tsConfigText)
        throw ex
    }
  }

  def genPackageMillText(dir: Path, tsPackage: TsPackage, tsConfig: TsConfig, moduleName: String, isObject: Boolean, isRootModule: Boolean): String = {
    tsConfig.references.flatMap(_.values).filterNot(_.endsWith(".json")).foreach { reference =>
      val packageDir = dir.resolve(reference)
      val subModuleName = packageDir.getName(packageDir.getNameCount - 1).toString
      writeMill(packageDir, "tsconfig.json", subModuleName, true)
    }
    tsConfig.references.flatMap(_.values).filter(_.endsWith(".json")).foreach { reference =>
      val tsConfig = dir.resolve(reference)
      val tsConfigDir = tsConfig.getParent
      val parentTsConfigNameWithPath = tsConfig.getName(tsConfig.getNameCount - 1).toString
      val parentModuleName = parentTsConfigNameWithPath.replace("tsconfig.", "").replace(".json", "")
      writeMill(tsConfigDir, parentTsConfigNameWithPath, parentModuleName, true)
    }
    tsConfig.`extends`.foreach { extend =>
      val tsConfig = dir.resolve(extend)
      val tsConfigDir = tsConfig.getParent
      val parentModuleName = tsConfig.getName(tsConfig.getNameCount - 1).toString
      writeMill(tsConfigDir, parentModuleName, parentModuleName.replace("tsconfig.", "").replace(".json", ""), false)
    }
    val packageName = if (isRootModule || dir.getName(dir.getNameCount - 1).toString.equalsIgnoreCase(moduleName) || dir.getName(dir.getNameCount - 1).toString.equalsIgnoreCase(".")) {
      "build"
    } else {
      s"build.`${dir.getName(dir.getNameCount - 1).toString}`"
    }

    s"""package $packageName
       |
       |import mill._, javascriptlib._
       |
       |${if (isObject) "object" else "trait"} `$moduleName` extends ${if (isRootModule) "RootModule with" else ""} TypeScriptModule ${
      tsConfig.`extends`.map { extend =>
        val tsConfig = dir.resolve(extend)
        val parentModuleName = tsConfig.getName(tsConfig.getNameCount - 1).toString

        s"with build.`${parentModuleName.replace("tsconfig.", "").replace(".json", "")}` "
      }.getOrElse("")
    }{
       |  ${
      if (tsConfig.references.nonEmpty)
        s"""
       |  def moduleDeps = Seq(
       |    ${
          tsConfig.references.flatMap(_.values).map { reference =>
            val packageDir = dir.resolve(reference)
            val subModuleName = packageDir.getName(packageDir.getNameCount - 1).toString
            if (reference.endsWith(".json")) {
              if (packageDir.getParent == dir) {
                s"build.`${dir.getName(dir.getNameCount - 1).toString}`.`${subModuleName.replace("tsconfig.", "").replace(".json", "")}`"
              } else {
                ""
              }
            } else {
              s"build.`$subModuleName`"
            }
          }.filter(_.nonEmpty).mkString(",\n    ")
        }
       |  )
       |  """ else ""
    }
       |  ${
      if (tsConfig.`include`.nonEmpty)
        s"""
           |  def sources = Task.Source(${tsConfig.`include`.map(str => s"\"$str\"").mkString(", ")})
           |  """ else ""
    }
       |  ${
      if (tsConfig.compilerOptions.nonEmpty)
        s"""
       |  def compilerOptions = super.compilerOptions() ++ Map(
       |    ${
          tsConfig.compilerOptions.map {
            case (k, v) => s"\"$k\" -> ${valueToString(v)}"
          }.mkString(",\n    ")
        }
       |  )
       |  """ else ""
    }
       |  ${
      if (tsPackage.dependencies.nonEmpty)
        s"""
           |  def npmDeps = super.npmDeps() + Seq(
           |    ${tsPackage.dependencies.map { case (k, v) => s"\"$k@$v\"" }.mkString(",\n    ")}
           |  )
           |  """ else ""
    }
       |  ${
      if (tsPackage.devDependencies.nonEmpty)
        s"""
           |  def npmDevDeps = super.npmDevDeps() + Seq(
           |    ${tsPackage.devDependencies.map { case (k, v) => s"\"$k@$v\"" }.mkString(",\n    ")}
           |  )
           |  """ else ""
    }
       |}""".stripMargin
  }

  def valueToString(value: ujson.Value): String = value match {
    case ujson.Str(v) => s"ujson.Str(\"$v\")"
    case ujson.Obj(v) => s"ujson.Obj(${
      v.value.map {
        case (kk, vv) => s"\"$kk\" -> ${valueToString(vv)}"
      }.mkString(",\n    ")
    })"
    case ujson.Arr(v) => s"ujson.Arr.from(Seq(${v.value.map(valueToString).mkString(",\n    ")}))"
    case ujson.Num(v) => s"ujson.Num($v)"
    case b: ujson.Bool => if (b.value) "ujson.True" else "ujson.False"
    case ujson.Null => "null"
  }

}
