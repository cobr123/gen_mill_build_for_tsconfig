
import upickle.default._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

object Main {

  def main(args: Array[String]): Unit = {
    // args.head == /path/to/tsconfig.json
    val tsConfig = Paths.get(args.head)
    writeMill(tsConfig.getParent, tsConfig, "package", true, true)
  }

  def writeMill(projectDir: Path, tsConfigPath: Path, moduleName: String, isObject: Boolean, isRootModule: Boolean = false): Unit = {
    val tsConfigDir = tsConfigPath.getParent
    val tsConfigText = Files.readString(tsConfigPath, StandardCharsets.UTF_8)
    try {
      if (tsConfigPath.getName(tsConfigPath.getNameCount - 1).toString.equalsIgnoreCase("tsconfig.json")) {
        val packageText = Files.readString(tsConfigDir.resolve("package.json"), StandardCharsets.UTF_8)
        val packageMillText = genPackageMillText(projectDir, tsConfigDir, read[TsPackage](packageText), read[TsConfig](tsConfigText), moduleName, isObject, isRootModule)
        val fileName = if (isRootModule) {
          "build.mill"
        } else {
          "package.mill"
        }
        Files.writeString(tsConfigDir.resolve(fileName), packageMillText, StandardCharsets.UTF_8)
      } else {
        val packageMillText = genPackageMillText(projectDir, tsConfigDir, TsPackage(), read[TsConfig](tsConfigText), moduleName, isObject, isRootModule)
        Files.writeString(tsConfigDir.resolve(s"$moduleName.mill"), packageMillText, StandardCharsets.UTF_8)
      }
    } catch {
      case ex: Throwable =>
        println(tsConfigPath, moduleName, isObject)
        println(tsConfigText)
        throw ex
    }
  }

  def genPackageMillText(projectDir: Path, tsConfigPath: Path, tsPackage: TsPackage, tsConfig: TsConfig, moduleName: String, isObject: Boolean, isRootModule: Boolean): String = {
    tsConfig.references.flatMap(_.values).filterNot(_.endsWith(".json")).foreach { reference =>
      val packageDir = tsConfigPath.resolve(reference)
      writeMill(projectDir, packageDir.resolve("tsconfig.json"), "package", true)
    }
    tsConfig.references.flatMap(_.values).filter(_.endsWith(".json")).foreach { reference =>
      val tsConfig = tsConfigPath.resolve(reference)
      val parentModuleName = getModuleName(tsConfig)
      writeMill(projectDir, tsConfig, parentModuleName, true)
    }
    tsConfig.`extends`.foreach { extend =>
      val tsConfig = tsConfigPath.resolve(extend)
      val parentModuleName = getModuleName(tsConfig)
      writeMill(projectDir, tsConfig, parentModuleName, false)
    }
    val packageName = if (isRootModule || getModuleName(tsConfigPath).equalsIgnoreCase(".")) {
      "build\nimport $packages._"
    } else {
      s"build.${getPackagePath(projectDir, tsConfigPath)}"
    }

    s"""package $packageName
       |
       |import mill._, javascriptlib._
       |
       |${if (isObject) "object" else "trait"} `$moduleName` extends ${if (isRootModule) "RootModule" else "TypeScriptModule"}  ${
      tsConfig.`extends`.map { extend =>
        val tsConfig = tsConfigPath.resolve(extend)
        if (extend.endsWith(".json")) {
          val parentModuleName = getModuleName(tsConfig)
          s"with build.`$parentModuleName` "
        } else {
          val parentModuleName = getPackagePath(projectDir, tsConfig)
          s"with build.$parentModuleName "
        }
      }.getOrElse("")
    }{
       |${tsConfig.getModuleDeps(projectDir, tsConfigPath, pad)}
       |${tsConfig.getSources(pad)}
       |${tsConfig.getCompilerOptions(pad)}
       |${tsPackage.getNpmDeps(pad)}
       |${tsPackage.getNpmDevDeps(pad)}
       |}""".stripMargin.replaceAll("\n{3,}","\n")
  }

  val pad = 2

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

  def getPackagePath(projectDir: Path, tsConfigPath: Path): String = {
    tsConfigPath.normalize().toString
      .replace(projectDir.normalize().toString, "")
      .dropWhile(_ == '/')
      .split('/')
      .mkString("`", "`.`", "`")
  }

  def getModuleName(tsConfigPath: Path): String = {
    tsConfigPath.getName(tsConfigPath.getNameCount - 1).toString
      .replace("tsconfig.", "").replace(".json", "")
  }
}
