import upickle.default.{macroRW, ReadWriter => RW}


case class TsPackage(scripts: Map[String, String] = Map.empty, dependencies: Map[String, String] = Map.empty, devDependencies: Map[String, String] = Map.empty)

object TsPackage {
  implicit val rw: RW[TsPackage] = macroRW
}