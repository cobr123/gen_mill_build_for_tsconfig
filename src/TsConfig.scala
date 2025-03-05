import upickle.default.{macroRW, ReadWriter => RW}


case class TsConfig(`extends`: Option[String] = None, `include`: List[String] = List.empty, compilerOptions: Map[String, ujson.Value] = Map.empty, references: List[Map[String, String]] = List.empty)

object TsConfig {
  implicit val rw: RW[TsConfig] = macroRW
}