package scala.scalanative.nir

import java.io.File
import java.nio.file.Path

object Location {
  sealed trait Location
  object NoLoc extends Location
  case class LocData(source: Path, line: Int) extends Location

  trait WithLocation {
    def loc: Location
  }
}