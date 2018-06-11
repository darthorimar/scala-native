package scala.scalanative.nir

import java.io.File
import java.nio.file.Path

sealed abstract class  Location

object Location {
  object None                       extends Location
  case class LocLabel(lbl: DiLabel) extends Location
}

trait WithLocation {
  def loc: Location
}
