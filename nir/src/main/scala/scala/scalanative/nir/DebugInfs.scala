package scala.scalanative.nir

import scala.scalanative.nir

sealed abstract class DebugInf {
  final def show: String = nir.Show(this)
}

final case class DIFile(filename: String, directory: String)        extends DebugInf
final case class DILocation(line: Int, column: Int, scope: DIScope) extends DebugInf

sealed abstract class DIScope   extends DebugInf
final case class DISubprogram() extends DIScope

object DebugInf {
  val defaultScope = DISubprogram() //todo use normal location
}

