package scala.scalanative
package nir

sealed abstract class DebugInf {
  final def show: String = nir.Show(this)
  def scopeLbl: DiLabel
}

object DebugInf {
  final case object None extends DebugInf {
    override def scopeLbl: DiLabel = throw new Exception("None doesn't have a scope.")
  }
  final case class DILocation(line: Int, column: Int, scopeLbl: DiLabel) extends DebugInf
  final case class DIFile(filename: String, directory: String)           extends DebugInf {
    override def scopeLbl: DiLabel = throw new Exception("None doesn't have a scope.")
  }
}

