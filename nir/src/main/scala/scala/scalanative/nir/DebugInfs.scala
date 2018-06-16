package scala.scalanative
package nir

sealed abstract class DebugInf {
  final def show: String = nir.Show(this)
  def scope: DiLabel
}

object DebugInf {
  final case class DILocation(line: Int, column: Int, scope: DiLabel)    extends DebugInf
  final case class DIFile(filename: String, directory: String)           extends DebugInf {
    override def scope: DiLabel = throw new Exception("None doesn't have a scope.")
  }
  final case class DISubprogram(name: String, file: DiLabel, scope: DiLabel) extends DebugInf
}

