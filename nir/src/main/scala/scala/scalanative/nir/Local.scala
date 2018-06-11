package scala.scalanative
package nir

final case class Local(id: Int) extends AnyVal {
  final def show: String = nir.Show(this)
}

final case class DiLabel(id: Int) {
  final def show: String = nir.Show(this)
}
