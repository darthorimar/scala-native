package scala.scalanative.nir

import scala.scalanative.nir

final case class DiLabel(id: Int) {
  final def show: String = nir.Show(this)
}