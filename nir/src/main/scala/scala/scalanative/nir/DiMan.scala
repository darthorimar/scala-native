package scala.scalanative.nir

import scala.collection.mutable


class DiMan(scope: String) {
  private val metas = mutable.HashMap.empty[DebugInf, DiLabel]
  private var id: Int = 1

  def genDiLabel(di: DebugInf): DiLabel =
    metas.getOrElseUpdate(di, {
      val label = DiLabel(scope, id)
      id += 1
      label
    })

  def getMetas: Seq[(DebugInf, DiLabel)] =
    metas.toSeq
}