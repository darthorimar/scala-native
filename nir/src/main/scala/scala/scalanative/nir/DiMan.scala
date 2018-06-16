package scala.scalanative.nir

import scala.collection.mutable


class DiMan(diFile: DebugInf.DIFile) {
  private val metas = mutable.HashMap.empty[DebugInf, DiLabel]
  private var id: Int = 2

  val diFileLabel = DiLabel(1)
  metas(diFile) = diFileLabel //todo move it out

  def genDiLabel(di: DebugInf): DiLabel =
    metas.getOrElseUpdate(di, {
      val label = DiLabel(id)
      id += 1
      label
    })

  def getMetas: Seq[(DebugInf, DiLabel)] =
    metas.toSeq
}