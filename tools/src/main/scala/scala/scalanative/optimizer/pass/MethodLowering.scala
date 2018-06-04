package scala.scalanative
package optimizer
package pass

import scala.collection.mutable
import analysis.ClassHierarchy._
import analysis.ClassHierarchyExtractors._
import nir._, Inst.Let

/** Translates high-level object-oriented method calls into
 *  low-level dispatch based on vtables for classes
 *  and dispatch tables for interfaces.
 */
class MethodLowering(implicit top: Top) extends Pass {
  override def onInsts(insts: Seq[Inst]) = {
    val buf = new nir.Buffer
    import buf._

    insts.foreach {
      case Let(n, Op.Method(obj, MethodRef(cls: Class, meth)), loc)
          if meth.isVirtual =>
        val vindex  = cls.vtable.index(meth)
        val typeptr = let(Op.Load(Type.Ptr, obj), loc)
        val methptrptr = let(
          Op.Elem(cls.rtti.struct,
                  typeptr,
                  Seq(Val.Int(0),
                      Val.Int(5), // index of vtable in type struct
                      Val.Int(vindex))),
                  loc)

        let(n, Op.Load(Type.Ptr, methptrptr), loc)

      case Let(n, Op.Method(obj, MethodRef(_: Class, meth)), loc) if meth.isStatic =>
        let(n, Op.Copy(Val.Global(meth.name, Type.Ptr)), loc)

      case Let(n, Op.Method(obj, MethodRef(trt: Trait, meth)), loc) =>
        val sig = meth.name.id
        if (top.tables.traitInlineSigs.contains(sig)) {
          let(n, Op.Copy(top.tables.traitInlineSigs(sig)), loc)
        } else {
          val sigid   = top.tables.traitDispatchSigs(meth.name.id)
          val typeptr = let(Op.Load(Type.Ptr, obj), loc)
          val idptr =
            let(Op.Elem(Rt.Type, typeptr, Seq(Val.Int(0), Val.Int(0))), loc)
          val id = let(Op.Load(Type.Int, idptr), loc)
          val rowptr = let(
            Op.Elem(Type.Ptr,
                    top.tables.dispatchVal,
                    Seq(Val.Int(top.tables.dispatchOffset(sigid)))),
                    loc)
          val methptrptr =
            let(Op.Elem(Type.Ptr, rowptr, Seq(id)), loc)
          let(n, Op.Load(Type.Ptr, methptrptr), loc)
        }

      case inst =>
        buf += inst
    }

    buf.toSeq
  }
}

object MethodLowering extends PassCompanion {
  override def apply(config: build.Config, top: Top) =
    new MethodLowering()(top)
}
