package scala.scalanative
package optimizer
package inject

import scala.collection.mutable.Buffer
import analysis.ClassHierarchy.Top
import nir._

/** Injects scalanative_class_has_trait and
 *  scalanative_trait_has_trait intrinsics.
 */
class HasTrait(implicit top: Top) extends Inject {
  import HasTrait._
  import top.tables

  def classHasTrait: Defn.Define = {
    implicit val fresh   = Fresh()
    val classid, traitid = Val.Local(fresh(), Type.Int)
    val boolptr          = Val.Local(fresh(), Type.Ptr)
    val result           = Val.Local(fresh(), Type.Bool)

    Defn.Define(
      Attrs(isExtern = true, inline = Attr.AlwaysInline),
      ClassHasTraitName,
      ClassHasTraitSig,
      Seq(
        Inst.Label(fresh(), Seq(classid, traitid), Location.NoLoc),
        Inst.Let(boolptr.name,
                 Op.Elem(tables.classHasTraitTy,
                         tables.classHasTraitVal,
                         Seq(Val.Int(0), classid, traitid)),
                 Location.NoLoc),
        Inst.Let(result.name, Op.Load(Type.Bool, boolptr), Location.NoLoc),
        Inst.Ret(result, Location.NoLoc)
      ),
      Location.NoLoc //todo location?
    )
  }

  def traitHasTrait: Defn.Define = {
    implicit val fresh  = Fresh()
    val leftid, rightid = Val.Local(fresh(), Type.Int)
    val boolptr         = Val.Local(fresh(), Type.Ptr)
    val result          = Val.Local(fresh(), Type.Bool)
    val loc = Location.NoLoc //todo location?
    Defn.Define(
      Attrs(isExtern = true, inline = Attr.AlwaysInline),
      TraitHasTraitName,
      TraitHasTraitSig,
      Seq(
        Inst.Label(fresh(), Seq(leftid, rightid), loc),
        Inst.Let(boolptr.name,
                 Op.Elem(tables.traitHasTraitTy,
                         tables.traitHasTraitVal,
                         Seq(Val.Int(0), leftid, rightid)),
                 loc),
        Inst.Let(result.name, Op.Load(Type.Bool, boolptr), loc),
        Inst.Ret(result, loc)
      ),
      loc
    )
  }

  override def apply(buf: Buffer[Defn]): Unit = {
    buf += classHasTrait
    buf += traitHasTrait
  }
}

object HasTrait extends InjectCompanion {
  val ClassHasTraitName =
    Global.Member(Global.Top("__extern"), "extern.__check_class_has_trait")
  val ClassHasTraitSig = Type.Function(Seq(Type.Int, Type.Int), Type.Bool)

  val TraitHasTraitName =
    Global.Member(Global.Top("__extern"), "extern.__check_trait_has_trait")
  val TraitHasTraitSig = Type.Function(Seq(Type.Int, Type.Int), Type.Bool)

  override def apply(config: build.Config, top: Top) =
    new HasTrait()(top)
}
