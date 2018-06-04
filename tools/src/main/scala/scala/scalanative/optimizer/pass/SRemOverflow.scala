package scala.scalanative.optimizer.pass

import scala.scalanative.nir
import scala.scalanative.nir._
import scala.scalanative.optimizer.analysis.ClassHierarchy.Top
import scala.scalanative.optimizer.{Pass, PassCompanion}
import scala.scalanative.build.Config

/**
 * Detects taking remainder for division by -1 and replaces it by division by 1 which can't overflow.
 *
 *
 * We implement '%' (remainder) with LLVM's 'srem' and it can overflow for cases:
 * Int.MinValue % -1
 * Long.MinValue % -1
 * E.g. On x86_64 'srem' might get translated to 'idiv' which computes both quotient and remainder at once
 * and quotient can overflow.
 */
class SRemOverflow(implicit top: Top) extends Pass {

  override def onInsts(insts: Seq[Inst]): Seq[Inst] = {
    val buf = new nir.Buffer
    import buf._

    insts.foreach {
      case Inst.Let(name,
                    sremBin @ Op.Bin(Bin.Srem, intType: Type.I, _, divisor),
                    loc)
          if intType.width == 32 || intType.width == 64 =>
        val safeDivisor         = Val.Local(fresh(), intType)
        val thenL, elseL, contL = fresh()

        val isPossibleOverflow =
          let(Op.Comp(Comp.Ieq, intType, divisor, Val.Int(-1)), loc)
        branch(isPossibleOverflow, Next(thenL), Next(elseL), loc)

        label(thenL, loc)
        jump(contL, Seq(Val.Int(1)), loc)

        label(elseL, loc)
        jump(contL, Seq(divisor), loc)

        label(contL, Seq(safeDivisor), loc)

        let(name, sremBin.copy(r = safeDivisor), loc)

      case other => buf += other
    }

    buf.toSeq
  }
}

object SRemOverflow extends PassCompanion {

  def apply(config: Config, top: Top): Pass = new SRemOverflow()(top)

}
