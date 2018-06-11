package scala.scalanative
package optimizer
package pass

import analysis.ClassHierarchy.Top

import nir._
import Inst.Let
import Bin._
import Comp._

import scala.None

/** Simplifies single instruction patterns */
class PartialEvaluation extends Pass {
  import PartialEvaluation._
  import ConstantFolding._

  override def onInst(inst: Inst): Inst = inst match {

    /* Iadd */
    case Let(n, Op.Bin(Iadd, ty, lhs, IVal(0)), loc) =>
      copy(n, lhs, loc)

    case Let(n, Op.Bin(Iadd, ty, lhs, rhs), loc) if (lhs == rhs) =>
      Let(n, Op.Bin(Imul, ty, lhs, IVal(2, ty)), loc)

    /* Isub */
    case Let(n, Op.Bin(Isub, ty, lhs, IVal(0)), loc) =>
      copy(n, lhs, loc)

    case Let(n, Op.Bin(Isub, ty, lhs, IVal(i)), loc) if (i < 0) =>
      Let(n, Op.Bin(Iadd, ty, lhs, IVal(-i, ty)), loc)

    case Let(n, Op.Bin(Isub, ty, lhs, rhs), loc) if (lhs == rhs) =>
      copy(n, IVal(0, ty), loc)

    /* Imul */
    case Let(n, Op.Bin(Imul, ty, lhs, IVal(0)), loc) =>
      copy(n, IVal(0, ty), loc)

    case Let(n, Op.Bin(Imul, ty, lhs, IVal(1)), loc) =>
      copy(n, lhs, loc)

    case Let(n, Op.Bin(Imul, ty, lhs, IVal(-1)), loc) =>
      Let(n, Op.Bin(Isub, ty, IVal(0, ty), lhs), loc)

    case Let(n, Op.Bin(Imul, ty, lhs, PowerOf2(shift)), loc) =>
      Let(n, Op.Bin(Shl, ty, lhs, shift), loc)

    /* Sdiv */
    case Let(n, Op.Bin(Sdiv, ty, _, IVal(0)), loc) =>
      copy(n, Val.Undef(ty), loc)

    case Let(n, Op.Bin(Sdiv, ty, lhs, IVal(1)), loc) =>
      copy(n, lhs, loc)

    case Let(n, Op.Bin(Sdiv, ty, lhs, IVal(-1)), loc) =>
      Let(n, Op.Bin(Isub, ty, IVal(0, ty), lhs), loc)

    case Let(n, Op.Bin(Sdiv, ty, IVal(0), _), loc) =>
      copy(n, IVal(0, ty), loc)

    /* Udiv */
    case Let(n, Op.Bin(Udiv, ty, _, IVal(0)), loc) =>
      copy(n, Val.Undef(ty), loc)

    case Let(n, Op.Bin(Udiv, ty, lhs, IVal(1)), loc) =>
      copy(n, lhs, loc)

    case Let(n, Op.Bin(Udiv, ty, IVal(0), _), loc) =>
      copy(n, IVal(0, ty), loc)

    case Let(n, Op.Bin(Udiv, ty, lhs, PowerOf2(shift)), loc) =>
      Let(n, Op.Bin(Lshr, ty, lhs, shift), loc)

    /* Srem */
    case Let(n, Op.Bin(Srem, ty, lhs, IVal(0)), loc) =>
      copy(n, Val.Undef(ty), loc)

    case Let(n, Op.Bin(Srem, ty, lhs, IVal(1)), loc) =>
      copy(n, IVal(0, ty), loc)

    case Let(n, Op.Bin(Srem, ty, lhs, IVal(-1)), loc) =>
      copy(n, IVal(0, ty), loc)

    case Let(n, Op.Bin(Srem, ty, IVal(0), rhs), loc) =>
      copy(n, IVal(0, ty), loc)

    /* Urem */
    case Let(n, Op.Bin(Urem, ty, lhs, IVal(0)), loc) =>
      copy(n, Val.Undef(ty), loc)

    case Let(n, Op.Bin(Urem, ty, lhs, IVal(1)), loc) =>
      copy(n, IVal(0, ty), loc)

    case Let(n, Op.Bin(Urem, ty, IVal(0), rhs), loc) =>
      copy(n, IVal(0, ty), loc)

    /* Shl */
    case Let(n, Op.Bin(Shl, Type.Int, lhs, Val.Int(a)), loc) if ((a & 31) == 0) =>
      copy(n, lhs, loc)

    case Let(n, Op.Bin(Shl, Type.Long, lhs, Val.Long(a)), loc) if ((a & 63) == 0) =>
      copy(n, lhs, loc)

    case Let(n, Op.Bin(Shl, ty, IVal(0), _), loc) =>
      copy(n, IVal(0, ty), loc)

    /* Lshr */
    case Let(n, Op.Bin(Lshr, Type.Int, lhs, Val.Int(a)), loc) if ((a & 31) == 0) =>
      copy(n, lhs, loc)

    case Let(n, Op.Bin(Lshr, Type.Long, lhs, Val.Long(a)), loc) if ((a & 63) == 0) =>
      copy(n, lhs, loc)

    case Let(n, Op.Bin(Lshr, ty, IVal(0), rhs), loc) =>
      copy(n, IVal(0, ty), loc)

    /* Ashr */
    case Let(n, Op.Bin(Ashr, Type.Int, lhs, Val.Int(a)), loc) if ((a & 31) == 0) =>
      copy(n, lhs, loc)

    case Let(n, Op.Bin(Ashr, Type.Long, lhs, Val.Long(a)), loc) if ((a & 63) == 0) =>
      copy(n, lhs, loc)

    case Let(n, Op.Bin(Ashr, ty, IVal(0), rhs), loc) =>
      copy(n, IVal(0, ty), loc)

    case Let(n, Op.Bin(Ashr, ty, IVal(-1), rhs), loc) =>
      copy(n, IVal(-1, ty), loc)

    /* And */
    case Let(n, Op.Bin(And, ty, lhs, rhs), loc) if (lhs == rhs) =>
      copy(n, lhs, loc)

    case Let(n, Op.Bin(And, ty, lhs, IVal(0)), loc) =>
      copy(n, IVal(0, ty), loc)

    case Let(n, Op.Bin(And, ty, lhs, IVal(-1)), loc) =>
      copy(n, lhs, loc)

    /* Or */
    case Let(n, Op.Bin(Or, ty, lhs, rhs), loc) if (lhs == rhs) =>
      copy(n, lhs, loc)

    case Let(n, Op.Bin(Or, ty, lhs, IVal(0)), loc) =>
      copy(n, lhs, loc)

    case Let(n, Op.Bin(Or, ty, lhs, IVal(-1)), loc) =>
      copy(n, IVal(-1, ty), loc)

    /* Xor */
    case Let(n, Op.Bin(Xor, ty, lhs, rhs), loc) if (lhs == rhs) =>
      copy(n, IVal(0, ty), loc)

    case Let(n, Op.Bin(Xor, ty, lhs, IVal(0)), loc) =>
      copy(n, lhs, loc)

    /* Ieq */
    case Let(n, Op.Comp(Ieq, ty, lhs, rhs), loc) if (lhs == rhs) =>
      copy(n, Val.True, loc)

    case Let(n, Op.Comp(Ieq, ty, lhs, Val.True), loc) =>
      copy(n, lhs, loc)

    case Let(n, Op.Comp(Ieq, ty, lhs, Val.False), loc) =>
      neg(n, lhs, loc)

    /* Ine */
    case Let(n, Op.Comp(Ine, ty, lhs, rhs), loc) if (lhs == rhs) =>
      copy(n, Val.False, loc)

    case Let(n, Op.Comp(Ine, ty, lhs, Val.False), loc) =>
      copy(n, lhs, loc)

    case Let(n, Op.Comp(Ine, ty, lhs, Val.True), loc) =>
      neg(n, lhs, loc)

    /* Ugt */
    case Let(n, Op.Comp(Ugt, ty, lhs, IVal(a)), loc) if (a == umaxValue(ty)) =>
      copy(n, Val.False, loc)

    /* Uge */
    case Let(n, Op.Comp(Uge, ty, lhs, IVal(a)), loc) if (a == uminValue(ty)) =>
      copy(n, Val.True, loc)

    /* Ult */
    case Let(n, Op.Comp(Ult, ty, lhs, IVal(a)), loc) if (a == uminValue(ty)) =>
      copy(n, Val.False, loc)

    /* Ule */
    case Let(n, Op.Comp(Ule, ty, lhs, IVal(a)), loc) if (a == umaxValue(ty)) =>
      copy(n, Val.True, loc)

    /* Sgt */
    case Let(n, Op.Comp(Sgt, ty, lhs, IVal(a)), loc) if (a == maxValue(ty)) =>
      copy(n, Val.False, loc)

    /* Sge */
    case Let(n, Op.Comp(Sge, ty, lhs, IVal(a)), loc) if (a == minValue(ty)) =>
      copy(n, Val.True, loc)

    /* Slt */
    case Let(n, Op.Comp(Slt, ty, lhs, IVal(a)), loc) if (a == minValue(ty)) =>
      copy(n, Val.False, loc)

    /* Sle */
    case Let(n, Op.Comp(Sle, ty, lhs, IVal(a)), loc) if (a == maxValue(ty)) =>
      copy(n, Val.True, loc)

    /* Select */
    case Let(n, Op.Select(cond, thenv, elsev), loc) if (thenv == elsev) =>
      copy(n, thenv, loc)

    case Let(n, Op.Select(cond, Val.True, Val.False), loc) =>
      copy(n, cond, loc)

    case Let(n, Op.Select(cond, Val.False, Val.True), loc) =>
      neg(n, cond, loc)

    case _ =>
      inst
  }

  private def copy(n: Local, value: Val, loc: Location): Inst =
    Let(n, Op.Copy(value), loc)

  private def neg(n: Local, value: Val, loc: Location): Inst =
    Let(n, Op.Bin(Xor, Type.Bool, value, Val.True), loc)
}

object PartialEvaluation extends PassCompanion {
  override def apply(config: build.Config, top: Top) =
    new PartialEvaluation

  object PowerOf2 {
    def unapply(v: Val): Option[Val] = {
      v match {
        case Val.Byte(b) =>
          extractPow2(b).map(p => Val.Byte(p.toByte))

        case Val.Short(s) =>
          extractPow2(s).map(p => Val.Short(p.toShort))

        case Val.Int(i) =>
          extractPow2(i).map(p => Val.Int(p.toInt))

        case Val.Long(l) =>
          extractPow2(l).map(p => Val.Long(p.toLong))

        case _ => None
      }
    }

    def log2(x: Double): Double =
      math.log(x) / math.log(2)

    def extractPow2(x: Double): Option[Double] = {
      if (x < 1)
        None
      else {
        val log = log2(x)
        if (math.floor(log) == log)
          Some(log)
        else
          None
      }
    }
  }

  private def minValue(ty: Type): Long = ty match {
    case Type.Byte  => Byte.MinValue
    case Type.Short => Short.MinValue
    case Type.Int   => Int.MinValue
    case Type.Long  => Long.MinValue
  }

  private def maxValue(ty: Type): Long = ty match {
    case Type.Byte  => Byte.MaxValue
    case Type.Short => Short.MaxValue
    case Type.Int   => Int.MaxValue
    case Type.Long  => Long.MaxValue
  }

  private def uminValue(ty: Type): Long =
    0L

  private def umaxValue(ty: Type): Long =
    minValue(ty)

}
