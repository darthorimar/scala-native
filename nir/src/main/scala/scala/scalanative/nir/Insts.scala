package scala.scalanative
package nir

sealed abstract class Inst extends WithLocation {
  final def show: String = nir.Show(this)
}

object Inst {
  final case object None extends Inst {
    override def loc: Location = Location.None
  }
  final case class Label(name: Local, params: Seq[Val.Local], loc: Location) extends Inst
  final case class Let(name: Local, op: Op, loc: Location)                   extends Inst
  object Let {
    def apply(op: Op, loc: Location)(implicit fresh: Fresh): Let = Let(fresh(), op, loc)
  }

  sealed abstract class Cf                                  extends Inst
  final case class Unreachable(loc: Location)                              extends Cf
  final case class Ret(value: Val, loc: Location)                          extends Cf
  final case class Jump(next: Next, loc: Location)                         extends Cf
  final case class If(value: Val, thenp: Next, elsep: Next, loc: Location) extends Cf
  final case class Switch(value: Val, default: Next, cases: Seq[Next], loc: Location)
      extends Cf
  final case class Throw(value: Val, unwind: Next, loc: Location) extends Cf
}
